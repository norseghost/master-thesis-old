# tidytext.R
# exploratory data analysis using the tidytext packages

library(data.table)
library(tidyverse)
library(tidytext)
library(tm)
library(here)
library(topicmodels)
library(ldatuning)
library(tikzDevice)
library(udpipe)
library(future)
options(future.globals.maxSize = 9512896000)
options(tikzDefaultEngine = "xetex")
options(tikzMetricsDictionary = here("/lib/tikzmetrics"))
options(tikzXelatexPackages =
        c(
          "\\usepackage{tikz}\n",
          "\\usepackage[active,tightpage,xetex]{preview}\n",
          "\\usepackage{fontspec,xunicode}\n",
          "\\PreviewEnvironment{pgfpicture}\n",
          "\\setlength\\PreviewBorder{0pt}\n")
        )
plan(multicore)
seed <- 1234
burnin <- 500
thin <- 300
iter <- 3000
n <- 2
sparse_treshold <- 0.998
control <- list(
            seed = seed,
            burnin = burnin,
            thin = thin,
            iter = iter)

### TEXT PREPROCESSING BLOCK
# TODO: Rewrite using tidyverse/future packages?
speeches <- fread(here("data/folketinget_1953_2019_raw.csv"))

# prepare udpipe for lemmatization
udmodel <- udpipe_download_model(
    language = "danish",
    model_dir = here("lib"),
    overwrite = FALSE
)
# lemmatize text for future tokenization
lemmatize <- function(text) {
    dt <- udpipe_annotate(
        object = udpipe_load_model(udmodel$file_model),
        x = text,
        # tokenizer = "horizontal",
        tagger = "default",
        parser = "none",
        trace = 0
    ) %>% as.data.table
    lemmata <- dt %>%
        select(lemma) %>%
        unlist %>%
        str_c(collapse = " ")
    return(lemmata)
}
# wrapper function to perform text cleanup steps
clean_text <- function(text) {
    text %>%
    tolower %>%
    removeNumbers %>%
    removePunctuation %>%
    removeWords(ft_stopwords) %>%
    lemmatize %>%
    # the corpus contains occurences of hangul character
    # hwalp - 홢 - this is unwanted
    str_remove_all(., "홢") %>%
    stripWhitespace
}
# set up parallel processing
library(parallel)
cluster <- makePSOCKcluster(
                names = 8
        )
# the cluster needs to see my stopwords
clusterExport(
        cl = cluster,
        varlist = c("ft_stopwords", "lemmatize", "clean_text", "udmodel"),
        envir = .GlobalEnv
)
#and needs to have the required libraries loaded
clusterEvalQ(
    cl = cluster, {
        library(tm)
        library(tidyverse)
        library(udpipe)
        library(data.table)
    }
)
# apply the cleaning operation in parallel
# the dataset is already prepared in 100 batches
speeches <- speeches[, text := parSapply(
                            cluster, .SD[, text], clean_text),
            by = .groups]

### CORPORA SPLITTING
# group speeches according to 
# - parliamentary periods
# - as maps to generalized trends in
#   Danish educational policy

# Periods:
# 53-57 (ny lærlingelov,vekseluddannelse)
# 57-68 (udvidelse af lærlingekonceptet)
# 68-78 (EFG)
# 78-90 (Haarder, U91, EUD, EUX)
# 90-01 (uddannelse til alle, markedsorientering, selvstyre)
# 01-14 (individualisering, ansvar for egen læring)
# 14-20 (fokus på unge; voksne falder fra)
speeches[, timeseries := future_sapply(Date, (function(x) {
                            case_when(
                                 x < as.Date("1957-05-28") ~ "1953-57",
                                 x < as.Date("1968-02-02") ~ "1957-68",
                                 x < as.Date("1978-08-30") ~ "1968-78",
                                 x < as.Date("1990-12-18") ~ "1978-90",
                                 x < as.Date("2001-11-27") ~ "1990-01",
                                 x < as.Date("2014-02-03") ~ "2001-14",
                                 x > as.Date("2014-02-03") ~ "2014-20"
                            )}
                        ))]

# Generate a set of Document-Term Matrices from the folketinget dataset
# input:
# - a folketinget tibble, modified to add a 'timeseries' field
#   (see bin/periods.R)
# TODO: rewrite to just call serializer functions below
pipeline <- function(speeches, n, sparse_threshold) {
  # there are errors in the folketinget dataset that makes documents
  # pre 1978 ish suspect -- remove these
  speeches <- speeches %>%
    filter(timeseries != "1957-68") %>%
    filter(timeseries != "1968-78")
  # TODO: this is basically a group_by, right?
  periods <- speeches %>%
    distinct(timeseries) %>%
    pull
  # TODO: And I think this is a terribad implementation of split()
  corpora <- vector("list", length(periods))
  names(corpora) <- periods
  for (i in seq_along(periods)) {
      corpora[[i]] <- speeches %>%
          filter(timeseries == periods[[i]])
  }
  # add a 'all' dtm too, as a control
  corpora[["all"]] <- speeches %>%
      filter(timeseries %in% periods)
  tfidf <- corpora %>%
    # unnest tokens using tidytext
    # TODO: add centralized config for n, token etc
    map(~ unnest_tokens(.x, lemma, text, token = "ngrams", n = 2)) %>%
    # add token fields
    map(~ future(count(.x, lemma, doc_id, sort = TRUE))) %>%
    values %>%
    # generate a tf_idf
    map(~ future(bind_tf_idf(.x, lemma, doc_id, n))) %>%
    values
  dtms <- tfidf %>%
    # the filter settings here are derived from inspecting the raw tf_idf
    # the cutoff is derived from the mean of all unique tf_idf scores
    # to raise the floor quite a bit
    map(~ future(filter(.x, tf_idf > mean(unique(tf_idf), na.rm = TRUE)))) %>%
    values %>%
    map(~ future(cast_dtm(.x, doc_id, lemma, n))) %>%
    values %>%
    # also these
    map(~ future(removeSparseTerms(.x, 0.9999))) %>%
    values %>%
    # the removal of sparse terms creates empty rows
    # this won't do
    # only empty rows will be non-unique
    # so ditch them (preserving row order)
    dtm[unique(dtm$i), ]
  return(list(tfidf, dtms))
}

periods <- speeches %>%
    filter(timeseries != "1957-68") %>%
    filter(timeseries != "1968-78") %>%
    distinct(timeseries) %>%
    pull
speeches <- vector("list", length(periods))
names(speeches) <- periods
for (i in seq_along(periods)) {
    speeches[[i]] <- speeches %>%
        filter(timeseries == periods[[i]])
}
speeches[["all"]] <- speeches %>%
    filter(timeseries %in% periods)
saveRDS(speeches, here("data/speeches_stopwords.rds"))

# The size of the corpora makes linear processing
# of all of them RAM intensive
# helper functions to serialize to/from disk,
# with consistent naming
#
# [tidy|tokens|tfidf|dtm|models]_{ngrams}_{identifier}_{corpus}.rds

read_tokens <- function(ngrams) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("tokens_", ngrams, ".*.rds")
  )
  print(filenames)
  tokens <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(tokens) <- filenames %>%
    map(~ str_match(.x, pattern = ".*-(\\d+-\\d+).rds")[,2])
  return(tokens)
}

collate_tokens <- function(tokens) {
  all_tokens <- tokens %>%
    map(~ bind_cols(.x))
}

create_tfidfs <- function(tokens, name) {
  tfidf <- bind_tf_idf(tokens, lemma, doc_id, n)
  saveRDS(tfidf, here(str_c("data/tfidf_", name, ".rds")))
  rm(tfidf)
  gc()
  return(NULL)
}
read_tfidfs <- function(ngrams) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("tfidf_", ngrams, ".*.rds")
  )
  tfidfs <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(tfidfs) <- filenames %>%
    map(~ str_match(.x, pattern = ".*_(\\d+-\\d+).rds")[, 2])
  return(tfidfs)
}

imap(tokens, ~ create_tfidfs(tokens = .x, name = str_c("trigrams_", .y)))

inspect_tfidf <- function(tfidf) {
  print("Summary over unique tf_idf values")
  tfidf %>%
    select(tf_idf) %>%
    unique(na.rm = TRUE) %>%
    summary %>%
    print
  print("Summary over all tf_idf values")
  tfidf %>%
    select(tf_idf) %>%
    summary %>%
    print
  print("30 most common terms")
  tfidf %>%
        distinct(lemma, tf_idf) %>%
        arrange(lemma, -tf_idf) %>%
        top_n(-30) %>%
        print(n = 30)
  print("30 least common terms")
  tfidf %>%
        distinct(lemma, tf_idf) %>%
        arrange(lemma, -tf_idf) %>%
        top_n(30) %>%
        print(n = 30)
}

filter_tfidf <- function(tfidf) {
  tfidf %>%
    # cut off terms with a tfidf value under the mean
    # of all tfidf values
    # to eliminate very common terms
    filter(tf_idf > mean(tf_idf, na.rm = TRUE)) %>%
    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    filter(tf_idf < quantile(tf_idf, 0.998))
}

generate_dtms <- function(tfidf) {
    cast_dtm(tfidf, doc_id, lemma, n)
}


topics <- lda %>%
    map(~ future(tidy(.x, matrix = "beta"))) %>%
    values

models_compare <- function(dtm, name, min_k, max_k, steps, cores) {
  dtm  <- dtm[unique(dtm$i), ]
  if(missing(min_k)) {
    min_k <- steps
  }
  models <- FindTopicsNumber(
      dtm,
      topics = seq(from = min_k, to = max_k, by = steps),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = control,
      verbose = TRUE,
      return_models = TRUE,
      if(!missing(cores)) {
        mc.cores = cores
      }
    )
  saveRDS(models, here(str_c(
            "data/models_", name, "_", min_k,
            "to", max_k, "by", steps, ".rds")))
  rm(models)
  gc()
  return(NULL)
}

imap(dtms, ~ models_compare(
                    dtm = .x,
                    name = str_c("bigrams_stopwords_quantile", .y),
                    min_k = 5,
                    max_k = 75,
                    steps = 10,
                    cores = 4L))

read_models <- function(ngrams, max_k, steps) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("models_", ngrams, ".*_", max_k, "by", steps, ".rds"
              )
  )
  models <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(models) <- filenames %>%
    map(~ str_match(.x, pattern = str_c("models_", ngrams, "_", "(.*)_\\d+by\\d+.rds"))[,2])
  return(models)
}

get_ldamodels <- function(models, k) {
  models %>%
    map(. %>%
    filter(topics == k) %>%
    select(LDA_model) %>%
    unlist(use.names = FALSE))
}

map

models <- dtm_nosparse %>%
    map(~ future(.x[unique(.x$i), ])) %>%
    values %>%
    map(~ FindTopicsNumber(
      .x,
      topics = seq(from = 2, to = 204, by = 5),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = control,
      verbose = TRUE,
      mc.cores = 8L,
      return.models = TRUE
    ))

models <- readRDS(here("data/models_120.rds"))

plot_models <- function(models) {
    map(models, ~ normalize_topic_numbers(.x)) %>%
    bind_rows(.id = "period") %>%
    reshape2::melt(., id.vars = c("topics", "period"), na.rm = TRUE) %>%
    plot_topic_numbers %>%
    ggsave(
           filename="edu_models_plot.pdf",
           width=15,
           height=15, 
           units = "cm",
           path = here("fig")
      )
}

normalize_topic_numbers <- function(values) {
  # Drop models if present, as they won't rescale
  # Also, Deveaud is not useful for this dataset
  values <- values %>% 
     select(-LDA_model, -Deveaud2014)
  # normalize to [0,1]
  columns <- values %>%
    select(-topics) %>%
    modify(~ scales::rescale(.x, to = c(0, 1), from = range(.x)))
  invert <- columns %>%
    select(Griffiths2004) %>%
    modify(~ (1.0 - .x))
  columns <- columns %>%
    select(-Griffiths2004) %>%
    bind_cols(invert)
  values <- values %>%
    select(topics) %>%
    bind_cols(columns)
}

plot_topic_numbers <- function(values) {
  # standard plot-
  p <- ggplot(values, aes_string(x = "topics", y = "value", group = "variable"))
  p <- p + facet_grid(period ~ .)
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"), size = 3)
  p <- p + guides(size = FALSE, shape = guide_legend(title = "Målefunktion"))
  p <- p + facet_grid(period ~ .)
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "Antal emner", y = NULL)
  p <- p + theme_bw() %+replace% theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey70"),
    panel.grid.minor.x = element_blank(),
    legend.key = element_blank(),
    strip.text.y = element_text(angle = 90)
  )
  # move strip block to left side
  g <- ggplotGrob(p)
  g$layout[g$layout$name == "strip-right", c("l", "r")] <- 3
  grid::grid.newpage()
  grid::grid.draw(g)
  return(p)
}

plot_models <- function(models, name = "models") {
    map(models, ~ normalize_topic_numbers(.x)) %>%
    bind_rows(.id = "period") %>%
    reshape2::melt(., id.vars = c("topics", "period"), na.rm = TRUE) %>%
    plot_topic_numbers %>%
    ggsave(
           filename = str_c(name, ".tex"),
           width = 4,
           height = 3,
           path = here("fig"),
           device = tikz,
           standAlone = FALSE
    )
}

topicmodels_json_ldavis <- function(model, dtm){
  require(LDAvis)
  require(slam)
  library(parallel)
  cluster <- makePSOCKcluster(
                names = 12
        )
  # Find required quantities
  phi <- as.matrix(posterior(model)$terms)
  theta <- as.matrix(posterior(model)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(dtm)
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(dtm$i)),
                                 term.frequency = term_freq,
                                 cluster = cluster)
  return(json_lda)
}

json <- map2(k25, dtm, topicmodels_json_ldavis)
imap(json, ~ serVis(
                json = .x, 
                out.dir = str_c(here("vis/", .y)),
                open.browser = FALSE))

assignments <- map2(lda, dtm, augment, .x, .y)


lda_to_docs <- function(lda) {
    tidy(lda, matrix = "gamma")
}

lda_to_topics <- function(lda) {
    tidy(lda, matrix = "beta")
}

topics <- map(ldas, ~ lda_to_topics(.x))

get_top_terms <- function(topics){
  topics %>%
    group_by(topic) %>%
    top_n(15, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
}

top_terms <- map(topics, ~ get_top_terms(.x))

plot_terms <- function(term_list) {
  term_list %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free", ncol = 3) +
      coord_flip() +
      scale_x_reordered()
}

term_plots <- map(top_terms, ~ plot_terms(.x))

paths <- str_c(names(term_plots), "-topicnumbers.tex")
pwalk(list(paths, term_plots), ggsave, path = here("fig"), width = 20, height = 40, device = tikz, standAlone = FALSE)


# topics pertainig to education, as determined by visual inspection
#TODO: named list instead?
edu_topic_numbers <- c(6, 16, 7, 3, 9)
ldas <- readRDS(here("data/lda-35_periods_bigrams"))
dtms <- readRDS(here("data/dtm_bigrams"))
docs <- map(ldas, ~ lda_to_docs(.x))
corpora <- readRDS(here("data/tokens_bigrams.rds"))
# construct list for 
edu_docs <- vector("list", length(docs))

l <- list(topicnum = edu_topic_numbers, edu_docs = edu_docs, docs = docs)
edu_docs <- pmap(l, function(edu_docs, docs, topicnum) { edu_docs <- filter(docs, topic == topicnum)}) %>%
# all docs have a probability of beaing assigned to a topic
# we only want the 3%
  map(~ filter(.x, gamma > quantile(gamma, 0.97)))
names(edu_docs) <- names(docs)

get_edu_corpora <- function(corpus, doc_ids) {
  corpus %>%
  filter(doc_id %in% doc_ids)
}

edu_corpora <- map2(corpora, edu_docs, ~                   
                    get_edu_corpora(corpus = .x,
                                    doc_ids = .y$document))

edu_tfidfs <- map(edu_corpora, ~  bind_tf_idf(.x, lemma, doc_id, n))

edu_dtms <- map(edu_tfidfs, ~ generate_dtms(.x)) 
