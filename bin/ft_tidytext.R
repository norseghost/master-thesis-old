# tidytext.R
# exploratory data analysis using the tidytext packages

library(tidyverse)
library(tidytext)
library(tm)
library(here)
library(topicmodels)
library(ldatuning)
library(tikzDevice)
library(future)
options(future.globals.maxSize = 9512896000)
options(tikzDefaultEngine = 'xetex')
options(tikzMetricsDictionary = here('/lib/tikzmetrics'))
options(tikzXelatexPackages =
        c(
          "\\usepackage{tikz}\n"
          ,"\\usepackage[active,tightpage,xetex]{preview}\n",
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


speeches <- read_csv(here("data/clean_no_stopwords_timeseries.csv"))

# Generate a set of Document-Term Matrices from the folketinget dataset
# input:
# - a folketinget tibble, modified to add a 'timeseries' field
#   (see bin/periods.R)
# TODO: incorporate periods code into this codefile
generate_dtms <- function(speeches) {
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

speeches <- readRDS(here("data/speeches_stopwords.rds"))

tidy <- speeches  %>%
    map(~ unnest_tokens(.x, lemma, text, token = "ngrams", n = 2))
saveRDS(tidy, here("data/bigrams_stopwords.rds"))

tidy <- readRDS(here("data/bigrams_stopwords.rds"))
tokens <- tidy %>%
  map(~ future(count(.x, lemma, doc_id, sort = TRUE))) %>%
  values
saveRDS(tokens, here("data/bigrams_tokens_stopwords.rds"))
tfidf <- tokens %>%
    map(~ future(bind_tf_idf(.x, lemma, doc_id, n))) %>%
    values
saveRDS(tfidf, here("data/bigrams_tfidf_stopwords.rds"))

tfidf <- readRDS(here("data/bigrams_tfidf_stopwords.rds"))

tfidf %>%
  map(~ summary(unique(.x$tf_idf, na.rm = TRUE)))

tfidf %>%
  map(~ summary(.x$tf_idf))



# tokens <- readRDS(here("data/tokens-trigrams-1990-01.rds")) %>%
#   count(lemma, doc_id, sort = TRUE)
# saveRDS(tokens, here("data/tokens_trigrams_count-1990-01.rds"))

# tokens <- readRDS(here("data/tokens-trigrams-2001-14.rds")) %>%
#   count(lemma, doc_id, sort = TRUE)
# saveRDS(tokens, here("data/tokens_trigrams_count-2001-14.rds"))
# tokens <- readRDS(here("data/tokens-trigrams-2014-20.rds")) %>%
#   count(lemma, doc_id, sort = TRUE)
# saveRDS(tokens, here("data/tokens_trigrams_count-2014-20.rds"))

read_tokens <- function(ngrams) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("tokens_", ngrams, "_count", ".*.rds")
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

imap(tokens, ~ create_tfidfs(tokens = .x, name = str_c("trigrams_", .y)))

tokens <- readRDS(here("data/tokens_bigrams.rds"))

tfidf %>%
    map(. %>%
        filter(tf_idf > mean(tf_idf, na.rm = TRUE))
    )

inspect_tfdidf <- function(tfidf) {
    tfidf %>% map(. %>%
        distinct(lemma, tf_idf) %>%
        arrange(lemma, -tf_idf) %>%
        top_n(-30) %>%
        print(n = 30))
}  


    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    map(~ filter(.x, tf_idf < quantile(tf_idf, 0.998)))

tfidf_filter <- tfidf %>%
    map(~ future(filter(.x, tf_idf > mean(tf_idf, na.rm = TRUE)))) %>%
    values
dtm <- tfidf_filter %>%
    map(~ future(cast_dtm(.x, doc_id, lemma, n))) %>%
    values



saveRDS(dtm, here("data/dtm_periods_stopwords_bigrams"))

dtm <- readRDS(here("data/dtm_periods_stopwords_bigrams"))

dtm <- dtm %>%
  map(~ future(removeSparseTerms(.x, 0.998))) %>%
  values %>%
  map(~ future(.x[unique(.x$i), ])) %>%
  values

saveRDS(dtm_nosparse, here("data/dtm_periods_stopwords_nosparse_bigrams"))

dtm <- readRDS(here("data/dtm_periods_stopwords_nosparse_bigrams")) %>%
  map(~ .x[unique(.x$i), ])

lda <- dtm %>%
    map(~ future(LDA(.x, method = "Gibbs", k = 35, control = control))) %>%
    values

saveRDS(lda, here("data/lda-35_periods_bigrams"))

docs <- lda %>%
    map(~ future(tidy(.x, matrix = "gamma"))) %>%
    values


topics <- lda %>%
    map(~ future(tidy(.x, matrix = "beta"))) %>%
    values

models_compare <- function(dtm, name, max_k, steps, cores) {
  dtm  <- dtm[unique(dtm$i), ]
  models <- FindTopicsNumber(
      dtm,
      topics = seq(from = steps, to = max_k, by = steps),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = control,
      verbose = TRUE,
      return_models = TRUE,
      if(!missing(cores)) {
        mc.cores = cores
      }
    )
  saveRDS(models, here(str_c("data/models", name, "_", max_k, "by", steps, ".rds")))
  rm(models)
  gc()
  return(NULL)
}

imap(dtm, ~ models_compare(
                    dtm = .x,
                    name = .y,
                    max_k = 75,
                    steps = 5,
                    cores = 16L))

read_models <- function(max_k, steps) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("*_", max_k, "by", steps, ".rds"
              )
  )
  models <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(models) <- filenames %>%
    map(~ str_match(.x, pattern = "models(.*)_\\d+by\\d+.rds")[,2])
  return(models)
}

keep_model <- function(models, k) {
  models %>%
    map(. %>%
    filter(topics == 25) %>%
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


assignments <- map2(lda, dtm, augment, .x, .y)

saveRDS(assignments, here("data/assignments_bigrams_k35.rds"))


# TODO: I'm writing a shitty and probably slow reimplementatin of map
#       This is probably ill advised, but seems faster than getting map to work
top_terms <- vector("list", length(periods))
names(top_terms) <- periods
for (i in seq_along(periods)) {
top_terms[[i]] <- topics[[i]] %>%
        group_by(topic) %>%
        top_n(15, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
}

top_terms <- topics %>%
  modify(. %>%
        group_by(topic) %>%
        top_n(15, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
  )

term_plots  <- vector("list", length(top_terms))
names(term_plots) <- names(top_terms)
for (i in seq_along(top_terms)) {
term_plots[[i]] <- top_terms[[i]] %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free", ncol = 5) +
        coord_flip() +
        scale_x_reordered()
}

saveRDS(term_plots, here("data/plots-bigrams-k35.rds"))

paths <- str_c(names(term_plots), "-topicnumbers.pdf")
pwalk(list(paths, term_plots), ggsave, path = here("fig"), width = 20, height = 40)

term_plots <- top_terms %>%
  modify(. %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
  )


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
imap(json, ~ serVis(json = .x, out.dir = str_c(here("vis/", .y)), open.browser = FALSE))



# topics pertainig to education, as determined by visual inspection
#TODO: named list instead?
edu_topic_numbers <- c(6, 16, 7, 3, 9)

# construct list for 
edu_docs<- vector("list", length(docs))

l <- list(topicnum = edu_topic_numbers, edu_docs = edu_docs, docs = docs)

edu_docs <- pmap(l, function(edu_docs, docs, topicnum) { edu_docs <- filter(docs, topic == topicnum)}) %>%
# all docs have a probability of beaing assigned to a topic
# we only want the 1%
  map(~ filter(.x, gamma > quantile(gamma, 0.97))
names(edu_docs) <- names(docs)

edu_tokens <- map2(tokens, edu_docs, ~ filter(.x, doc_id %in% .y$document))

edu_tfidf <- edu_tokens %>%
    map(~ future(bind_tf_idf(.x, lemma, doc_id, n))) %>%
    values %>%
    # filter out terms that are below the median tf_idf score per corpus
    # doi://10.18637/jss.v040.i13 for rationale,
    map(~ filter(.x, tf_idf > median(tf_idf, na.rm = TRUE))) %>%
    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    map(~ filter(.x, tf_idf < quantile(tf_idf, 0.998)))
edu_dtm <- edu_tfidf %>%
    map(~ future((cast_dtm(.x, doc_id, lemma, n)))) %>%
    values

edu_lda <- edu_dtm %>%
    map(~ future(LDA(.x, method = "Gibbs", k = 15, control = list(seed = 1234, burnin = 500, thin = 300, iter = 3000)))) %>%
    values
edu_json <- map2(edu_lda, edu_dtm, topicmodels_json_ldavis)
imap(json, ~ serVis(json = .x, out.dir = str_c(here("vis/edu/", .y)), open.browser = FALSE))


edu_models <- edu_dtm %>%
    map(~ FindTopicsNumber(
      .x,
      topics = seq(from = 5, to = 60, by = 5),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 1234),
      verbose = TRUE,
      mc.cores = 8
    ))

