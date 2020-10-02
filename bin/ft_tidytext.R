# tidytext.R
# exploratory data analysis using the tidytext packages

library(tm)
library(here)
library(textmineR)
library(topicmodels)
library(ldatuning)
library(tikzDevice)
library(udpipe)
library(austin)
library(cowplot)
library(future)
library(ggrepel)
library(ggdendro)
library(dendextend)
library(grid)
library(gridExtra)
library(magick)
library(png)
library(xtable)
library(plotly)
library(Sentida)
library(data.table)
library(tidyverse)
library(tidytext)

options(future.globals.maxSize = 9512896000)
options(tikzDefaultEngine = "luatex")
options(tikzMetricsDictionary = here("/lib/tikzmetrics"))
options(tikzLualatexPackages =
        c(
          "\\usepackage{tikz}\n",
          "\\usepackage[active,tightpage,psfixbb]{preview}\n",
          "\\usepackage{fontspec}\n",
          "\\PreviewEnvironment{pgfpicture}\n",
          "\\setlength\\PreviewBorder{0pt}\n"
        )
)
theme_minimal()
plan(multicore)
seed <- 1234
# burnin <- 500
# thin <- 300
# iter <- 3000
n <- 2
#global ID for for this round of processing
identifier <- "bigrams_no_stopwords"
treshold <- 0.998
min_k <- 5
max_k <- 125
steps <- 10
k <- 35
n <- 2
control <- list(
                seed = seed
                # burnin = burnin,
                # thin = thin,
                # iter = iter
)
# blocs and parties to use in analysis
blocs <- c(# don't use the greenland/faroese parties
           # they add too much noise
           "Rød Blok",
           "Centrum",
           "Blå Blok",
           "ikke angivet")
parties <- c(# similarly, the smaller parties 
             # 1) add noise to the data; pre-analysis
             # 2) too many parties p
             "Enhedslisten",
             "Socialdemokratiet",
             "Socialistisk Folkeparti",
             "Dansk Folkeparti",
             "Venstre",
             "Konservative Folkeparti",
             "Radikale Venstre",
             "Liberal Alliance",
             "Ny Alliance",
             "Alternativet",
             "Fremskridspartiet"
             )

preprocess <- function(raw_speeches) {
  raw_speeches %>%
    clean_corpus %>%
    group_corpora %>%
    split_corpora
}

pipeline <- function(proc_speeches) {
  imap(proc_speeches, ~ write_ngrams(
    corpus = .x,
    n = n,
    name = identifier,
    period = .y))
  tokens <- read_ngrams(name = identifier)
  imap(tokens, ~ write_tfidfs(
      tokens = .x,
      name = identifier,
      period = .y))
  tfidfs <- read_tfidfs(name = identifier)
  dtms <- tfidfs %>%
    map(~ future(filter_tfidf(.x, treshold))) %>%
    values %>%
    map(~ future(generate_dtms(.x))) %>%
    values
  saveRDS(dtms, here(str_c("data/dtms_", identifier, ".rds")))
  imap(dtms, ~ models_compare(
                    dtm = .x,
                    period = .y,
                    name = identifier,
                    min_k = min_k,
                    max_k = max_k,
                    steps = steps,
                    cores = 12L))
  models <- read_models(
                    name = identifier,
                    min_k = min_k,
                    max_k = max_k,
                    steps = steps)
  plot_models(
            models = models,
            name = identifier,
            min_k = min_k,
            max_k = max_k,
            steps = steps)
  return(list(tfidfs = tfidfs, dtms = dtms, models = models))
}

### TEXT PREPROCESSING BLOCK
# TODO: Rewrite using tidyverse/future packages?
speeches <- fread(here("data/folketinget_1953_2019_raw.csv"))

# prepare udpipe for lemmatization
udmodel <- udpipe_download_model(
    language = "danish",
    model_dir = here("lib"),
    overwrite = FALSE
)
# I'll maybe want more stopwords later
# after some exploratory analysis
# but wait! Stopwords counter-indicated for certain analysis
stopwords <- c(
    readLines(here("lib/stopwords.txt")),
    readLines(
        "https://raw.githubusercontent.com/stopwords-iso/stopwords-da/master/stopwords-da.txt",
        warn = FALSE)
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
clean_text <- function(text, use_stopwords = FALSE) {
    text %>%
    tolower %>%
    removeNumbers %>%
    removePunctuation %>%
    execute_if(use_stopwords,
      removeWords(stopwords)) %>%
    lemmatize %>%
    # the corpus contains occurences of hangul character
    # hwalp - 홢 - this is unwanted
    str_remove_all(., "홢") %>%
    stripWhitespace
}
clean_corpus <- function(folketinget) {
  # this operation is eminently parallelizable
  # wrap the wrapper to do so
  library(parallel)
  cluster <- makePSOCKcluster(
                  names = 8
          )
  # the cluster needs to see my stopwords
  clusterExport(
          cl = cluster,
          varlist = c("stopwords", "lemmatize", "clean_text", "udmodel"),
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
  folketinget[, text := parSapply(
                              cluster, .SD[, text], clean_text),
              by = .groups]
}

group_corpora  <- function(folketinget) {
  # group speeches according to 
  # - parliamentary periods
  # - as maps to generalized trends in
  #   Danish educational policy
  folketinget <- as.data.table(folketinget)
  folketinget[, timeseries := sapply(Date, (function(x) {
    case_when(
      # 53-57 (ny lærlingelov,vekseluddannelse)
      x < as.Date("1957-05-28") ~ "1953-57",
      # 57-68 (udvidelse af lærlingekonceptet)
      x < as.Date("1968-02-02") ~ "1957-68",
      # 68-78 (EFG)
      x < as.Date("1978-08-30") ~ "1968-78",
      # 78-90 (Haarder, U91, EUD, EUX)
      x < as.Date("1990-12-18") ~ "1978-90",
      # 90-01 (uddannelse til alle, markedsorientering, selvstyre)
      x < as.Date("2001-11-27") ~ "1990-01",
      # 01-14 (individualisering, ansvar for egen læring)
      x < as.Date("2014-02-03") ~ "2001-14",
      # 14-20 (fokus på unge; voksne falder fra)
      x > as.Date("2014-02-03") ~ "2014-20"
    )}
  ))]
}

split_corpora <- function(ft_grp) {
  # FIXME: I can get the same effect with group_by in the functions
  #        below; which would even let me parameterize and use the
  #        same code if I wanted a different basis for generating
  #        subcorpora.
  ft_grp <- ft_grp %>%
  # there are errors in the folketinget dataset that makes documents
  # pre 1 "bigrams_no_stopwords_extra_filtered"978 ish suspect -- remove these
    filter(!timeseries %in% c("1957-68", "1968-78"))
  ft_split <- ft_grp %>%
    group_by(timeseries)
  ft_names <- ft_split %>%
    group_keys
  ft_split <- ft_split %>%
    group_split(keep = FALSE) %>%
    # this is rather cryptic, but it returns the first column of the
    # grouping variable - the periods outlined above
    setNames(ft_names[[1]])
  ft_split[["all"]] <- ft_grp
  return(ft_split)
 }


# The size of the corpora makes linear processing
# of all of them RAM intensive
# helper functions to serialize to/from disk,
# with consistent naming
#
# TODO: this seems like a place where functionals might be useful
#
# [tokens|tfidf|dtm|models]_{ngrams}_{identifier}_{corpus}.rds

write_ngrams <- function(corpus, ngrams, name, period) {
  cat(str_c("writing tokens\n"))
  tokens <- unnest_tokens(corpus, lemma, text, token = "ngrams", n = ngrams)
  counted <- count(tokens, lemma, doc_id, sort = TRUE)
  saveRDS(counted, here(str_c("data/tokens_", name, "_n", n, "_", period, ".rds")))
  rm(tokens, counted)
  gc()
  return(NULL)
}
read_ngrams <- function(name) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("tokens_", name, "_n", n, ".*.rds")
  )
  print(filenames)
  tokens <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(tokens) <- filenames %>%
    map(~ str_match(.x, pattern = ".*_(\\d+-\\d+).rds")[,2])
  return(tokens)
}

# WIP - some sets of ngrams are too big far all tokens at once
collate_tokens <- function(tokens) {
  tokens %>%
    bind_rows
}

write_tfidfs <- function(tokens, name, period) {
# edu_collated <- map(edu_block, ~(create_collated_corpora, "Blok"))
  cat(str_c("writing tfidfs\n"))
  tfidf <- bind_tf_idf(tokens, lemma, doc_id, n)
  saveRDS(tfidf, str_c("data/tfidf_", name, "_", period, ".rds"))
}

read_tfidfs <- function(name) {
  cat(str_c("reading tfidfs\n"))
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("tfidf_", name, ".*.rds")
  )
  print(filenames)
  tfidfs <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(tfidfs) <- filenames %>%
    map(~ str_match(.x, pattern = ".*_(\\d+-\\d+|all).rds")[, 2])
  return(tfidfs)
}

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
}

filter_tfidf <- function(tfidf, treshold) {
  tfidf %>%
    # cut off terms with a tfidf value under the mean
    # of all tfidf values
    # to eliminate very common terms
    filter(tf_idf > mean(tf_idf, na.rm = TRUE)) %>%
    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    filter(tf_idf < quantile(tf_idf, treshold))
}

generate_dtms <- function(tfidf) {
  cat(str_c("Generating dtm...\n"))
    cast_dtm(tfidf, doc_id, lemma, n)
}

filter_dtm <- function(dtm) {
  dtm %>%
    # this removes terms that do not occur in the upper
    # two per mille of documents
    # NOTE: given the particularities of this dataset, this may
    #       reduce complexity overmuch
    removeSparseTerms(0.99999) %>%
    .[unique(.$i), ]
}

models_compare <- function(dtm, name, period, min_k, max_k, steps, cores) {
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
            "data/models_", name, "_", period, "_", min_k,
            "to", max_k, "by", steps, ".rds")))
  rm(models)
  gc()
  return(NULL)
}

read_models <- function(name, min_k, max_k, steps) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("models_", name, "_(\\d+|all).*_", min_k, "to",  max_k, "by", steps, ".rds"
              )
  )
  cat("found models:\n")
  map(filenames, ~ cat(str_c(.x, "\n")))
  models <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(models) <- filenames %>%
    map(~ str_match(.x, 
          pattern = str_c("models_", name, "_", "(.*)_\\d+to\\d+by\\d+.rds"))[, 2])
  return(models)
}

get_ldamodels <- function(models, k) {
  models %>%
    map(. %>%
    filter(topics == k) %>%
    select(LDA_model) %>%
    unlist(use.names = FALSE))
}

normalize_topic_numbers <- function(values) {
  # Drop models if present, as they won't rescale
  # Also, Deveaud is not useful for this dataset
  values <- values %>% 
     select(-LDA_model)
  # normalize to [0,1]
  columns <- values %>%
    select(-topics) %>%
    modify(~ scales::rescale(.x, to = c(0, 1), from = range(.x)))
  invert <- columns %>%
    select(Griffiths2004, Deveaud2014) %>%
    modify(~ (1.0 - .x))
  columns <- columns %>%
    select(-Griffiths2004, -Deveaud2014) %>%
    bind_cols(invert)
  values <- values %>%
    select(topics) %>%
    bind_cols(columns)
}

plot_topic_numbers <- function(values) {
  # standard plot-
  values %>% ggplot(aes(x = topics, y = value, group = variable)) +
    facet_grid(period ~ .) +
    geom_line(linetype = "dashed") +
    geom_point(aes(shape = variable), size = 3) +
    scale_x_continuous(breaks = values$topics) +
    guides(shape = guide_legend(title="Målefunktion")) +
    labs(x = "Antal emner", y = NULL) +
    theme_bw() %+replace% theme(
          legend.key = element_blank(),
          legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey70"),
          panel.grid.minor.x = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank(),
          strip.text.y = element_text(angle = 90)
    )
}

plot_models <- function(models, name, period, min_k, max_k, steps) {
    map(models, ~ normalize_topic_numbers(.x)) %>%
    bind_rows(.id = "period") %>%
    pivot_longer(!c(period, topics), names_to = "variable", values_to = "value") %>%
    plot_topic_numbers %>%
    ggsave(
           filename = str_c("models_", name, "_", min_k,
            "to", max_k, "by", steps,  ".tex"),
           width = 5,
           height = 8,
           path = here("fig"),
           device = tikz,
           standAlone = FALSE
    )
}

topicmodels_json_ldavis <- function(model, dtm, name, k, period){
  require(LDAvis)
  require(slam)
  library(parallel)
  cluster <- makePSOCKcluster(
                names = 8
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
                                 cluster = cluster,
                                 reorder.topics = TRUE)
  saveRDS(json_lda, here(str_c("data/json_", name, "_k", k, "_", period, ".rds")))
}

l <- list(model = ldas, dtm = dtms, period = names(ldas), name = identifier, k = k)
pmap(l, ~
     topicmodels_json_ldavis(
              model = ..1,
              dtm = ..2,
              period = ..3,
              name = ..4,
              k = ..5
     ))
read_json <- function(name, k) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("json_", name, "_k", k, "_", ".*.rds"
              )
  )
  jsons <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(jsons) <- filenames %>%
    map_chr(~ str_match(.x, pattern = str_c("json_", name, "_k", k,  "_", "(.*).rds"))[,2]) 
  return(jsons)
}
jsons <- read_json(name = identifier, k = k)

# FIXME: How do I make a directory inside the function?
imap(jsons, ~ serVis(
                json = .x, 
                out.dir = here(str_c("vis/bigrams_tfidfs_more_filtered/", .y)),
                open.browser = FALSE))

assignments <- map2(ldas, dtms, augment, .x, .y)


lda_to_docs <- function(lda) {
    tidy(lda, matrix = "gamma")
}

lda_to_topics <- function(lda) {
    tidy(lda, matrix = "beta")
}

topic_list <- map(ldas, ~ lda_to_topics(.x))

get_top_terms <- function(topics, number) {
  topics %>%
    group_by(topic) %>%
    top_n(number, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
}

doc_list <- map(ldas, ~ lda_to_docs(.x))

# WIP: get top documents per topic
#
get_n_docs_pr_topic <- function(docs, number) {
  docs %>%
    group_by(topic) %>%
    top_n(number, gamma) %>%
    ungroup() %>%
    arrange(topic, document)
}

get_docs_pr_topic <- function(docs, frac) {
  docs %>%
    filter(gamma > frac)
}

top_terms <- map(topic_list, ~ get_top_terms(.x, 15))

plot_terms <- function(top_terms, topics = c(2, 13, 24, 35, 17, 29)) {
  top_terms %>%
    filter(topic %in% topics) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
    ) +
    facet_wrap(~ topic, scales = "free", ncol = 3) +
    coord_flip() +
    scale_x_reordered() +
    xlab("Begreber for udvalgte emner") +
    ylab("Begrebernes vægtning for emnet")
}

save_terms_plot <- function(p, name) {
  p %>%
    ggsave(filename = str_c("terms_", name, ".tex"),
           path = here("fig/"),
           width = 7,
           height = 7,
           device = tikz,
           standAlone = FALSE
           )
}

### DENDROGRAM
# plotting cluster relationships
model_to_clust <- function(model) {
  clust <- as.matrix(posterior(model)$terms) %>%
    CalcHellingerDist %>%
    as.dist %>%
    hclust("ward.D")
}

clust_to_dendro <- function(clust, branches = c(19, 23, 1)) {
  topics <- lda_to_topics(model)
  topic_labels <- get_top_terms(topics, 1)$term %>%
    modify(~ paste("  ", .x, sep = ""))
  dendro <- clust %>%
    as.dendrogram %>%
    set_labels(topic_labels) %>%
    set("labels_cex", 0.7) %>%
    set("labels_to_character") %>%
    set("branches_k_color", value = 6:1, k = 6) %>%
    # sort() %>%     # highlight_branches_col
    set("by_labels_branches_lwd", value = branches , type = "all")
}

save_topic_cluster_plot <- function(dendro, name = "dendro") {
  # TODO: tried building this programmatically
  #       but it turns parts of the string into subdirectories
  filename <- file.path(here("fig/"), paste0("cluster_", name, ".tex"))
  tikz(filename, width = 5, height = 5)
  par(mar = c(2, 2, 2, 10))
  plot(dendro, horiz = TRUE, axes = FALSE)
  dev.off()
}

# FIXME: the ggplot version still cuts off the plot labels
save_topic_cluster_plot <- function(dendro, name) {
  ggd <- as.ggdend(dendro, horiz = TRUE)
  p <- ggplot(ggd, horiz = TRUE)
    # prevent labels being chopped off
  p <- p + theme(plot.margin = margin(0, 2, 0, 0, "in"))
  #  for a circle graph
  #  scale_y_reverse(expand = c(0.2, 0)) +
  #  coord_polar(start = 0)
  p %>%
    ggsave(filename = str_c("cluster_", name, ".tex"),
           path = here("fig/"),
           width = 5,
           height = 7,
           device = tikz,
           standAlone = FALSE
           )
}

# topics pertaining to education per analysis period,
# as determined by visual inspection
# TODO: named list instead?
# TODO: Cannot handle more than one topic per period
# FIXME: quite monstrous code, actually
edu_topic_numbers <- c(6, 16, 7, 3, 9)
# read in required data
# subject to change
ldas <- readRDS(here("data/lda-35_periods_bigrams"))
ldas <- ldas[order(names(ldas))]
docs <- map(ldas, ~ lda_to_docs(.x))
corpora <- readRDS(here("data/speeches_with_periods.rds"))
# construct list for education docs
edu_docs <- vector("list", length(docs))

l <- list(topicnum = edu_topic_numbers, edu_docs = edu_docs, docs = docs)
edu_docs <- pmap(l, 
                 function(edu_docs, docs, topicnum) {
                   edu_docs <- filter(docs, topic == topicnum)}) %>%
                  # all docs have a probability of being assigned to a topic
                  # we only want the 3% most likely
                  map(~ filter(.x, gamma > quantile(gamma, prob = 1 - 3/100)))
names(edu_docs) <- names(docs)

get_edu_corpora <- function(corpus, doc_ids) {
  corpus %>%
  filter(doc_id %in% doc_ids)
}

edu <- map2(corpora, edu_docs,
                    ~get_edu_corpora(corpus = .x,
                                    doc_ids = .y$document))
# do we need metadata?
metadata <- readRDS(here("data/speeches_metadata.rds"))
edu <- edu %>%
  map(~ inner_join(.x, metadata, by = doc_id))


### WORDFISH
# using the austin package


# need to remove the per-period grouping now,
# to allow for more creative grouping later
wordfish_corpus <- function(corpus, timeperiod, group, n, filters, filter_col) {
  # unnest and clean up metadata, unselecting the 'all' group
  cat("cleaning corpus metadata\n")
  corpus <- corpus[corpus != "all"] %>%
    bind_rows %>%
    get_political
  if(!missing(filters)) {
  # add filter for unwanted rows in the resulting list
  # For instance, the Greenland and Faroese parties are not
  # all that relevant for the larger analysis
    cat("filtering\n")
    corpus <- corpus  %>%
      # turn group variable to a symbol
      # then unquote it
      # stupid black magic syntax
      # TODO: rewrite using .data[[group]] syntax
      #       ?dplyr_data_masking
      #       or dplyr::across
      filter(!!sym(filter_col) %in% filters)
  }
  if(!missing(group)) {
    cat(str_c("Group ", group, " detected\n"))
    split(corpus, corpus[timeperiod]) %>%
      map(. %>%
          corpus_to_fish({{ group }}, n)
        ) }
  else {
    cat(str_c("No group detected. Mapping to ", timeperiod, "\n"))
    corpus_to_fish(corpus, {{ timeperiod }}, n)
  }
}

corpus_to_fish <- function(corpus, group, n) {
  cat(str_c("Collating tokens on group: ", group, "\n"))
  corpus %>%
    select(text, all_of(group)) %>%
    group_by(across({{ group }})) %>%
    summarize(text = paste(text, collapse = " ")) %>%
    unnest_tokens(lemma, text, token = "ngrams", n = n) %>%
    count(lemma, across({{ group }}), sort = TRUE) %>%
    bind_tf_idf(lemma, {{ group }}, n) %>%
    cast_dtm({{ group }}, lemma, n) %>%
    as.wfm %>%
    trim_wfm(min.count = 5, min.doc = 0) %>%
    wordfish
}

trim_wfm <- function(wfm, min.count=5, min.doc = 5) {
  wfm <- trim(wfm, min.count = min.count, min.doc = min.doc)
  # this can lead to columns summing to 0
  wfm[,colSums(wfm != 0) != 0] 
}

coef_fish_to_tibble <- function(fish) {
  words <- coef(fish)$words
  words["token"] <- rownames(words)
  words <- as_tibble(words)
}

get_coef_terms <- function(coef_tibble, n = 10) {
  n <- n
  top_bot <- list(
    top = ~slice_max(.x, n = n),
    bot = ~slice_min(.x, n = n)
  )
  # FIXME: this is fugly amounts of repetition
  #        Can't get my head around how to do this the dplyr way
  #        so let's brute-force it for now
  max_psi <- coef_tibble %>%
    group_by(across(period)) %>%
    slice_max(psi, n = n) %>%
    select(period, token) %>%
    summarise(neutral = paste(token, collapse = ", "))
  min_psi <- coef_tibble %>%
    group_by(across(period)) %>%
    slice_min(psi, n = n) %>%
    select(period, token) %>%
    summarise(neutral = paste(token, collapse = ", "))
  max_beta <- coef_tibble %>%
    group_by(across(period)) %>%
    slice_max(beta, n = n) %>%
    select(period, token) %>%
    summarise(right = paste(token, collapse = ", "))
  min_beta <- coef_tibble %>%
    group_by(across(period)) %>%
    slice_min(beta, n = n) %>%
    select(period, token) %>%
    summarise(left = paste(token, collapse = ", "))
  terms <- inner_join(min_beta, max_psi) %>%
    inner_join(max_beta)
}

write_term_xtable <- function(coef_terms, name) {
  coef_terms <- coef_terms %>%
    rename("Periode" = period,
           "Venstreladede begreber" = left,
           "Højreladede begreber" = right,
           "Værdineutrale begreber" = neutral
           )
  t <- xtable(coef_terms,
              caption = "Oversigt over begrebsvægtning efter en \\textit{wordfish} beregning over analyseperioderne",
              label = "tab:lrterms",
              align = c("l",
                        "l",
                        "p{2in}",
                        "p{2in}",
                        "p{2in}"
                        )
  )
  print(t,
        type = "latex",
        include.rownames = FALSE,
        booktabs = TRUE,
        only.contents = TRUE,
        # latex.environments = "\\begin{adjustwidth}{-8em}{-8em}",
        table.placement = NULL,
        latex.environments = NULL,
        file = filename
  )
  # system2(command = "sed", args= (c("-i",
  #                                   "s/^\\&\\ //g",
  #                                   filename
  #                                   )
  # ))
}

coef_fishlist <- function(fishlist) {
  fishlist %>%
   map(~ coef_fish_to_tibble(.x)) %>% 
   bind_rows(.id = "period")
}

summarize_fish <- function(x) {
  # create a tibble of wordfish summaries
    if (!is(x, "wordfish"))
    stop("First argument must be a wordfish model")
    fish_summary <- rownames_to_column(summary(x)$scores, var = "Gruppe")  %>% as_tibble
}

periods_fishlist <- function(fishlist) {
  fishlist %>%
    map(~ summarize_fish(.x)) %>%
    bind_rows(.id = "Periode")
}

write_coef_fishlist_plot <- function(fishlist, name) {
  plots <- imap(fishlist, ~ ggplot_coef_fish(coef(.x), plot_title = .y))
  p <- plot_grid(plotlist = plots,
           ncol = 2,
           align = "vh"
           )
  save_coef_plot(p, str_c("list_", name))
}

plot_constructor <- function(words, beta, psi) {
  p <- ggplot(data = words, aes(x = beta, y = psi))
}

plot_coef_fish <- function(fish, psi = TRUE) {
  # if (!psi) {
  #   ggplot(words, aes(x = beta)) +
  #     geom_point()
  # }
  # if(length(fish) < 12) {
  #   fishlist <- TRUE
  #   words <- fish %>%
  #     map(~ coef_fish_to_tibble(.x)) %>%
  #     bind_rows(.id = "period")
  #    img_plot <- plot_constructor(words, beta, psi) +
  #      facet_wrap(vars(period), nrow = 2)
  # } else {
    words <- coef_fish_to_tibble(fish) %>%
      mutate(period = "1978-2020")
    img_plot <- plot_constructor(words, beta, psi)
  # }
  img_plot <- img_plot +
    geom_point() +
    theme_void() +
    theme(strip.text = element_blank())
    ggsave(img_plot,
           filename = "tmp.svg",
           path = here("tmp/"),
           device = svg
    )
  plot_img <- image_read("tmp/tmp.svg")
  p <- plot_constructor(words, beta, psi) +
    annotation_custom(rasterGrob(plot_img,
                                 width = unit(1, "npc"),
                                 height = unit(1, "npc")),
                      -Inf, Inf, -Inf, Inf) +
    xlab("Venstre-højre fordeling") +
    ylab("Ordenes fordeling")
  # if(fishlist) {
  #   p <- p  +
  #      facet_wrap(vars(period), nrow = 2)
  # }
  p <- p +
  geom_label_repel(data = top_n(words, 5, beta),
                  aes(label = token)
                  ) +
  geom_label_repel(data = top_n(words, 5, -beta),
                  aes(label = token)
                  ) +
  geom_label_repel(data = top_n(words, 5, psi),
                  aes(label = token)
                  )
}
save_coef_plot <- function(p, name) {
  filename <- str_c("coef_", name, ".tex")
  ggsave(p,
            filename = filename,
            path = here("fig"),
            device = tikz,
            standAlone = FALSE
  )
  system2(command = "sed", args= (c("-i",
                                    "s:coef:../fig/coef:g",
                                    here(str_c("fig/", filename))
                                    )
  )
  )
}
write_coef_plot <- function(p, name) {
  save_coef_plot(p, name)
}

plot_fishlist <- function(fishlist_periods) {
  ggplot(data = fishlist_periods,
         aes(y = Estimate, x = Periode, group = Gruppe)) +
    geom_line(aes(color = Gruppe)) +
    geom_point() +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size = rel(0.8))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Estimeret position")
}

write_wordfish_timeseries_plot <- function(fishlist, name, width = 5, height = 3) {
  fishlist <- periods_fishlist(fishlist) %>%
    plot_fishlist %>%
    ggsave(filename = here(str_c("fig/wordfish_", name, ".tex")),
           device = tikz,
           width = width,
           height = height,
           standAlone = FALSE
    )
}

###Sentiment analysis

read_ann <- function() {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("speeches_annotated_84\\d*.rds"
              )
  )
  cat("found annotations:\n")
  map(filenames, ~ cat(str_c(.x, "\n")))
  ann <- map(filenames, ~readRDS(here(str_c("data/", .x)))) %>%
    bind_rows()
}

filter_ann <- function(ann, edu) {
  #only keep the annotations for those documents that are
  # in our (presumed) education related corpus
  edudocs <- edu %>%
    bind_rows %>%
    select(doc_id) %>%
    unique %>%
    flatten %>%
    unlist
  ann %>%
    filter(doc_id %in% edudocs) %>%
    select(sentence, sentence_id, doc_id) %>%
    distinct
}

sentences  <- filter_ann(read_ann(), edu)

find_sentiments <- function(sentences, sentence) {
  sent <- list(
    sent_total = ~sentida(.x, "total"),
    sent_mean = ~sentida(.x, "mean")
    )
  sentences %>%
    rowwise %>%
    mutate(across({{sentence}}, sent, .names = "{.fn}")) %>%
    ungroup %>% 
    select(-{{sentence}})
}

tally_sentiments <- function(sentiments) {
  pol <- list(
    pos = ~sign(.x) > 0
    )
  pol_sum <- list(
    sumpos = ~sum(sign(.x) > 0),
    sumneg = ~sum(sign(.x) < 0)
    )
  sentiments %>%
    group_by(doc_id) %>%
    mutate(across(c(sent_mean, sent_total), pol_sum)) %>%
    # this adds a row per doc_id
    # not very tidy
    # add_count(across(c(sent_mean, sent_total), pol)) %>%
    mutate(across(c(sent_mean, sent_total), ~sign(.x), .names = "{.col}_sign")) %>%
    select(-n) %>%
    ungroup
}

aggregate_sentiments <- function(sentiments) {
  agg <- list(
    mean = ~mean(.x),
    median = ~median(.x),
    max = ~max(.x),
    min = ~min(.x)
    )
  sentiments %>%
  group_by(doc_id) %>%
  mutate(across(c(sent_total, sent_mean), agg)) %>%
  nest(data = c(sentence_id, sent_total, sent_mean, sent_mean_sign, sent_total_sign)) %>%
  ungroup %>%
  distinct %>%
  mutate(sentiments = data) %>%
  select(-data) # %>%
  #mutate(doc_id = as.double(doc_id))
}

sentiments <- find_sentiments(sentences, sentence) %>% tally_sentiments %>% aggregate_sentiments

edu_pol <- edu %>%
  bind_rows %>%
  get_political

doc_sentiments <- find_sentiments(edu_pol, text)

sent_pol <- left_join(sentiments, doc_sentiments) %>%
  mutate(timeseries = as.factor(timeseries))%>%
  mutate(Period = as.factor(Period)) %>%
  mutate(Parti = as.factor(Parti))

sent_pol %>%
  filter(Parti %in% parties) %>%
  group_by(Parti) %>%
    mutate(across(c(sent_mean_sumpos, sent_mean_sumneg), ~sum(.x)/cur_group_rows(), .names = "party_sum_{.col}")) %>%
    mutate(across(party_sum_sent_mean_sumneg, ~ .x * -1)) %>%
    pivot_longer(c(party_sum_sent_mean_sumneg, party_sum_sent_mean_sumpos), names_to = "sum", values_to = "score") %>%
    ggplot(aes(x = reorder(Parti, score),
               y = score, 
               fill = score > 0)) +
      geom_bar(stat="identity") +
      coord_flip() +
      guides(fill = FALSE)

bloc_sent <- filter(sent_pol, Blok %in% blocs)

party_sent <- filter(sent_pol, Parti %in% parties)

plot_sentiment_posneg <- function(corpus) {
  corpus %>%
    pivot_longer(c(sent_mean_min, sent_mean_max), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = Parti, y = value, color = variable)) +
      guides(color = "none") +
           facet_grid(timeseries ~ .)  +
           geom_point() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Spredning i talernes gennemsnitlige holdningsværdi, på sætningsniveau" ) +
    xlab(element_blank())
}

p<-plot_sentiment_posneg(filter(sent_pol, Parti %in% parties))

ggsave(p,
       device = tikz,
       filename = str_c("sent_posneg", identifier, ".tex"),
       path = here("fig/"),
       width = 5,
       height = 9
       )

plot_sentiment_minmax <- function(corpus) {
  corpus %>%
    # group_by(Parti) %>%
    # mutate(across(c(sent_mean_sumpos, sent_mean_sumneg), sum, .names = "party_sum_{.col}")) %>%
    # mutate(across(party_sum_sent_mean_sumneg, ~ .x * -1)) %>%
    # mutate(log_ratio = log2(party_sum_sent_mean_sumneg / party_sum_sent_mean_sumpos)) %>%
  pivot_longer(c(sent_total_min, sent_total_max), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = Parti, y = value, color = variable)) +
      guides(color = "none") +
           facet_grid(timeseries ~ .)  +
           geom_point() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Spredning i talernes yderlige holdningsværdi, på sætningsniveau" ) +
    xlab(element_blank())
}

p3 <-plot_sentiment_minmax (filter(sent_pol, Parti %in% parties))

ggsave(p3,
       device = tikz,
       filename = str_c("sent_minmax", identifier, ".tex"),
       path = here("fig/"),
       width = 5,
       height = 9
       )


plot_sentiment_time <- function(corpus, group, period) {
  corpus %>%
  # pivot_longer(c(sent_mean, sent_total), names_to = "variable", values_to = "value") %>%
  ggplot(aes(y = sent_mean, x = Date)) +
    facet_grid(group) +
    geom_point(aes_string(color = period)) +
    theme(axis.text.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = "bottom",
    ) +
    ylab("Gennemsnitlig holdningsværdi, på talebasis") +
    xlab("Tid")
}

# WIP: the above crashes luatex
#      attempting another tack
plot_sentiment_time <- function(corpus, group, period) {
  p <- ggplot(corpus, aes(y = sent_mean, x = Date)) +
    facet_grid(group) +
    geom_point(aes_string(color = period))
  img_plot <- p +
    theme_void() +
    theme(strip.text = element_blank())
  ggsave(img_plot,
         filename = "tmp.svg",
           path = here("tmp/"),
           device = svg
    )
  plot_img <- image_read("tmp/tmp.svg")
  p <- p +
    annotation_custom(rasterGrob(plot_img,
                                 width = unit(1, "npc"),
                                 height = unit(1, "npc")),
                      -Inf, Inf, -Inf, Inf) +
    theme(axis.text.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = "bottom",
    ) +
    ylab("Gennemsnitlig holdningsværdi, på talebasis") +
    xlab("Tid")
}

p2 <- plot_sentiment_time(filter(doc_sentiments, Parti %in% parties), "Parti", "timeseries")

ggsave(p2,
       device = pdf,
       filename = str_c("sent_spread_time_", identifier, ".pdf"),
       path = here("fig/"),
       width = 8,
       height = 9
  )

plot_sentiment_time <- function(corpus, group, period) {
  corpus %>%
  # pivot_longer(c(sent_mean, sent_total), names_to = "variable", values_to = "value") %>%
  ggplot(aes(y = sent_mean, x = Date)) +
    facet_grid(group) +
    geom_point(aes_string(color = period)) +
    theme(axis.text.x = element_blank())
}

### METADATA WORK
# All filtering/coercion/massaging happens to bare metadata
# the text is... quite voluminous
metadata <- readRDS(here('data/speeches_metadata.rds'))

# fix transcription errors and inconsistencies
clean_parties <- function(metadata) {
  metadata <- metadata %>%
    mutate(Parti = replace_na(Parti, "ikke angivet")) %>%
    mutate(Parti = str_remove_all(Parti, "(?:20px - |x - |- |Det )")) %>%
    mutate(Parti = str_replace(Parti, "Atassut", "Atássut")) %>%
    mutate(Parti = str_replace(Parti, "Tjódveldisflokkurin", "Tjóðveldisflokkurin")) %>%
    mutate(Parti = str_replace(Parti, "Tjóðveldi$", "Tjóðveldisflokkurin")) %>%
    mutate(Parti = str_replace(Parti, "Javnadarflokkurin$", "Javnaðarflokkurin")) %>%
    mutate(Parti = str_replace(Parti, "Socialdemorkatiet", "Socialdemokratiet")) %>%
    mutate(Parti = str_replace(Parti, "Socialdemokraterne", "Socialdemokratiet")) %>%
    mutate(Parti = str_replace(Parti, "Centrumdemokraterne", "Centrum-Demokraterne"))
}

get_political <- function(metadata) {
  metadata %>%
    clean_parties %>%
    # Erring on the side of inclusivity
    # Some more centrist parties may have issue
    mutate(Blok = factor(case_when(
           Parti %in% c(
                        "Dansk Folkeparti",
                        "Venstre",
                        "Konservative Folkeparti",
                        "Fremskridtspartiet",
                        "De Uafhængige",
                        "Nye Borgerlige"
                        ) ~ "Blå Blok",
           Parti %in% c(
                        "Alternativet",
                        "Enhedslisten",
                        "Socialdemokratiet",
                        "Socialistisk Folkeparti",
                        "Danmarks Kommunistiske Parti",
                        "Venstresocialisterne",
                        "Fælles Kurs"
                        ) ~ "Rød Blok",
           Parti %in% c(
                        "Uden for partierne",
                        "Kristeligt Folkeparti",
                        "Centrum-Demokraterne",
                        "Radikale Venstre",
                        "Ny Alliance",
                        "Liberal Alliance",
                        "Retsforbundet",
                        "Liberalt Centrum"
                        ) ~ "Centrum",
           # this isn't entirely accurate 
           # unafilliated members are also picked up
           Parti %in% c(
                        "Javnaðarflokkurin",
                        "Tjóðveldisflokkurin",
                        "Fólkaflokkurin",
                        "Sambandsflokkurin",
                        "Atássut",
                        "Siumut",
                        "Inuit Ataqatigiit"
                        ) ~ "Grønland/Færøerne",
           Parti == "ikke angivet" ~ "ikke angivet",
           ), levels = c("Rød Blok",
           "Blå Blok",
           "Centrum",
           "Grønland/Færøerne",
           "ikke angivet")))
}

split_by_govt <- function(metadata) {
  metadata %>%
    split(.$Period)
}
split_by_block <- function(metadata) {
  metadata %>%
    split(.$Blok)
}


ggsave_to_variable <- function(p, width = 10, height = 10, dpi = 300) {
  pixel_width <- (width  * dpi) / 2.54
  pixel_height <- (height * dpi) / 2.54
  img <- magick::image_graph(pixel_width, pixel_height, res = dpi)
  on.exit(utils::capture.output({
    grDevices::dev.off()}))
  plot(p)
  return(img)
}
