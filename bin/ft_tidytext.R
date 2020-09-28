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
            mdels = models,
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

  folketinget[, timeseries := future_sapply(Date, (function(x) {
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

filter_dtm <- function(dtm) {
  dtm %>%
    # this removes terms that do not occur in the upper
    # two per mille of documents
    # NOTE: given the particularities of this dataset, this may
    #       reduce complexity overmuch
    removeSparseTerms(0.998) %>%
    .[unique(.$i), ]
}

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

topicmodels_json_ldavis <- function(model, dtm, name, k, period){
  require(LDAvis)
  require(slam)
  library(parallel)
  cluster <- makePSOCKcluster(
                names = 1
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
imap(jsons, ~ serVis(
                json = .x, 
                out.dir = here(str_c("vis/", identifier, "/", .y)),
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
edu_corpora <- edu_corpora %>%
  map(~ inner_join(.x, metadata, by = doc_id))

edu_by_govt <- split_by_govt(edu_corpora$all)

edu_by_block <- map(edu_corpora, ~ split_by_block(.x))

edu_block_tfidfs  <- map(edu_by_block, ~ map(.x, ~ bind_tf_idf(.x, lemma, doc_id, n)))

edu_block_dtms  <- map(edu_block_tfidfs, ~ map(.x, ~ generate_dtms((filter_tfidf(.x, treshold)))))

### WORDFISH
# using the austin package

# we need to convert DocumentTerm matrices to WordFrequency matrices
edu_block_wfms  <- map(edu_block_dtms, ~ map(.x, ~ as.wfm(.x)))

# This can be memory intensive, so serialize to disk along the way
write_wfms <- function(wfms, name, period) {
    saveRDS(wfms, here(str_c("data/wfm_", name = identifier, "_", period, '.rds')))
}
imap(edu_block_wfms, ~ write_wfms(
     wfms = .x,
     name = identifier,
     period = .y))

read_wfm <- function(name) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("fish_", name, ".*.rds")
  )

# trim wfm to remove seldom-occuring tokens
trim_wfm <- function(wfm, min.count=5, min.doc = 5) {
  wfm <- trim(wfm, min.count = min.count, min.doc = min.doc )
  # this can lead to columns summing to 0
  wfm[,colSums(wfm != 0) != 0] 
}

order_wfm <- function(wfm) {
  wfm <- wfm[,order(as.integer(colnames(wfm)))]
}


fish <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(fish) <- filenames %>%
    map(~ str_match(.x, pattern = ".*_(\\d+-\\d+|all).rds")[, 2])
  return(fish)
}
write_wordfish <- function(wfms, name, period) {
  wfms %>%
    map(~ wordfish(.x)) %>%
    saveRDS(., here("data/fish_", name = identifier, "_", period))
}

imap(fish, ~ write_wordfish(
     fish = .x,
     name = str_c(identifier, "_2014-20"),
     period = .y))

read_wordfish <- function(name) {
  filenames <- list.files(
              path = here("data/"),
              pattern = str_c("fish_", name, ".*.rds")
  )
  fish <- map(filenames, ~readRDS(here(str_c("data/", .x))))
  names(fish) <- filenames %>%
    map(~ str_match(.x, pattern = ".*_(\\d+-\\d+|all).rds")[, 2])
  return(fish)
}


write_plot_fish <- function(fish, name, period) {
  fish <- as_tibble(fish) %>%
    select(`Rød Blok`, `Blå Blok`)
  plots <- imap(fish, ~ ggplot_fish(coef(.x), plot_title = .y ))
  p <- plot_grid(plotlist=plots) %>%
  save_plot(
           filename = str_c("coef_", name, "_", period, ".tex"),
           nrow = 2,
           path = here("fig"),
           device = tikz,
           width = 3
           standAlone = FALSE
    )
}

#' ggPlot the Word Parameters From a Wordfish Model
#' 
#' Plots sorted beta and optionally also psi parameters from a Wordfish model
#' Rewritten to use ggplot2 by Martin Andersen
#' 
#' 
#' @param x a fitted Wordfish model
#' @param pch Default is to use small dots to plot positions
#' @param psi whether to plot word fixed effects
#' @param ... Any extra graphics parameters to pass in
#' @return A plot of sorted beta and optionally psi parameters.
#' @author Will Lowe
#' @author Martin Andersen
#' @seealso \code{\link{wordfish}}
#' @importFrom methods is
#' @importFrom ggplot2 dotchart text plot
#' @export
#' @method ggplot_fish
ggplot_fish <- function(x, psi=TRUE, plot_title = "placeholder", ...){

  if (!is(x, "coef.wordfish"))
    stop("First argument must be coefficients from a Wordfish model")

  if (is.null(x$docs))
    stop(paste("Plotting word parameters in the multinomial parameterization",
               "probably won't\n  be very informative.  Try plotting the value",
               "of coef(model, form='poisson')"))
  if(!missing(plot_title)) {
    plot_title <- plot_title
  }

  words <- x$words
  words["token"] <- rownames(words)
  words <- as_tibble(words)
  if (!psi) {
    ggplot(words, aes(x = beta)) +
           geom_dotplot()
  } else {
    ggplot(data = words, aes(x = beta, y = psi)) +
           geom_point() +
           xlab("Beta") +
           ylab("Psi") +
           geom_label(data = top_n(words, 5, beta),
                      aes(label = token)
           ) +
           geom_label(data = top_n(words, 5, -beta),
                      aes(label = token)
           ) +
           geom_label(data = top_n(words, 5, psi),
                      aes(label = token)
           ) +
           ggtitle(plot_title)
  }
}

map(fish, ~ write_plot_fish(fish = fish,
                           name = "edu_test",
                           period = "2014-20"))

edu_tfidfs <- map(edu_corpora, ~  bind_tf_idf(.x, lemma, doc_id, n))
edu_dtms <- map(edu_tfidfs, ~ generate_dtms(.x))

### METADATA WORK
# All filtering/coercion/massaging happens to bare metadata
# the text is... quite voluminous
metadata <- readRDS(here('data/speeches_metadata.rds'))

# fix transcription errors and inconsistencies
clean_parties <- function(metadata) {
  metadata <- metadata %>%
    mutate(Parti = str_remove_all(Parti, "(?:20px - |x - |- |Det )")) %>%
    mutate(Parti = str_replace(Parti, "Atassut", "Atássut")) %>%
    mutate(Parti = str_replace(Parti, "Tjódveldisflokkurin", "Tjóðveldisflokkurin")) %>%
    mutate(Parti = str_replace(Parti, "Tjóðveldi$", "Tjóðveldisflokkurin")) %>%
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
            "Liberal Alliance",
            "Venstre",
            "Konservative Folkeparti",
            "Kristeligt Folkeparti",
            "Ny Alliance",
            "Centrum-Demokraterne",
            "Fremskridtspartiet",
            "De Uafhængige",
            "Liberalt Centrum",
            "Nye Borgerlige",
            "Retsforbundet"
            ) ~ "Blå Blok",
        Parti %in% c(
            "Alternativet",
            "Enhedslisten",
            "Radikale Venstre",
            "Socialdemokraterne",
            "Socialistisk Folkeparti",
            "Danmarks Kommunistiske Parti",
            "Venstresocialisterne",
            "Fælles Kurs"
            ) ~ "Rød Blok",
        # this isn't entirely accurate 
        # unafilliated members are also picked up
        TRUE ~ "Grønland/Færøerne"
    ), levels = c("Rød Blok", "Grønland/Færøerne", "Blå Blok")))
}

split_by_govt <- function(metadata) {
  metadata %>%
    split(.$Period)
}
split_by_block <- function(metadata) {
  metadata %>%
    split(.$Blok)
}
