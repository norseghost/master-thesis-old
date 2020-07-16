# ft_tidytext.R
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
control <- list(
            seed = seed,
            burnin = burnin,
            thin = thin,
            iter = iter)


ft_speeches <- read_csv(here("data/ft_clean_no_stopwords_timeseries.csv"))


  
ft_periods <- ft_speeches %>%
    filter(timeseries != "1957-68") %>%
    filter(timeseries != "1968-78") %>%
    distinct(timeseries) %>%
    pull
ft_periods_speeches <- vector("list", length(ft_periods))
names(ft_periods_speeches) <- ft_periods
for (i in seq_along(ft_periods)) {
    ft_periods_speeches[[i]] <- ft_speeches %>%
        filter(timeseries == ft_periods[[i]])
}
ft_periods_speeches[["all"]] <- ft_speeches %>%
    filter(timeseries %in% ft_periods)
saveRDS(ft_periods_speeches, here("data/ft_periods_speeches_stopwords.rds"))

ft_periods_speeches <- readRDS(here("data/ft_periods_speeches_stopwords.rds"))

ft_periods_tidy <- ft_periods_speeches  %>%
    map(~ unnest_tokens(.x, lemma, text, token = "ngrams", n = 2))
saveRDS(ft_periods_tidy, here("data/ft_periods_bigrams_stopwords.rds"))

ft_periods_tidy <- readRDS(here("data/ft_periods_bigrams_stopwords.rds"))
ft_periods_tokens <- ft_periods_tidy %>%
  map(~ future(count(.x, lemma, doc_id, sort = TRUE))) %>%
  values
saveRDS(ft_periods_tokens, here("data/ft_periods_bigrams_tokens_stopwords.rds"))
ft_periods_tfidf <- ft_periods_tokens %>%
    map(~ future(bind_tf_idf(.x, lemma, doc_id, n))) %>%
    values
saveRDS(ft_periods_tfidf, here("data/ft_periods_bigrams_tfidf_stopwords.rds"))

ft_periods_tfidf %>%
  map(~ summary(unique(.x$tf_idf, na.rm = TRUE)))
ft_periods_tfidf %>%
  map(~ summary(.x$tf_idf))



# ft_periods_tokens <- readRDS(here("data/tokens-trigrams-1990-01.rds")) %>%
#   count(lemma, doc_id, sort = TRUE)
# saveRDS(ft_periods_tokens, here("data/tokens_trigrams_count-1990-01.rds"))

# ft_periods_tokens <- readRDS(here("data/tokens-trigrams-2001-14.rds")) %>%
#   count(lemma, doc_id, sort = TRUE)
# saveRDS(ft_periods_tokens, here("data/tokens_trigrams_count-2001-14.rds"))
# ft_periods_tokens <- readRDS(here("data/tokens-trigrams-2014-20.rds")) %>%
#   count(lemma, doc_id, sort = TRUE)
# saveRDS(ft_periods_tokens, here("data/tokens_trigrams_count-2014-20.rds"))

# ft_periods_tokens <- readRDS(here("data/ft_periods_tokens_bigrams.rds"))
# look at range of terms in tfidf
ft_periods_tfidf %>%
    map(. %>%
        filter(tf_idf > mean(unique(tf_idf), na.rm = TRUE)) %>%
        distinct(lemma, tf_idf) %>%
        arrange(lemma, -tf_idf) %>%
        top_n(-30) %>%
        print(n = 30)
    )

    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    map(~ filter(.x, tf_idf < quantile(tf_idf, 0.998)))

ft_periods_tfidf_filter <- ft_periods_tfidf %>%
    map(~ future(filter(.x, tf_idf > mean(unique(tf_idf), na.rm = TRUE)))) %>%
    values
ft_periods_dtm <- ft_periods_tfidf_filter %>%
    map(~ future(cast_dtm(.x, doc_id, lemma, n))) %>%
    values



saveRDS(ft_periods_dtm, here("data/dtm_periods_stopwords_bigrams"))

ft_periods_dtm <- readRDS(here("data/dtm_periods_stopwords_bigrams"))

ft_periods_dtm_nosparse <- ft_periods_dtm %>%
  map(~ future(removeSparseTerms(.x, 0.9999))) %>%
  values

saveRDS(ft_periods_dtm_nosparse, here("data/dtm_periods_stopwords_nosparse_bigrams"))

ft_periods_dtm_nosparse <- readRDS(here("data/dtm_periods_stopwords_nosparse_bigrams"))

ft_periods_lda <- ft_periods_dtm %>%
    map(~ future(LDA(.x, method = "Gibbs", k = 35, control = control ))) %>%
    values

saveRDS(ft_periods_lda, here("data/lda-35_periods_bigrams"))

ft_periods_docs <- ft_periods_lda %>%
    map(~ future(tidy(.x, matrix = "gamma"))) %>%
    values


ft_periods_topics <- ft_periods_lda %>%
    map(~ future(tidy(.x, matrix = "beta"))) %>%
    values

models_compare <- function(dtm, name) {
  dtm  <- dtm[unique(dtm$i), ]
  max_K <- 204
  steps <- 5
  models <- FindTopicsNumber(
      dtm,
      topics = seq(from = 2, to = max_K, by = steps),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = control,
      verbose = TRUE,
      return.models = TRUE
    )
  saveRDS(models, here(str_c("data/", name, "_", max_K, "by", steps,".rds")))
}

imap(ft_periods_dtm_nosparse, models_compare(dtm = .x, name = .y))

ft_periods_models <- ft_periods_dtm_nosparse %>%
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

ft_periods_models <- readRDS(here("data/models_120.rds"))

models_plot <- ft_edu_models %>%
    map(~ normalize_topic_numbers(.x)) %>%
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

normalize_topic_numbers <- function(values) {
  # Drop models if present, as they won't rescale
  # Also, Deveaud is not useful for this dataset
  values <- values %>% 
    select(-Deveaud2014)
  # normalize to [0,1]
  columns <- values %>%
    select(-topics) %>%
    modify(~ scales::rescale(.x, to = c(0, 1), from = range(.x))) 
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


ft_assignments <- map2(ft_periods_lda, ft_periods_dtm, augment, .x, .y)

saveRDS(ft_assignments, here("data/assignments_bigrams_k35.rds"))


# TODO: I'm writing a shitty and probably slow reimplementatin of map
#       This is probably ill advised, but seems faster than getting map to work
ft_periods_top_terms <- vector("list", length(ft_periods))
names(ft_periods_top_terms) <- ft_periods
for (i in seq_along(ft_periods)) {
ft_periods_top_terms[[i]] <- ft_periods_topics[[i]] %>%
        group_by(topic) %>%
        top_n(15, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
}

ft_periods_top_terms <- ft_periods_topics %>%
  modify(. %>%
        group_by(topic) %>%
        top_n(15, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
  )

ft_periods_term_plots  <- vector("list", length(ft_periods_top_terms))
names(ft_periods_term_plots) <- names(ft_periods_top_terms)
for (i in seq_along(ft_periods_top_terms)) {
ft_periods_term_plots[[i]] <- ft_periods_top_terms[[i]] %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free", ncol = 5) +
        coord_flip() +
        scale_x_reordered()
}

saveRDS(ft_periods_term_plots, here("data/plots-bigrams-k35.rds"))

paths <- str_c(names(ft_periods_term_plots), "-topicnumbers.pdf")
pwalk(list(paths, ft_periods_term_plots), ggsave, path = here("fig"), width = 20, height = 40)

ft_periods_term_plots <- ft_periods_top_terms %>%
  modify(. %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
  )


topicmodels_json_ldavis <- function(fitted, doc_term){
  require(LDAvis)
  require(slam)
  library(parallel)
  cluster <- makePSOCKcluster(
                names = 8
        )
  # Find required quantities
  phi <- as.matrix(posterior(fitted)$terms)
  theta <- as.matrix(posterior(fitted)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(doc_term)
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(doc_term$i)),
                                 term.frequency = term_freq,
                                 cluster = cluster)
  return(json_lda)
}

ft_json <- map2(ft_periods_lda, ft_periods_dtm, topicmodels_json_ldavis)

library(LDAvis)

imap(ft_json, ~ serVis(json = .x, out.dir = str_c(here("vis/", .y)), open.browser = FALSE))



# topics pertainig to education, as determined by visual inspection
#TODO: named list instead?
edu_topic_numbers <- c(6, 16, 7, 3, 9)

# construct list for 
ft_edu_docs<- vector("list", length(ft_periods_docs))

l <- list(topicnum = edu_topic_numbers, edu_docs = ft_edu_docs, docs = ft_periods_docs)

ft_edu_docs <- pmap(l, function(edu_docs, docs, topicnum) { edu_docs <- filter(docs, topic == topicnum)}) %>%
# all docs have a probability of beaing assigned to a topic
# we only want the 1%
  map(~ filter(.x, gamma > quantile(gamma, 0.97))
names(ft_edu_docs) <- names(ft_periods_docs)

ft_edu_tokens <- map2(ft_periods_tokens, ft_edu_docs, ~ filter(.x, doc_id %in% .y$document))

ft_edu_tfidf <- ft_edu_tokens %>%
    map(~ future(bind_tf_idf(.x, lemma, doc_id, n))) %>%
    values %>%
    # filter out terms that are below the median tf_idf score per corpus
    # doi://10.18637/jss.v040.i13 for rationale,
    map(~ filter(.x, tf_idf > median(tf_idf, na.rm = TRUE))) %>%
    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    map(~ filter(.x, tf_idf < quantile(tf_idf, 0.998)))
ft_edu_dtm <- ft_edu_tfidf %>%
    map(~ future((cast_dtm(.x, doc_id, lemma, n)))) %>%
    values

ft_edu_lda <- ft_edu_dtm %>%
    map(~ future(LDA(.x, method = "Gibbs", k = 15, control = list(seed = 1234, burnin = 500, thin = 300, iter = 3000)))) %>%
    values
edu_json <- map2(ft_edu_lda, ft_edu_dtm, topicmodels_json_ldavis)
imap(ft_json, ~ serVis(json = .x, out.dir = str_c(here("vis/edu/", .y)), open.browser = FALSE))


ft_edu_models <- ft_edu_dtm %>%
    map(~ FindTopicsNumber(
      .x,
      topics = seq(from = 5, to = 60, by = 5),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 1234),
      verbose = TRUE,
      mc.cores = 8
    ))

