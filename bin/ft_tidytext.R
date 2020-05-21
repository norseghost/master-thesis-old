# ft_tidytext.R
# exploratory data analysis using the tidytext packages

library(tidyverse)
library(tidytext)
library(tm)
library(here)
library(future)
library(topicmodels)
library(ldatuning)
library(textmineR)
library(gmp)
library(gofastr)
options(future.globals.maxSize = 8912896000)
plan(multicore)

ft_speeches <- read_csv(here("data/ft_lematized_timeseries.csv"))
factor
ft_tidy <- ft_speeches  %>%
    unnest_tokens(lemma, text)

# count token occurrences across documents

ft_tidy <- read_csv(here("data/ft_tidy_lemmatized.csv"))

ft_periods <- ft_tidy %>%
    distinct(timeseries) %>%
    pull

ft_periods_tokens <- vector("list", length(ft_periods))
names(ft_periods_tokens) <- ft_periods
for (i in seq_along(ft_periods)) {
    ft_periods_tokens[[i]] <- ft_tidy %>%
        filter(timeseries == ft_periods[[i]]) %>%
        count(lemma, doc_id, sort = TRUE)
}


ft_periods_tokens <- ft_periods_tokens[-c(1,4)]

ft_periods_tokens[["all"]] <- ft_tidy %>%
    filter(timeseries == names(ft_periods_tokens)) %>%
    count(lemma, doc_id, sort = TRUE)

saveRDS(ft_periods_tokens, here("data/ft_periods_tokens.rds"))

ft_periods_tokens <- readRDS(here("data/ft_periods_tokens.rds"))


ft_periods_tfidf <- ft_periods_tokens %>%
    map(~ future(bind_tf_idf(.x, lemma, doc_id, n))) %>%
    values %>%
    # filter out terms that are in the lower 40 percent of
    # tf_idf score per corpus
    map(~ filter(.x, tf_idf > quantile(tf_idf, .4, na.rm = TRUE)))

ft_periods_dtm <- ft_periods_tfidf %>%
    map(~ future(cast_dtm(.x, doc_id, lemma, n))) %>%
    values

ft_periods_models <- ft_periods_dtm %>%
    map(~ FindTopicsNumber(
      .x,
      topics = seq(from = 2, to = 30, by = 1),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 1234)
    ))

# TODO: Run a coherence score test per period to determine 'k'
# https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
k_list <- seq(1, 20, by = 1)
generate_models <- function(dtm, ks) {
            # filename <- file.path(here("models/"), paste0(k, "_topics.rda"))
            # if (!file.exists(filename)) {
    models <- vector("list", length(ks))
    for (k in seq_along(ks)) {
                models[[k]] <- FitLdaModel(dtm = dtm, k = k, method = "vem")
                models[[k]]$k <- k
                models[[k]]$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
           }
                # save(m, file = filename)
            # } else {
            #     load(filename)
            # }
}



group_99_tidy %>%
    count(lemma, sort = TRUE) %>%
    filter(n > 2500) %>%
    mutate(lemma = reorder(lemma, n)) %>%
    ggplot(aes(lemma, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

group99_tfidf <- group_99_tidy %>%
    bind_tf_idf(lemma, doc_id, n)

group99_tfidf %>%
    arrange(desc(tf_idf)) %>%
    str_detect(, "udd")

group99_dtm <- group_99_tidy %>%
    cast_dtm(doc_id, lemma, n)

library(topicmodels)
group_99_lda <- LDA(group99_dtm, k = 24, control = list(seed = 1234))

gr99_topics <- tidy(group_99_lda, matrix = "beta")

gr99_top_terms <- gr99_topics %>%
    group_by(topic) %>%
    top_n(15, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

gr99_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()

ggsave("gr99-2.png", path = here("fig"))
