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
