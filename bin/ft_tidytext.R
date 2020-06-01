# ft_tidytext.R
# exploratory data analysis using the tidytext packages

library(tidyverse)
library(tidytext)
library(tm)
library(here)
library(future)
library(topicmodels)
library(ldatuning)
library(tikzDevice)
options(future.globals.maxSize = 8912896000)
plan(multicore)

ft_speeches <- read_csv(here("data/ft_lematized_timeseries.csv"))

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
    # filter out terms that are below the median tf_idf score per corpus
    # doi://10.18637/jss.v040.i13 for rationale,
    map(~ filter(.x, tf_idf > median(tf_idf, na.rm = TRUE))) %>%
    # also filter out the 0.002 most rare terms
    # (of those left) to catch misspellings and errors
    # TODO: Reference?
    map(~ filter(.x, tf_idf, < quantile(tf_idf, 0.998)))

ft_periods_dtm <- ft_periods_tfidf %>%
    map(~ future((cast_dtm(.x, doc_id, lemma, n)))) %>%
    values

ft_periods_models <- ft_periods_dtm %>%
    map(~ FindTopicsNumber(
      .x,
      topics = seq(from = 2, to = 100, by = 3),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 1234)
    ))

models_plot <- ft_periods_models %>%
    map(~ normalize_topic_numbers(.x)) %>%
    bind_rows(.id = "period") %>%
    reshape2::melt(., id.vars = c("topics", "period"), na.rm = TRUE) %>%
    plot_topic_numbers %>%
    ggsave(
           filename="models_plot.tex",
           width=7,
           height=10, 
           units = "in",
           device = tikz,
           path = here("fig"),
           standAlone = FALSE)

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


ft_periods_lda <- ft_periods_dtm %>%
    map(~ future(LDA(.x, k = 15, control = list(seed = 1234)))) %>%
    values

ft_periods_docs <- ft_periods_lda %>%
    map(~ future(tidy(.x, matrix = "gamma"))) %>%
    values


ft_periods_topics <- ft_periods_lda %>%
    map(~ future(tidy(.x, matrix = "beta"))) %>%
    values

# TODO: run this for all periods
ft_assignments <- augment(ft_periods_lda[[3]], data = ft_periods_dtm[[3]])


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

ft_periods_term_plots  <- vector("list", length(ft_periods))
names(ft_periods_top_terms) <- ft_periods
for (i in seq_along(ft_periods)) {
ft_periods_term_plots[[i]] <- ft_periods_top_terms[[i]] %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
library(tikzDevice)
        coord_flip() +
        scale_x_reordered()
}

library(tikzDevice)
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

paths <- str_c(names(models_plot), "-topicnumbers.tex")

pwalk(list(paths, models_plot), ggsave, width=4, height=3, units = "in", device = tikz, path = here("fig"), standAlone = FALSE)
