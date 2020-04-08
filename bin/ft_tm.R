# ft_tidytext.R
# exploratory data analysis using the tidytext packages

library(tidyverse)
library(tidytext)
library(tm)
library(here)
library(Matrix)

ft_speeches <- read_csv(here("data/folketinget_1953_2019_raw.csv"))

ft_corpus <-
    VCorpus(DataframeSource(ft_speeches),
        readerControl = list(langugage = "da")
    )
remove_hwalp <- function(text) {
    str_remove_all(text, '홢')
}
# Return NA instead of tolower error
tryTolower <- function(x) {
    # return NA when there is an error
    y <- NA
    # tryCatch error
    try_error <- tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, "error")) {
          y <- tolower(x)
      }
    return(y)
}

# might want to add more stopwords later
ft_stopwords <- c(stopwords("danish"))

# or, alternately, automattically remove most/least _n_ words
clean_corpus <- function(corpus) {
    corpus <- tm_map(
        corpus,
        content_transformer(remove_hwalp)
    )
    corpus <- tm_map(
        corpus,
        content_transformer(tryTolower)
    )
    corpus <- tm_map(
        corpus, removeWords,
        ft_stopwords
    )
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, stemDocument)
    corpus <- tm_map(corpus, removeNumbers)
    return(corpus)
}

ft_corpus <- clean_corpus(ft_corpus)

ft_tdm <- TermDocumentMatrix(
    ft_corpus,
    control = list(
        weighting = weightTfIdf
        # bounds = list(global = c(0, 0))
    )
)

terms <- findFreqTerms(ft_tdm, 5)
