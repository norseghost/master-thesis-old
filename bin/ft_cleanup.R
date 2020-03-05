# ft_cleanup.R
# clean up text in the folketinget dataset

library(tidyverse)
library(data.table)
library(tm)
library(here)
library(parallel)

ft_speeches <- fread(here("data/folketinget_1953_2019_raw.csv"))

cluster <- makePSOCKcluster(names = 4, outfile = "")

# I'll probably want more stopwords later
# after some exploratory analysis
ft_stopwords <- c(stopwords("danish"))
clean_text <- function(text) {
    removeNumbers(text)
    removePunctuation(text)
    removeWords(text, ft_stopwords)
    stripWhitespace(text)
    # the corpus contains occurences of hangul character
    # hwalp - 홢 - this is unwanted
    str_remove_all(text, '홢')
    tolower(text)
}

# the cluster needs to see my stopwords
clusterExport(
        cl = cluster,
        varlist = c("ft_stopwords"),
        envir = .GlobalEnv
)
clusterEvalQ(
    cl = cluster, {
        library(tm)
        library(tidyverse)
    }
)

# apply the cleaning operation in parallel
# the dataset is already prepared in 100 batches
ft_speeches[, text := parSapply(
                            cluster, .SD[, text], clean_text),
            by = .groups]
