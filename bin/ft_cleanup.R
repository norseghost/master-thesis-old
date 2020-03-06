# ft_cleanup.R
# clean up text in the folketinget dataset

library(tidyverse)
library(data.table)
library(tm)
library(here)
library(parallel)

ft_speeches <- fread(here("data/folketinget_1953_2019_raw.csv"))


# I'll probably want more stopwords later
# after some exploratory analysis
ft_stopwords <- c(stopwords("danish"))
clean_text <- function(text) {
    text %>%
    removeNumbers %>%
    removePunctuation %>%
    removeWords(., ft_stopwords) %>%
    # the corpus contains occurences of hangul character
    # hwalp - 홢 - this is unwanted
    str_remove_all(., "홢") %>%
    stripWhitespace %>%
    tolower
}

ft_speeches[, text := sapply(
                             .SD[, text], clean_text),
             by = .groups]
# tried running this in parallel
# but that killed my laptop :(
# OOm maybe?
 cluster <- makePSOCKcluster(names = 4, outfile = "")
# the cluster needs to see my stopwords
clusterExport(
        cl = cluster,
        varlist = c("ft_stopwords"),
        envir = .GlobalEnv
)
#and needs to have the required libraries loaded
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

