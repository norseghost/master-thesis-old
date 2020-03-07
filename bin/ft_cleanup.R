# ft_cleanup.R
# clean up text in the folketinget dataset

library(tidyverse)
library(data.table)
library(tm)
library(here)
library(parallel)

ft_raw <- fread(here("data/folketinget_1953_2019_raw.csv"))
# prepare udpipe for lemmatization
library(udpipe)
udmodel <- udpipe_download_model("danish")
lemmatize_speeches <- function(text) {
    dt <- udpipe_annotate(
        object = udpipe_load_model(udmodel$file_model),
        x = text,
        doc_id = "1",
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

test <- lemmatize_speeches(ft_raw$text[[355]], ft_raw$doc_id[[355]])


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

ft_clean <- ft_raw[, text := sapply(
                             .SD[, text], clean_text),
             by = .groups]

ft_lemmatized <- ft_clean[, text := sapply(
                             .SD[, text], lemmatize_speeches),
             by = .groups]

# tried running this in parallel
# but that killed my laptop :(
# OOm maybe?
cluster <- makePSOCKcluster(names = 4, outfile = "")
# the cluster needs to se e my stopwords
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

