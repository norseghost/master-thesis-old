# ft_cleanup.R
# clean up text in the folketinget dataset

library(tidyverse)
library(data.table)
library(tm)
library(here)


ft_raw <- fread(here("data/folketinget_1953_2019_raw.csv"))

# prepare udpipe for lemmatization
library(udpipe)
udmodel <- udpipe_download_model("danish")
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

# I'll probably want more stopwords later
# after some exploratory analysis
ft_stopwords <- c(

    readLines(
        "https://raw.githubusercontent.com/stopwords-iso/stopwords-da/master/stopwords-da.txt",
        warn = FALSE),
    readLines(
        here("bin/stopwords.txt"))
)

clean_text <- function(text) {
    text %>%
    tolower %>%
    removeNumbers %>%
    removePunctuation %>%
    removeWords(stopwords) %>%
    lemmatize %>%
    # the corpus contains occurences of hangul character
    # hwalp - 홢 - this is unwanted
    str_remove_all(., "홢") %>%
    stripWhitespace
}
gr99 <- ft_raw[.groups == 99] %>% as.data.table

gr99_clean <- gr99[, text := sapply(
                            .SD[, text], clean_text),
             by = .I]

# set up parallel processing
library(parallel)
cluster <- makeForkCluster(nnodes = 3, outfile = "")
# the cluster needs to se e my stopwords
clusterExport(
        cl = cluster,
        varlist = c("ft_stopwords", "lemmatize", "clean_text"),
        envir = .GlobalEnv
)
#and needs to have the required libraries loaded
clusterEvalQ(
    cl = cluster, {
        library(tm)
        library(tidyverse)
        library(udpipe)
    }
)
gr99_clean <- gr99[, text := parSapply(
                            cluster, .SD[, text], clean_text),
             by = .I]

# apply the cleaning operation in parallel
# the dataset is already prepared in 100 batches
ft_speeches[, text := parSapply(
                            cluster, .SD[, text], clean_text),
            by = .groups]


# apply the cleaning operation in parallel
# the dataset is already prepared in 100 batches
ft_speeches[, text := parSapply(
                            cluster, .SD[, text], clean_text),
            by = .groups]

ft_clean <- ft_raw[, text := sapply(
                             .SD[, text], clean_text),
             by = .groups]

ft_lemmatized <- ft_clean[, text := sapply(
                             .SD[, text], lemmatize),
             by = .groups]
