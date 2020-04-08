# ft_lda.R
# perform Latent Dirichlet Analysis on scraped speeches from the floor of
# the Danish parliament
# http://gitlab.com/grwatz/folketinget
library(rvw)
library(tidyverse)
library(tidytext)
library(here)
library(parallel)
library(data.table)
# options(error = recover)
options(datatable.verbose = TRUE)
cat("| Loading speeches from the Danish parliament\n")
ft_speeches <- fread(here("data/folketinget_1953_2019_raw.csv"))
# pb <- txtProgressBar(min = 0, max = length(unique(ft_speeches$.group)), style = 2)
cluster <- makeForkCluster(nnodes = 4, outfile = "")

# number of unique documents,
# set up a progress bar the size of our data set
#
# build vocabulary
cat("| Constructing vocabulary\n")
ft_vocab <- ft_speeches[, sort(unique(unlist(strsplit(text, " "))))]

# https://github.com/rvw-org/rvw/wiki/Topic-modeling-with-Latent-Dirichlet-Allocation-(LDA)
# Add hashes to document corpus
clusterExport(cl = cluster, varlist = c("ft_vocab"), envir = environment())
add_features <- function(x) {
    split_words <- unlist(strsplit(x, " "))
    counted_words <- aggregate(
        data.frame(count = split_words),
        list(word = split_words),
        length
    )
    res_str <- paste0(apply(counted_words, 1, function(x) {
        paste0(
            (which(ft_vocab == x[["word"]]) - 1),
            ":",
            as.numeric(x[["count"]])
        )
    }),
    collapse = " "
    )
    res_str
}
cat("| Adding features to speeches\n")
ft_speeches[, features := parSapply(
                            cluster, .SD[, text], add_features),
            by = .groups]
# close(pb)
fwrite(ft_speeches, file = here("data/folketinget_features.csv"))

ft_features <- fread(here("data/folketinget_features.csv"))
# required number of bits for range of feature hashes
bits <- ceiling(log2(length(ft_vocab)))
num_docs <- 800170
# set up our training model
# TODO: which vw features would be relevant to set?
#       num_topics should probably be higher than 7, for instance
ft_model <- vwsetup(
    feature_params = list(bit_precision = bits),
    optimization_params = list(initial_t = 1, power_t = 0.5), # Parameters for learning rate schedule
    option = "lda", # Enable LDA algorithm
    num_topics = 30, # Specify the number of topics to learn (the same as were manually classified)
    lda_D = num_docs,
    minibatch = 16 # Analyze 16 documents at a time
)



# train our model
vwtrain(
    vwmodel = ft_model,
    data = ft_features,
    namespaces = list(" " = "features"),
    fixed = "features"
)

vwout <- vwaudit(vwmodel = ft_model)
head(vwout)
