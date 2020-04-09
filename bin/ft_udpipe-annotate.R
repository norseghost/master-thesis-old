# ft_udpipe-annotate.R
# annotate scraped speeches from the floor of
# the Danish parliament with udpipe
# http://gitlab.com/grwatz/folketinget
library(rvw)
library(tidyverse)
library(tidytext)
library(here)
library(parallel)
library(data.table)
library(udpipe)
#options(error = recover)
cat("| Downloading danish udpipe model")
udpipe_model <- udpipe_download_model("danish")
cat("| Loading speeches from the Danish parliament\n")
ft_speeches <- fread(here("data/folketinget_1953_2019_raw.csv"))
cat("| Annotating speeches\n")
pb <- txtProgressBar(min = 0, max = length(unique(ft_speeches$.group)), style = 2)
# use one less than all available physical cores
for (x in unique(ft_speeches$.groups)) {
    setTxtProgressBar(pb, x)
    # cat("[ ] processing group", x, "\n")
    ft_annotation <- udpipe(
        ft_speeches[.groups == x, .(doc_id, text)],
        udpipe_model$file_model,
        parallel.cores = 4,
        trace = 0
    )
    saveRDS(ft_annotation, file = here(paste0("data/folketinget_annotation_", x, ".rds")))
}
close(pb)

# This doens't work - data.table complains about uneven types in the result
# ft_annotation <- ft_speeches[, {
#         udpipe(
#             .SD,
#             udpipe_model$file_model,
#             parallel.cores = 4,
#             trace = 1
#         )
#          setTxtProgressBar(pb, .GRP)
#     },
#      by = .groups,
#      .SDcols = c("text", "doc_id")]

