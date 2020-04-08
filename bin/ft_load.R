# ft_load.R
# load UDpipe parsed speeches from the floor of
# the Danish parliament
# http://gitlab.com/grwatz/folketinget
library(tidyverse)
library(tidytext)
library(here)
library(parallel)
library(data.table)
library(udpipe)
# udpipe_model <- udpipe_download_model("danish")

# pb <- txtProgressBar(min = 0, max = length(unique(ft_speeches$.group)), style = 3)

# TODO: rename data files
# TODO: the annotations are *much* too large for my available RAM
#       (~250MB x 100 = ~25 GB)
#       Barring building a ML workstation, I need to workshop
#       chunkwize operation
ft_anno_files <- list.files(path = here("data/"), pattern = "rds.csv", full.names = TRUE)
ft_anno_list <- lapply(ft_anno_files, fread)
ft_annotation <- rbindlist(ft_anno_list)
for (x in ft_anno_files) {
    anno <- fread(x)
    fwrite(anno, file = paste0(x, ".csv"))
}

