# ft_gofastr.R 
# try out the gofstr package fro rapid creation of DTM / TDM

library(here)
library(gofastr)
library(tm)
library(tidyverse)
library(data.table)

# load cleaned dataset into menory
ft_speeches <- fread(here("data/folketinget_cleaned.csv"))

ft_tdm <- q_tdm(ft_speeches$text, ft_speeches$doc_id)

ft_dtm <- q_dtm(ft_speeches$text, ft_speeches$doc_id)

ft_tf_idf <- ft_tdm %>%
    # filter_documents() %>%
    tm:: weightTfIdf()
