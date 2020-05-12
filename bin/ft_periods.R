# ft_periods.R
# take danish parliamentary speech data
# and group them according to 
# - parliamentary periods
# - as maps to generalized trends in
#   Danish educational policy

# Periods:
# 53-57 (ny lærlingelov,vekseluddannelse)
# 57-68 (udvidelse af lærlingekonceptet)
# 68-78 (EFG)
# 78-90 (Haarder, U91, EUD/EUX)
# 90-00 (uddannelse til alle, markedsorientering, selvstyre)
# 00-14 (individualisering, ansvar for egen læring)
# 14-20 (fokus på unge; voksne falder fra)

library(here)
library(data.table)
library(tidyverse)

ft_speeches <- fread(here("data/ft_clean_lemmatized.csv"))

ft_speeches[, timeseries := sapply(Period, (function(x) {
                            year <- as.integer(str_extract(x, "\\d{4}$"))
                            case_when(
                                 year <= 1957 ~ "1953-57",
                                 year <= 1968 ~ "1957-68",
                                 year <= 1978 ~ "1968-78",
                                 year <= 1990 ~ "1978-90",
                                 year <= 2000 ~ "1990-00",
                                 year <= 2014 ~ "2000-14",
                                 year <= 2020 ~ "2014-20"
                            )}
                        ))]


