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
# 90-01 (uddannelse til alle, markedsorientering, selvstyre)
# 01-14 (individualisering, ansvar for egen læring)
# 14-20 (fokus på unge; voksne falder fra)

library(here)
library(data.table)
library(tidyverse)
library(future)

options(future.globals.maxSize= 89128960000)

ft_speeches <- fread(here("data/ft_clean_lemmatized.csv"))
ft_speeches[, timeseries := sapply(Date, (function(x) {
                            case_when(
                                 x < as.Date("1957-05-28") ~ "1953-57",
                                 x < as.Date("1968-02-02") ~ "1957-68",
                                 x < as.Date("1978-08-30") ~ "1968-78",
                                 x < as.Date("1990-12-18") ~ "1978-90",
                                 x < as.Date("2001-11-27") ~ "1990-01",
                                 x < as.Date("2014-02-03") ~ "2001-14",
                                 x > as.Date("2014-02-03") ~ "2014-20"
                            )}
                        ))]

