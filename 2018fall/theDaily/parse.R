# --------------------------------------Documentation --------------------------------------------------
#
# Author:   Hannah Fresques
# Date:     November 5, 2018
# Project:  New School Data Embed
# Purpose:  Pull meta data on The Daily podcast
# 
#_______________________________________________________________________________________________________


# setup -------------------------------------------------------------------
library(dplyr)
library(here)
library(readr)

# query itunes api --------------------------------------------------------
# not where I ultimately got data from, but leaving it here incase it's useful in the future.

# library(downloader)
# library(jsonlite)
# download(
#   "https://itunes.apple.com/search?media=podcast&term=The+Daily&artistName=The+New+York+Times",
#   here("dailyQuery.json")
#   )
# 
# df <- read_json(here("dailyQuery.json")) 


# query RSS feed ----------------------------------------------------------

library(tidyRSS)
rssURL <- "http://rss.art19.com/the-daily"

df <- tidyfeed(rssURL)


# data checks -------------------------------------------------------------

df %>% nrow() # 465

library(lubridate)
df <- df %>% mutate(date=date(item_date_published))

df %>% select(date) %>% n_distinct() # 458 unique dates.

df %>% group_by(date) %>% filter(n()>1) %>% select(item_date_published,item_title,item_description) %>% View()
# all are actual separate episodes, not duplicates.

df %>% arrange(date) %>% select(item_date_published,item_title) %>% head(10)
# teaser podcast was on Jan 17, 2017, but first real episode was February 1, 2017

# check if any days are missing.
# first make list of all days within the time range
time_range <- df %>% pull(date)
all_days <- seq(mdy("2/1/2017"), max(time_range), by="day") %>% as_tibble() %>% 
  rename(date=value) %>% 
  mutate(weekday = !(wday(date, label = TRUE, abbr = FALSE) %in% c("Saturday","Sunday")))

noPod <- anti_join(all_days %>% filter(weekday),df, by="date")
noPod # 13 week days with no podcast published:
#    date       weekday
#  1 2017-02-20 TRUE   
#  2 2017-05-29 TRUE   
#  3 2017-07-03 TRUE   
#  4 2017-07-04 TRUE   
#  5 2017-09-04 TRUE   
#  6 2017-11-23 TRUE   
#  7 2017-11-24 TRUE   
#  8 2017-12-25 TRUE   
#  9 2018-01-01 TRUE   
# 10 2018-01-15 TRUE   
# 11 2018-05-28 TRUE   
# 12 2018-07-04 TRUE   
# 13 2018-09-03 TRUE   


# write to csv ------------------------------------------------------------

df %>% select(item_date_published,date,item_title,item_description,item_link) %>% write_csv("dailyQuery.csv")

