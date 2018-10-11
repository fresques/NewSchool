# --------------------------------------Documentation --------------------------------------------------
#
# Author:   Hannah Fresques
# Date:     October 11, 2018
# Project:  New School Data Embed 
# Purpose:  Filter and join data of politician tweets and put it in google sheets so students can use it.
#           See: https://data.world/bkey/politician-tweets
#_______________________________________________________________________________________________________

library(readr)
library(dplyr)
library(stringr)

pols  <- read_delim(
  "https://query.data.world/s/atzo3t7kailko2effmzhrpegz7vne6", 
  delim = ";"
  )

tweets <- read_delim(
  "https://query.data.world/s/fw3eflunuaay36uhe223ldlmmogjoe", 
  delim = ";"
  )


# look at pols ------------------------------------------------------------

pols %>% nrow() # 668
pols %>% select(id) %>% n_distinct() # 668

# create flags for branch of government and party
pols <- pols %>% 
  mutate(
    republican     = str_detect(tolower(array_agg),"republican"    ),
    democrat       = str_detect(tolower(array_agg),"democrat"      ),
    independent    = str_detect(tolower(array_agg),"independent"   ),
    senator        = str_detect(tolower(array_agg),"senator"       ),
    congressperson = str_detect(tolower(array_agg),"congressperson"),
    governor       = str_detect(tolower(array_agg),"governor"      )
  )

pols %>% count(republican,democrat,independent)
# fyi the house currently has 435 representatives

pols %>% count(senator,congressperson,governor)

pols %>% filter(!senator & !congressperson & !governor) %>% View()
# let's exclude these people. Seems to be a random collection of folks.



# look at tweets ----------------------------------------------------------

tweets %>% head(100) %>% View()

tweets %>% nrow() # 1,661,553
tweets %>% select(id) %>% n_distinct() # 1,661,553
tweets %>% select(user_id) %>% n_distinct() # 664

# what dates are covered?
tweets %>% select(created_at) %>% summary()
library(lubridate)
tweets %>% mutate(year = year(created_at)) %>% count(year)
tweets %>% filter(created_at > dmy("1/1/2017")) %>% mutate(month = month(created_at)) %>% count(month)
# mostly seems to be 2013 thru April 2017.

activity_by_month <- tweets %>% 
  mutate(
    year = year(created_at),
    month = month(created_at)
  ) %>% 
  group_by(year,month) %>% 
  summarise(
    n_tweets = n(),
    n_accounts = n_distinct(user_id)
  )

library(ggplot2)
activity_by_month %>% 
  filter(year>2013) %>% 
  ggplot(aes(x=paste(year,month), y=n_accounts)) +
  geom_col()
# number of accounts captured in the tweets data remains pretty constant in the later years.


activity_by_day <- tweets %>% 
  mutate(
    date = date(created_at)
  ) %>% 
  group_by(date) %>% 
  summarise(
    n_tweets = n(),
    n_accounts = n_distinct(user_id),
    rt_max = max(retweet_count),
    rt_sum = sum(retweet_count)
  )
# innauguration was a big day on twitter...




# make file for students --------------------------------------------------

# cut tweets to a small time period so the table is a more reasonable size.
# just do jan 20 2017
tweets_cut <- tweets %>% 
  filter(date(created_at)==dmy("20/1/2017"))
# 2601 tweets

tweets_cut <- tweets_cut %>%
  rename(
    tweet_created_at = created_at,
    tweet_id = id
  ) %>% 
  left_join(
    pols %>% 
      rename(
        accnt_created_at = created_at,
        user_id = id
      ),
    by="user_id")

pols %>% count(is_verified)
# is_verified is never NA on the pols list

tweets_cut %>% count(is_verified)
# good. every tweet had a match in the pols table.

tweets_cut %>% count(democrat,republican,independent)
# good, everyone has one and only one party.

tweets_cut %>% count(senator,congressperson,governor)
# still some non-officials in here, who I'll cut. (2160 tweets)
# also want to categorize the ppl flagged both as senator and congressperson.

tweets_cut %>% filter(senator & congressperson) %>% 
  select(user_id,screen_name,description) %>% distinct() %>% View()
# no dual congress/senator accounts tweeted on this day.
# Other days, it includes Todd Young, Tammy Duckworth, Chris Van Hollen
# All three made the switch from representative to senator Jan 3 2017
# so for the time period in our tweets_cut, they're senators.

tweets_cut2 <- tweets_cut %>% 
  filter(senator | congressperson | governor) %>% 
  # turn binary switches for office and party into categorical var
  mutate(
    office = case_when(
      senator ~ "senator",
      congressperson ~ "congressperson",
      governor ~ "govenor",
      TRUE ~ "other"
    ),
    party = case_when(
      democrat ~ "democrat",
      republican ~ "republican",
      independent ~ "independent",
      TRUE ~ "other"
    ),
    engagement = (favorites_count + retweet_count) / latest_followers_count
  )

tweets_cut2 %>% count(senator,congressperson,governor,office)
tweets_cut2 %>% count(democrat,republican,independent,party)

# noooooo. data doesn't have replies, so can't calculate the ratio :(
# what does tweet engagement look like?
tweets_cut2 %>% 
  select(latest_followers_count,favorites_count,retweet_count,engagement) %>% 
  summary()
# what are the surprisingly engaging tweets?
tweets_cut2 %>% filter(engagement > 3) %>% View()

# who are the ppl w lots of followers?
tweets_cut2 %>% filter(latest_followers_count>1000000) %>% 
  select(user_id,screen_name,latest_followers_count) %>% 
  distinct()
# basically who you'd expect
# also worth noting that not everyone in the dataset is in office as of Jan 2017
# e.g. Boehner is still in there.

# correlation between followers and favorites?
tweets_cut2 %>% 
  ggplot(aes(x=latest_followers_count,y=favorites_count)) +
  geom_point() +
  scale_x_log10()
# somewhat

# what about followers and retweets?
tweets_cut2 %>% 
  ggplot(aes(x=latest_followers_count,y=retweet_count)) +
  geom_point() +
  scale_x_log10()
# less than with favorites.



# this is still too many tweets. let's do just senators.
tweets_cut3 <- tweets_cut2 %>% 
  filter(office=="senator")



# export for google sheets ---------------------------------------------------

# lol can't use googlesheets package anymore now that I turned on advanced protection.
write_csv(
  tweets_cut2 %>% 
  select(
    screen_name,description,latest_followers_count,office,party,
    tweet_created_at,tweet_text,favorites_count,retweet_count
    ) ,
  "politician-tweets-cleaned.csv"
)

  
  
