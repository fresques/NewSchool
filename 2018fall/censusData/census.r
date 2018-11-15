#______________________________________________________________________________
#
# Author:   Hannah Fresques
# Date:     November 15, 2018
# Purpose:  Clean up and combine census data for New School class
# 
#______________________________________________________________________________

library(dplyr)
library(readr)
library(here)
library(downloader)



# get data ----------------------------------------------------------------

# get county names
download("https://www2.census.gov/geo/docs/reference/codes/files/st36_ny_cou.txt","rawData/st36_ny_cou.txt")
names <- read_csv("rawData/st36_ny_cou.txt", col_names = FALSE)
head(names)

# working with census data for all counties in NY state
lang <- read_csv("rawData/aff_download/ACS_16_5YR_S1601_with_ann.csv", col_types = list(GEO.id2 = col_character()))
inc  <- read_csv("rawData/aff_download/ACS_16_5YR_S1901_with_ann.csv", col_types = list(GEO.id2 = col_character()))
pop  <- read_csv("rawData/aff_download/ACS_16_5YR_S0101_with_ann.csv", col_types = list(GEO.id2 = col_character()))
pop09<- read_csv("rawData/aff_download/ACS_09_5YR_S0101_with_ann.csv", col_types = list(GEO.id2 = col_character()))



# clean data --------------------------------------------------------------


# clean up language data
View(head(lang,20))

lang2 <- lang %>% 
  rename(
    pctEnglishO = HC04_EST_VC01,
    countyID = GEO.id2
  ) %>% 
  mutate(pctEnglish = as.numeric(pctEnglishO)) %>% 
  select(countyID,pctEnglish,pctEnglishO)

lang2 %>% filter(is.na(pctEnglishO)==F & is.na(pctEnglish)==T) %>% count(pctEnglish,pctEnglishO) # none, good.
summary(lang2)



# clean up income data
View(head(inc,20))

inc2 <- inc %>% 
  rename(
    median_incomeO = HC01_EST_VC13,
    mean_incomeO = HC01_EST_VC15,
    countyID = GEO.id2
  ) %>% 
  mutate(
    median_income = ifelse(median_incomeO=="250,000+",250000,as.numeric(median_incomeO)),
    mean_income   = ifelse(mean_incomeO  =="250,000+",250000,as.numeric(mean_incomeO  ))
  ) %>% 
  select(countyID,median_incomeO,median_income,mean_incomeO,mean_income)

inc2 %>% filter(is.na(median_incomeO)==F & is.na(median_income)==T) %>% count(median_incomeO,median_income) # none, good.
inc2 %>% filter(is.na(mean_incomeO  )==F & is.na(mean_income  )==T) %>% count(mean_incomeO  ,mean_income  ) # none, good.
summary(inc2)


# clean up population data
pop2 <- pop %>% 
  rename(
    popO = HC01_EST_VC01,
    countyID = GEO.id2
  ) %>% 
  mutate(pop16 = as.numeric(popO)) %>% 
  select(countyID,pop16,popO)


pop09_2 <- pop09 %>% 
  rename(
    popO = HC01_EST_VC01,
    countyID = GEO.id2
  ) %>% 
  mutate(pop09 = as.numeric(popO)) %>% 
  select(countyID,pop09,popO)

# combine files
counties <- pop2 %>% select(-popO) %>% 
  left_join(
    pop09_2 %>% select(-popO),
    by="countyID"
  ) %>% 
  left_join(
    inc2 %>% select(-median_incomeO,-mean_incomeO),
    by="countyID"
  ) %>% 
  left_join(
    lang2 %>% select(-pctEnglishO),
    by="countyID"
  )

# 62 counties. good.
head(counties)




# add on county names.
counties2 <- counties %>% 
  left_join(
    names %>% 
      mutate(countyID=paste0(X2,X3)) %>% 
      select(countyID,countyName=X4),
    by="countyID"
  ) %>% 
  select(countyID,countyName,pop09,pop16,everything())
head(counties2)


# limit to just NYC area counties
nycArea <- c("New York County","Bronx County","Kings County","Queens County","Richmond County",
             "Rockland County","Nassau County","Westchester County","Suffolk County")

counties3 <- counties2 %>% 
  filter(countyName %in% nycArea)

# save file ---------------------------------------------------------------

write_csv(counties3, "cleanCensus.csv")

