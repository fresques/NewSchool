###############################################################################################
#
# Author: Hannah Fresques
# Date: December 11, 2017
# Purpose: Summarize restaurant inspection data from NYC Department of Health and Mental Hygine.
#
###############################################################################################

library(dplyr)
library(readr)


# read data from api
insp <- read_csv(url('https://data.cityofnewyork.us/resource/9w7m-hzhe.csv?$limit=1000000'))

# check problems importing
problems(insp) %>% head()
problems(insp) %>% select(col) %>% table(useNA = "always")
# only problems were with the zip code column. 
# I don't care about that column so I'll just plow ahead.

insp %>% head(100) %>% View()

insp %>% colnames()
insp %>% nrow()

insp %>% select(camis) %>% unique() %>% nrow()
insp %>% select(camis,dba) %>% unique() %>% nrow()
# camis is a restaurant ID

insp %>% select(camis, inspection_date) %>% unique() %>% nrow()
insp %>% select(camis, inspection_date,violation_code) %>% unique() %>% nrow()
# that combination almost uniquely idetifies rows, but not quite.

# we're only interested in grades, not each violation, so cut down the data
insp2 <- insp %>% select(camis,dba,boro,cuisine_description,grade,grade_date) %>% unique()


# only keep the latest grade for each restaurant.
insp3 <- insp2 %>% 
  group_by(camis) %>%
  slice(which.max(grade_date)) %>%
  as.data.frame()
insp %>% filter(is.na(grade_date)==F) %>% select(camis, dba) %>% unique() %>% nrow()
insp3 %>% nrow()
# yes, expected numbers of rows were kept.

# what dates are covered?
insp3 %>% select(grade_date) %>% summary() 

# for simplcity, limit to restaurants inspected in 2017.
insp4 <- insp3 %>%
  filter(grade_date >= as.POSIXlt("2017/1/1 00:00:00") & boro != "Missing") %>%
  rename(cuisine=cuisine_description)
insp4 %>% select(grade_date) %>% summary() 

# great.
insp4 %>% select(grade) %>% table()
# per data dictionary, Z means grade pending.

# summarize by boro
boro <- insp4 %>%
  group_by(boro) %>%
  summarize(
    n=n(),
    nBad = sum(grade!="A")
  ) %>%
  mutate(
    pctBad = nBad/n
  )

# summarize by cuisine
cuis <- insp4 %>%
  group_by(cuisine) %>%
  summarize(
    n=n(),
    nBad = sum(grade!="A")
  ) %>%
  mutate(
    pctBad = nBad/n
  ) %>%
  arrange(-n)

cuis %>% View()
cuis %>% summary()

# save files
# there must be a get the location of a script but I haven't found it.
# rootDir <- "yourFilePathHere"
write.csv(boro, paste0(rootDir,"boro.csv"), row.names = FALSE)
write.csv(cuis, paste0(rootDir,"cuis.csv"), row.names = FALSE)








