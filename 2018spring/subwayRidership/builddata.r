# subway ridership by station and year

library(dplyr)
library(stringr)
library(downloader)
library(rstudioapi)
library(tidyr)
library(purrr)
library(readr)
library(rvest)

rootDir <- getActiveDocumentContext()$path %>% dirname()
setwd(rootDir)

ridershipURL <- "http://web.mta.info/nyct/facts/ridership/ridership_sub.htm"
download(ridershipURL, destfile = "./ridership.html")
htmlTableOG <- readLines("./ridership.html", skipNul = FALSE, warn=FALSE)

htmlTableOG %>% head(20)
htmlTableOG %>% tail(20)

htmlTable <- htmlTableOG[265:5460]

htmlTable %>% head(20)
htmlTable %>% tail(20)


# better way to pull out the alt text, but I couldn't quite get it to work.
    # altTxt <- htmlTable2[i] %>% 
      # read_html() %>% 
      # html_nodes("img") %>%
      # html_attr("alt")

# pull out just the alt text
altTxt <- htmlTable %>% str_extract_all(regex('alt="[0-9a-zA-Z] subway')) 
# terrible hack, there must be a cleaner way :(
altTxt2 <- altTxt
for(i in 1:length(altTxt2)){
  if(length(altTxt2[[i]])==0){
    altTxt2[[i]] <- NA
  }
  altTxt2[[i]] <- altTxt2[[i]] %>% str_c(collapse=",")
}
altTxt3 <- altTxt2 %>% unlist()
altTxt4 <- altTxt3 %>% str_replace_all("\\p{quotation mark}","") %>% str_replace_all("alt=" ,'')

altCell <- ifelse(is.na(altTxt4)==F,paste0("<td>",altTxt4,"</td>"),"")
# put in alt text in as a cell after the last cell. 
htmlTable2 <- htmlTable
for(i in 1:length(htmlTable2)){
  htmlTable2[i] <- htmlTable[i] %>% paste0(altCell[i])
}

htmlTable2[c(34)]
  
write_lines(htmlTable2,"cleanTable.html")
mytable <- "cleanTable.html" %>%
  read_html() %>%
  html_table(fill=TRUE) %>% 
  .[[1]]

# stash table names
names <- colnames(mytable)
colnames(mytable) <- paste0("a",1:ncol(mytable))

# row order is important to preserve while cleaning
# add row numbers toot suite
mytable <- mytable %>% mutate(row=row_number())

# look at the table
mytable %>% sample_n(50) %>% View()
mytable %>% tail(20) %>% View()

# split into summary table and main table
sumtable <- mytable[429:434,]
mytable  <- mytable[  1:427,]

colnames(sumtable) <- c("Borough",names[2:11])
sumtable <- sumtable[,1:9]

names2 <- c("Station","Lines",paste0("r",2011:2016),"Change1516","PctChange1516","Rank16","row")
colnames(mytable) <- names2


mytable %>% head(200) %>% View()
mytable %>% tail(40) %>% View()

# copy borough down.
boroughs <- c("Bronx","The Bronx","Brooklyn","Manhattan","Queens","Staten Island")
mytable %>% filter(Station %in% boroughs) 
key <- mytable %>% filter(Station %in% boroughs) %>% select(row,Station)
mybreaks <- c(-Inf,key$row,Inf)
mylabels <- c("Error",key$Station)
mytable <- mytable %>% 
  mutate(borough = as.character(cut(row,mybreaks,labels=mylabels,right=FALSE)))

mytable[1:5    ,c("Station","r2011","r2015","r2016","borough")]
mytable[68:74  ,c("Station","r2011","r2015","r2016","borough")]
mytable[226:234,c("Station","r2011","r2015","r2016","borough")]
mytable[346:352,c("Station","r2011","r2015","r2016","borough")]
# rows I expect to drop: 5
mytable2 <- mytable %>% 
  filter(str_trim(Station)!="" & !(Station %in% boroughs)) 

# clean up station names and line names
mytable2 <- mytable2 %>% 
  rename(LinesO=Lines,StationO=Station) 
mytable2 <- mytable2 %>% 
  mutate(
    Lines=LinesO %>% str_replace_all('alt="',"") %>% 
      str_replace_all(" subway","") %>% 
      str_replace_all("Â",""),
    Station=StationO %>% 
      str_replace_all("Â","")
  )

# check that we have all lines
lines <- mytable2 %>%
  select(borough,Station,Lines) %>% 
  mutate(Lines=strsplit(as.character(Lines), ",")) %>% 
  unnest(Lines)
chk <- lines %>% count(Lines) 
fd <- chk %>% filter(nchar(Lines)>1) # messed up - none! yay!
chk %>% filter(nchar(Lines)==1) %>% View() # has all the real lines. good.

# make final table
mytable3 <- mytable2 %>% select(borough,Station,Lines,r2011:r2016)

# clean up the environment
rm(list=setdiff(ls(), c("mytable3","sumtable")))

# send to google sheets
library(googlesheets)
# gs_auth(new_user = TRUE)
# doesn't put things in the right folder but I'll live with that for now.
# mysheet <- gs_new("ridership", ws_title = "byStation", input = mytable3)
mysheet <- gs_title("ridership")
mysheet <- mysheet %>% 
  gs_ws_new(ws_title = "summary", input = sumtable[,1:7])
















