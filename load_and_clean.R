########## Clear Environment  ###################
rm(list=ls())
.rs.restartR()

### set working directory###
setwd("~/NSS/nss_data_science/Seattle-Public-Library-Checkouts")

### use packages ###
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(data.table)
  
################ Load, Categorize, & Summarize Data for each year ################
  
### 2006
checkouts_06 <- read_csv('data/Checkouts_by_Title_2006.csv')
  
checkouts_06_category <- checkouts_06 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))
  
checkouts_06_monthly <- checkouts_06_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))
  
checkouts_06_yearly <- checkouts_06_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

  
### 2007
checkouts_07 <- read_csv('data/Checkouts_by_Title_2007.csv')

checkouts_07_category <- checkouts_07 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_07_monthly <- checkouts_07_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_07_yearly <- checkouts_07_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2008
checkouts_08 <- read_csv('data/Checkouts_by_Title_2008.csv')

checkouts_08_category <- checkouts_08 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_08_monthly <- checkouts_08_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_08_yearly <- checkouts_08_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2009
checkouts_09 <- read_csv('data/Checkouts_by_Title_2009.csv')

checkouts_09_category <- checkouts_09 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_09_monthly <- checkouts_09_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_09_yearly <- checkouts_09_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2010
checkouts_10 <- read_csv('data/Checkouts_by_Title_2010.csv')

checkouts_10_category <- checkouts_10 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_10_monthly <- checkouts_10_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_10_yearly <- checkouts_10_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2011
checkouts_11 <- read_csv('data/Checkouts_by_Title_2011.csv')

checkouts_11_category <- checkouts_11 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_11_monthly <- checkouts_11_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_11_yearly <- checkouts_11_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2012
checkouts_12 <- read_csv('data/Checkouts_by_Title_2012.csv')

checkouts_12_category <- checkouts_12 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_12_monthly <- checkouts_12_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_12_yearly <- checkouts_12_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2013
checkouts_13 <- read_csv('data/Checkouts_by_Title_2013.csv')

checkouts_13_category <- checkouts_13 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_13_monthly <- checkouts_13_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_13_yearly <- checkouts_13_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2014
checkouts_14 <- read_csv('data/Checkouts_by_Title_2014.csv')

checkouts_14_category <- checkouts_14 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_14_monthly <- checkouts_14_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_14_yearly <- checkouts_14_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2015
checkouts_15 <- read_csv('data/Checkouts_by_Title_2015.csv')

checkouts_15_category <- checkouts_15 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_15_monthly <- checkouts_15_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_15_yearly <- checkouts_15_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2016
checkouts_16 <- read_csv('data/Checkouts_by_Title_2016.csv')

checkouts_16_category <- checkouts_16 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_16_monthly <- checkouts_16_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_16_yearly <- checkouts_16_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2017
checkouts_17 <- read_csv('data/Checkouts_by_Title_2017.csv')

checkouts_17_category <- checkouts_17 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                              MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                              MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                              MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                              MaterialType == 'MAGAZINE' ~ "Magazine",
                              TRUE ~ "Other"))

checkouts_17_monthly <- checkouts_17_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_17_yearly <- checkouts_17_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))

### 2018
checkouts_18 <- read_csv('data/Checkouts_by_Title_2018.csv')

checkouts_18_category <- checkouts_18 %>% 
  mutate(Material = case_when((tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                         MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                         MaterialType == 'EBOOK' ~ "EBook",
                         (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                         MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                         MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                         MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                         MaterialType == 'MAGAZINE' ~ "Magazine",
                         TRUE ~ "Other"))

checkouts_18_monthly <- checkouts_18_category %>% 
  group_by(CheckoutYear, CheckoutMonth,UsageClass,Material) %>% 
  summarise(MatMthlyCheckouts = sum(Checkouts)) %>% 
  mutate(CheckoutMonYr = sprintf("%s-%02d",CheckoutYear, CheckoutMonth)) %>% 
  group_by(CheckoutMonYr) %>% 
  mutate(MonthlyTotal = sum(MatMthlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,CheckoutMonYr,UsageClass,Material,MatMthlyCheckouts,MonthlyTotal) %>% 
  mutate(MonthlyPct = 100*(MatMthlyCheckouts/MonthlyTotal))

checkouts_18_yearly <- checkouts_18_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = 100*(MatYrlyCheckouts/YearlyTotal))


## wordcloud subjects by == digital vs physical for audio, ebook, book

checkouts_monthly_all <-  rbind(checkouts_06_monthly,checkouts_07_monthly,checkouts_08_monthly,checkouts_09_monthly,checkouts_10_monthly,checkouts_11_monthly,checkouts_12_monthly,checkouts_13_monthly,checkouts_14_monthly,checkouts_15_monthly,checkouts_16_monthly,checkouts_17_monthly,checkouts_18_monthly)
#checkouts_monthly_all <- checkouts_monthly_all %>% 
#  select

checkouts_yearly_all <-  rbind(checkouts_06_yearly,checkouts_07_yearly,checkouts_08_yearly,checkouts_09_yearly,checkouts_10_yearly,checkouts_11_yearly,checkouts_12_yearly,checkouts_13_yearly,checkouts_14_yearly,checkouts_15_yearly,checkouts_16_yearly,checkouts_17_yearly,checkouts_18_yearly)

saveRDS(checkouts_monthly_all,'checkouts_monthly_all.RDS')
saveRDS(checkouts_yearly_all,'checkouts_yearly_all.RDS')

ggplot(checkouts_monthly_all, aes(fill=Material, y=MonthlyPct, x=CheckoutMonYr)) + 
  geom_bar( stat="identity") 

ggplot(checkouts_yearly_all, aes(fill=Material, y=YearlyTotal/1000, x=CheckoutYear)) + 
  geom_bar( stat="identity") 
#choose fill of material; choose digital, physical or both; monthly view

# "EBook"   "Book"  phys & digital "Audiobook"   "Other"       

library(tm)
library(wordcloud)
library(RColorBrewer)

dig_ebooks_18 <- checkouts_18_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_18_subjects <- unique(dig_ebooks_18$Subjects)
dig_ebooks_18.corpus <- Corpus(VectorSource(dig_ebooks_18_subjects))
dig_ebooks_18.corpus <- tm_map(dig_ebooks_18.corpus, removePunctuation)
dig_ebooks_18.corpus <- tm_map(dig_ebooks_18.corpus, content_transformer(tolower))
dig_ebooks_18.corpus <- tm_map(dig_ebooks_18.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_18_tdm <- TermDocumentMatrix(dig_ebooks_18.corpus)
dig_ebooks_18_m <- as.matrix(dig_ebooks_18_tdm)
dig_ebooks_18_v <- sort(rowSums(dig_ebooks_18_m),decreasing=TRUE)
dig_ebooks_18_df <- data.frame(Year = '2018',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_18_v),freq=dig_ebooks_18_v)

phys_books_18 <- checkouts_18_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_18_subjects <- unique(phys_books_18$Subjects)
phys_books_18.corpus <- Corpus(VectorSource(phys_books_18_subjects))
phys_books_18.corpus <- tm_map(phys_books_18.corpus, removePunctuation)
phys_books_18.corpus <- tm_map(phys_books_18.corpus, content_transformer(tolower))
phys_books_18.corpus <- tm_map(phys_books_18.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_18_tdm <- TermDocumentMatrix(phys_books_18.corpus)
phys_books_18_m <- as.matrix(phys_books_18_tdm)
phys_books_18_v <- sort(rowSums(phys_books_18_m),decreasing=TRUE)
phys_books_18_df <- data.frame(Year = '2018',UsageClass = 'Physical',Material='Book',word = names(phys_books_18_v),freq=phys_books_18_v)

phys_audiobooks_18 <- checkouts_18_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_18_subjects <- unique(phys_audiobooks_18$Subjects)
phys_audiobooks_18.corpus <- Corpus(VectorSource(phys_audiobooks_18_subjects))
phys_audiobooks_18.corpus <- tm_map(phys_audiobooks_18.corpus, removePunctuation)
phys_audiobooks_18.corpus <- tm_map(phys_audiobooks_18.corpus, content_transformer(tolower))
phys_audiobooks_18.corpus <- tm_map(phys_audiobooks_18.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_18_tdm <- TermDocumentMatrix(phys_audiobooks_18.corpus)
phys_audiobooks_18_m <- as.matrix(phys_audiobooks_18_tdm)
phys_audiobooks_18_v <- sort(rowSums(phys_audiobooks_18_m),decreasing=TRUE)
phys_audiobooks_18_df <- data.frame(Year = '2018',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_18_v),freq=phys_audiobooks_18_v)

dig_audiobooks_18 <- checkouts_18_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_18_subjects <- unique(dig_audiobooks_18$Subjects)
dig_audiobooks_18.corpus <- Corpus(VectorSource(dig_audiobooks_18_subjects))
dig_audiobooks_18.corpus <- tm_map(dig_audiobooks_18.corpus, removePunctuation)
dig_audiobooks_18.corpus <- tm_map(dig_audiobooks_18.corpus, content_transformer(tolower))
dig_audiobooks_18.corpus <- tm_map(dig_audiobooks_18.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_18_tdm <- TermDocumentMatrix(dig_audiobooks_18.corpus)
dig_audiobooks_18_m <- as.matrix(dig_audiobooks_18_tdm)
dig_audiobooks_18_v <- sort(rowSums(dig_audiobooks_18_m),decreasing=TRUE)
dig_audiobooks_18_df <- data.frame(Year = '2018',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_18_v),freq=dig_audiobooks_18_v)


top_checkouts_2018 <- rbind(dig_ebooks_18,dig_audiobooks_18,phys_books_18,phys_audiobooks_18)


top_checkouts_subjects_2018 <- rbind(dig_ebooks_18_df,dig_audiobooks_18_df,phys_books_18_df,phys_audiobooks_18_df)

saveRDS(top_checkouts_2018,'top_checkouts_2018.RDS')
saveRDS(top_checkouts_subjects_2018,'top_checkouts_subjects_2018.RDS')


com <- top_checkouts_2018 %>% 
  subset(Subjects %like% 'comic')

pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(phys_other_18_df$word,phys_other_18_df$freq,min.freq=10,max.words=100, random.order=T, rot.per=.15,  vfont=c("sans serif","plain"))

dev.off()


## round percent to 1/2 decimal, divide totals by 1000, assign particular colors to material types,
## select yearly vs monthly view --can i click on year and get monthly view?
## take out fiction/nonfiction/audiobooks/literature? from final wordcloud dataframe
## change to wordcloud2 (finish--need to update parameters)
## add table of top titles based on wordcloud choices
## only give book/audiobook if physical selected for word cloud and ebook/audiobook if digital selected