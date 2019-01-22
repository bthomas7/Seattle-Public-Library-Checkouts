########## Clear Environment  ###################
rm(list=ls())
#.rs.restartR()

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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                              MaterialType == 'EBOOK' ~ "EBook",
                              (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                              (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
  mutate(Material = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                         MaterialType == 'EBOOK' ~ "EBook",
                         (tolower(Subjects) %like% 'audiobook'|(tolower(Publisher) %like% 'audio'& UsageClass == 'Physical')|MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                         (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
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
checkouts_yearly_all <-  rbind(checkouts_06_yearly,checkouts_07_yearly,checkouts_08_yearly,checkouts_09_yearly,checkouts_10_yearly,checkouts_11_yearly,checkouts_12_yearly,checkouts_13_yearly,checkouts_14_yearly,checkouts_15_yearly,checkouts_16_yearly,checkouts_17_yearly,checkouts_18_yearly)

saveRDS(checkouts_monthly_all,'checkouts_monthly_all.RDS')
saveRDS(checkouts_yearly_all,'checkouts_yearly_all.RDS')

ggplot(checkouts_monthly_all, aes(fill=Material, y=MonthlyPct, x=CheckoutMonYr)) + 
  geom_bar( stat="identity") 

ggplot(checkouts_yearly_all, aes(fill=Material, y=YearlyTotal/1000, x=CheckoutYear)) + 
  geom_bar( stat="identity") 
#choose fill of material; choose digital, physical or both; monthly view

#"Music"     "EBook"     "Comic"     "Video"     "Book"   "Audiobook" "Magazine"  "Other"     "Visual"  

library(tm)
library(wordcloud)
library(RColorBrewer)

phys_books <- checkouts_18_category %>% 
  subset(UsageClass == 'Physical') %>% 
  subset(Material == 'Book')
phys_book_subjects <- unique(test$Subjects)
phys_book.corpus <- Corpus(VectorSource(test2))
phys_book.corpus <- tm_map(phys_book.corpus, removePunctuation)
phys_book.corpus <- tm_map(phys_book.corpus, content_transformer(tolower))
phys_book.corpus <- tm_map(phys_book.corpus, function(x) removeWords(x, stopwords("english")))
phys_book_tdm <- TermDocumentMatrix(phys_book.corpus)
phys_book_m <- as.matrix(phys_book_tdm)
phys_book_v <- sort(rowSums(phys_book_m),decreasing=TRUE)
phys_book_df <- data.frame(word = names(phys_book_v),freq=phys_book_v)




pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq,min.freq=50,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()


## round percent to 1/2 decimal, divide totals by 1000, assign particular colors to material types,
## select yearly vs monthly view --can i click on year and get monthly view?