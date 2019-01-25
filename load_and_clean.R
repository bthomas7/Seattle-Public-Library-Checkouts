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
library(tm)
library(wordcloud)
library(RColorBrewer)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_06_yearly <- checkouts_06_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_06 <- checkouts_06_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_06_subjects <- unique(dig_ebooks_06$Subjects)
dig_ebooks_06.corpus <- Corpus(VectorSource(dig_ebooks_06_subjects))
dig_ebooks_06.corpus <- tm_map(dig_ebooks_06.corpus, removePunctuation)
dig_ebooks_06.corpus <- tm_map(dig_ebooks_06.corpus, content_transformer(tolower))
dig_ebooks_06.corpus <- tm_map(dig_ebooks_06.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_06_tdm <- TermDocumentMatrix(dig_ebooks_06.corpus)
dig_ebooks_06_m <- as.matrix(dig_ebooks_06_tdm)
dig_ebooks_06_v <- sort(rowSums(dig_ebooks_06_m),decreasing=TRUE)
dig_ebooks_06_df <- data.frame(Year = '2006',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_06_v),freq=dig_ebooks_06_v)

phys_books_06 <- checkouts_06_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_06_subjects <- unique(phys_books_06$Subjects)
phys_books_06.corpus <- Corpus(VectorSource(phys_books_06_subjects))
phys_books_06.corpus <- tm_map(phys_books_06.corpus, removePunctuation)
phys_books_06.corpus <- tm_map(phys_books_06.corpus, content_transformer(tolower))
phys_books_06.corpus <- tm_map(phys_books_06.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_06_tdm <- TermDocumentMatrix(phys_books_06.corpus)
phys_books_06_m <- as.matrix(phys_books_06_tdm)
phys_books_06_v <- sort(rowSums(phys_books_06_m),decreasing=TRUE)
phys_books_06_df <- data.frame(Year = '2006',UsageClass = 'Physical',Material='Book',word = names(phys_books_06_v),freq=phys_books_06_v)

phys_audiobooks_06 <- checkouts_06_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_06_subjects <- unique(phys_audiobooks_06$Subjects)
phys_audiobooks_06.corpus <- Corpus(VectorSource(phys_audiobooks_06_subjects))
phys_audiobooks_06.corpus <- tm_map(phys_audiobooks_06.corpus, removePunctuation)
phys_audiobooks_06.corpus <- tm_map(phys_audiobooks_06.corpus, content_transformer(tolower))
phys_audiobooks_06.corpus <- tm_map(phys_audiobooks_06.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_06_tdm <- TermDocumentMatrix(phys_audiobooks_06.corpus)
phys_audiobooks_06_m <- as.matrix(phys_audiobooks_06_tdm)
phys_audiobooks_06_v <- sort(rowSums(phys_audiobooks_06_m),decreasing=TRUE)
phys_audiobooks_06_df <- data.frame(Year = '2006',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_06_v),freq=phys_audiobooks_06_v)

dig_audiobooks_06 <- checkouts_06_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_06_subjects <- unique(dig_audiobooks_06$Subjects)
dig_audiobooks_06.corpus <- Corpus(VectorSource(dig_audiobooks_06_subjects))
dig_audiobooks_06.corpus <- tm_map(dig_audiobooks_06.corpus, removePunctuation)
dig_audiobooks_06.corpus <- tm_map(dig_audiobooks_06.corpus, content_transformer(tolower))
dig_audiobooks_06.corpus <- tm_map(dig_audiobooks_06.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_06_tdm <- TermDocumentMatrix(dig_audiobooks_06.corpus)
dig_audiobooks_06_m <- as.matrix(dig_audiobooks_06_tdm)
dig_audiobooks_06_v <- sort(rowSums(dig_audiobooks_06_m),decreasing=TRUE)
dig_audiobooks_06_df <- data.frame(Year = '2006',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_06_v),freq=dig_audiobooks_06_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_07_yearly <- checkouts_07_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_07 <- checkouts_07_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_07_subjects <- unique(dig_ebooks_07$Subjects)
dig_ebooks_07.corpus <- Corpus(VectorSource(dig_ebooks_07_subjects))
dig_ebooks_07.corpus <- tm_map(dig_ebooks_07.corpus, removePunctuation)
dig_ebooks_07.corpus <- tm_map(dig_ebooks_07.corpus, content_transformer(tolower))
dig_ebooks_07.corpus <- tm_map(dig_ebooks_07.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_07_tdm <- TermDocumentMatrix(dig_ebooks_07.corpus)
dig_ebooks_07_m <- as.matrix(dig_ebooks_07_tdm)
dig_ebooks_07_v <- sort(rowSums(dig_ebooks_07_m),decreasing=TRUE)
dig_ebooks_07_df <- data.frame(Year = '2007',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_07_v),freq=dig_ebooks_07_v)

phys_books_07 <- checkouts_07_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_07_subjects <- unique(phys_books_07$Subjects)
phys_books_07.corpus <- Corpus(VectorSource(phys_books_07_subjects))
phys_books_07.corpus <- tm_map(phys_books_07.corpus, removePunctuation)
phys_books_07.corpus <- tm_map(phys_books_07.corpus, content_transformer(tolower))
phys_books_07.corpus <- tm_map(phys_books_07.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_07_tdm <- TermDocumentMatrix(phys_books_07.corpus)
phys_books_07_m <- as.matrix(phys_books_07_tdm)
phys_books_07_v <- sort(rowSums(phys_books_07_m),decreasing=TRUE)
phys_books_07_df <- data.frame(Year = '2007',UsageClass = 'Physical',Material='Book',word = names(phys_books_07_v),freq=phys_books_07_v)

phys_audiobooks_07 <- checkouts_07_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_07_subjects <- unique(phys_audiobooks_07$Subjects)
phys_audiobooks_07.corpus <- Corpus(VectorSource(phys_audiobooks_07_subjects))
phys_audiobooks_07.corpus <- tm_map(phys_audiobooks_07.corpus, removePunctuation)
phys_audiobooks_07.corpus <- tm_map(phys_audiobooks_07.corpus, content_transformer(tolower))
phys_audiobooks_07.corpus <- tm_map(phys_audiobooks_07.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_07_tdm <- TermDocumentMatrix(phys_audiobooks_07.corpus)
phys_audiobooks_07_m <- as.matrix(phys_audiobooks_07_tdm)
phys_audiobooks_07_v <- sort(rowSums(phys_audiobooks_07_m),decreasing=TRUE)
phys_audiobooks_07_df <- data.frame(Year = '2007',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_07_v),freq=phys_audiobooks_07_v)

dig_audiobooks_07 <- checkouts_07_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_07_subjects <- unique(dig_audiobooks_07$Subjects)
dig_audiobooks_07.corpus <- Corpus(VectorSource(dig_audiobooks_07_subjects))
dig_audiobooks_07.corpus <- tm_map(dig_audiobooks_07.corpus, removePunctuation)
dig_audiobooks_07.corpus <- tm_map(dig_audiobooks_07.corpus, content_transformer(tolower))
dig_audiobooks_07.corpus <- tm_map(dig_audiobooks_07.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_07_tdm <- TermDocumentMatrix(dig_audiobooks_07.corpus)
dig_audiobooks_07_m <- as.matrix(dig_audiobooks_07_tdm)
dig_audiobooks_07_v <- sort(rowSums(dig_audiobooks_07_m),decreasing=TRUE)
dig_audiobooks_07_df <- data.frame(Year = '2007',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_07_v),freq=dig_audiobooks_07_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_08_yearly <- checkouts_08_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_08 <- checkouts_08_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_08_subjects <- unique(dig_ebooks_08$Subjects)
dig_ebooks_08.corpus <- Corpus(VectorSource(dig_ebooks_08_subjects))
dig_ebooks_08.corpus <- tm_map(dig_ebooks_08.corpus, removePunctuation)
dig_ebooks_08.corpus <- tm_map(dig_ebooks_08.corpus, content_transformer(tolower))
dig_ebooks_08.corpus <- tm_map(dig_ebooks_08.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_08_tdm <- TermDocumentMatrix(dig_ebooks_08.corpus)
dig_ebooks_08_m <- as.matrix(dig_ebooks_08_tdm)
dig_ebooks_08_v <- sort(rowSums(dig_ebooks_08_m),decreasing=TRUE)
dig_ebooks_08_df <- data.frame(Year = '2008',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_08_v),freq=dig_ebooks_08_v)

phys_books_08 <- checkouts_08_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_08_subjects <- unique(phys_books_08$Subjects)
phys_books_08.corpus <- Corpus(VectorSource(phys_books_08_subjects))
phys_books_08.corpus <- tm_map(phys_books_08.corpus, removePunctuation)
phys_books_08.corpus <- tm_map(phys_books_08.corpus, content_transformer(tolower))
phys_books_08.corpus <- tm_map(phys_books_08.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_08_tdm <- TermDocumentMatrix(phys_books_08.corpus)
phys_books_08_m <- as.matrix(phys_books_08_tdm)
phys_books_08_v <- sort(rowSums(phys_books_08_m),decreasing=TRUE)
phys_books_08_df <- data.frame(Year = '2008',UsageClass = 'Physical',Material='Book',word = names(phys_books_08_v),freq=phys_books_08_v)

phys_audiobooks_08 <- checkouts_08_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_08_subjects <- unique(phys_audiobooks_08$Subjects)
phys_audiobooks_08.corpus <- Corpus(VectorSource(phys_audiobooks_08_subjects))
phys_audiobooks_08.corpus <- tm_map(phys_audiobooks_08.corpus, removePunctuation)
phys_audiobooks_08.corpus <- tm_map(phys_audiobooks_08.corpus, content_transformer(tolower))
phys_audiobooks_08.corpus <- tm_map(phys_audiobooks_08.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_08_tdm <- TermDocumentMatrix(phys_audiobooks_08.corpus)
phys_audiobooks_08_m <- as.matrix(phys_audiobooks_08_tdm)
phys_audiobooks_08_v <- sort(rowSums(phys_audiobooks_08_m),decreasing=TRUE)
phys_audiobooks_08_df <- data.frame(Year = '2008',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_08_v),freq=phys_audiobooks_08_v)

dig_audiobooks_08 <- checkouts_08_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_08_subjects <- unique(dig_audiobooks_08$Subjects)
dig_audiobooks_08.corpus <- Corpus(VectorSource(dig_audiobooks_08_subjects))
dig_audiobooks_08.corpus <- tm_map(dig_audiobooks_08.corpus, removePunctuation)
dig_audiobooks_08.corpus <- tm_map(dig_audiobooks_08.corpus, content_transformer(tolower))
dig_audiobooks_08.corpus <- tm_map(dig_audiobooks_08.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_08_tdm <- TermDocumentMatrix(dig_audiobooks_08.corpus)
dig_audiobooks_08_m <- as.matrix(dig_audiobooks_08_tdm)
dig_audiobooks_08_v <- sort(rowSums(dig_audiobooks_08_m),decreasing=TRUE)
dig_audiobooks_08_df <- data.frame(Year = '2008',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_08_v),freq=dig_audiobooks_08_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_09_yearly <- checkouts_09_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_09 <- checkouts_09_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_09_subjects <- unique(dig_ebooks_09$Subjects)
dig_ebooks_09.corpus <- Corpus(VectorSource(dig_ebooks_09_subjects))
dig_ebooks_09.corpus <- tm_map(dig_ebooks_09.corpus, removePunctuation)
dig_ebooks_09.corpus <- tm_map(dig_ebooks_09.corpus, content_transformer(tolower))
dig_ebooks_09.corpus <- tm_map(dig_ebooks_09.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_09_tdm <- TermDocumentMatrix(dig_ebooks_09.corpus)
dig_ebooks_09_m <- as.matrix(dig_ebooks_09_tdm)
dig_ebooks_09_v <- sort(rowSums(dig_ebooks_09_m),decreasing=TRUE)
dig_ebooks_09_df <- data.frame(Year = '2009',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_09_v),freq=dig_ebooks_09_v)

phys_books_09 <- checkouts_09_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_09_subjects <- unique(phys_books_09$Subjects)
phys_books_09.corpus <- Corpus(VectorSource(phys_books_09_subjects))
phys_books_09.corpus <- tm_map(phys_books_09.corpus, removePunctuation)
phys_books_09.corpus <- tm_map(phys_books_09.corpus, content_transformer(tolower))
phys_books_09.corpus <- tm_map(phys_books_09.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_09_tdm <- TermDocumentMatrix(phys_books_09.corpus)
phys_books_09_m <- as.matrix(phys_books_09_tdm)
phys_books_09_v <- sort(rowSums(phys_books_09_m),decreasing=TRUE)
phys_books_09_df <- data.frame(Year = '2009',UsageClass = 'Physical',Material='Book',word = names(phys_books_09_v),freq=phys_books_09_v)

phys_audiobooks_09 <- checkouts_09_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_09_subjects <- unique(phys_audiobooks_09$Subjects)
phys_audiobooks_09.corpus <- Corpus(VectorSource(phys_audiobooks_09_subjects))
phys_audiobooks_09.corpus <- tm_map(phys_audiobooks_09.corpus, removePunctuation)
phys_audiobooks_09.corpus <- tm_map(phys_audiobooks_09.corpus, content_transformer(tolower))
phys_audiobooks_09.corpus <- tm_map(phys_audiobooks_09.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_09_tdm <- TermDocumentMatrix(phys_audiobooks_09.corpus)
phys_audiobooks_09_m <- as.matrix(phys_audiobooks_09_tdm)
phys_audiobooks_09_v <- sort(rowSums(phys_audiobooks_09_m),decreasing=TRUE)
phys_audiobooks_09_df <- data.frame(Year = '2009',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_09_v),freq=phys_audiobooks_09_v)

dig_audiobooks_09 <- checkouts_09_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_09_subjects <- unique(dig_audiobooks_09$Subjects)
dig_audiobooks_09.corpus <- Corpus(VectorSource(dig_audiobooks_09_subjects))
dig_audiobooks_09.corpus <- tm_map(dig_audiobooks_09.corpus, removePunctuation)
dig_audiobooks_09.corpus <- tm_map(dig_audiobooks_09.corpus, content_transformer(tolower))
dig_audiobooks_09.corpus <- tm_map(dig_audiobooks_09.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_09_tdm <- TermDocumentMatrix(dig_audiobooks_09.corpus)
dig_audiobooks_09_m <- as.matrix(dig_audiobooks_09_tdm)
dig_audiobooks_09_v <- sort(rowSums(dig_audiobooks_09_m),decreasing=TRUE)
dig_audiobooks_09_df <- data.frame(Year = '2009',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_09_v),freq=dig_audiobooks_09_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_10_yearly <- checkouts_10_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_10 <- checkouts_10_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_10_subjects <- unique(dig_ebooks_10$Subjects)
dig_ebooks_10.corpus <- Corpus(VectorSource(dig_ebooks_10_subjects))
dig_ebooks_10.corpus <- tm_map(dig_ebooks_10.corpus, removePunctuation)
dig_ebooks_10.corpus <- tm_map(dig_ebooks_10.corpus, content_transformer(tolower))
dig_ebooks_10.corpus <- tm_map(dig_ebooks_10.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_10_tdm <- TermDocumentMatrix(dig_ebooks_10.corpus)
dig_ebooks_10_m <- as.matrix(dig_ebooks_10_tdm)
dig_ebooks_10_v <- sort(rowSums(dig_ebooks_10_m),decreasing=TRUE)
dig_ebooks_10_df <- data.frame(Year = '2010',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_10_v),freq=dig_ebooks_10_v)

phys_books_10 <- checkouts_10_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_10_subjects <- unique(phys_books_10$Subjects)
phys_books_10.corpus <- Corpus(VectorSource(phys_books_10_subjects))
phys_books_10.corpus <- tm_map(phys_books_10.corpus, removePunctuation)
phys_books_10.corpus <- tm_map(phys_books_10.corpus, content_transformer(tolower))
phys_books_10.corpus <- tm_map(phys_books_10.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_10_tdm <- TermDocumentMatrix(phys_books_10.corpus)
phys_books_10_m <- as.matrix(phys_books_10_tdm)
phys_books_10_v <- sort(rowSums(phys_books_10_m),decreasing=TRUE)
phys_books_10_df <- data.frame(Year = '2010',UsageClass = 'Physical',Material='Book',word = names(phys_books_10_v),freq=phys_books_10_v)

phys_audiobooks_10 <- checkouts_10_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_10_subjects <- unique(phys_audiobooks_10$Subjects)
phys_audiobooks_10.corpus <- Corpus(VectorSource(phys_audiobooks_10_subjects))
phys_audiobooks_10.corpus <- tm_map(phys_audiobooks_10.corpus, removePunctuation)
phys_audiobooks_10.corpus <- tm_map(phys_audiobooks_10.corpus, content_transformer(tolower))
phys_audiobooks_10.corpus <- tm_map(phys_audiobooks_10.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_10_tdm <- TermDocumentMatrix(phys_audiobooks_10.corpus)
phys_audiobooks_10_m <- as.matrix(phys_audiobooks_10_tdm)
phys_audiobooks_10_v <- sort(rowSums(phys_audiobooks_10_m),decreasing=TRUE)
phys_audiobooks_10_df <- data.frame(Year = '2010',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_10_v),freq=phys_audiobooks_10_v)

dig_audiobooks_10 <- checkouts_10_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_10_subjects <- unique(dig_audiobooks_10$Subjects)
dig_audiobooks_10.corpus <- Corpus(VectorSource(dig_audiobooks_10_subjects))
dig_audiobooks_10.corpus <- tm_map(dig_audiobooks_10.corpus, removePunctuation)
dig_audiobooks_10.corpus <- tm_map(dig_audiobooks_10.corpus, content_transformer(tolower))
dig_audiobooks_10.corpus <- tm_map(dig_audiobooks_10.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_10_tdm <- TermDocumentMatrix(dig_audiobooks_10.corpus)
dig_audiobooks_10_m <- as.matrix(dig_audiobooks_10_tdm)
dig_audiobooks_10_v <- sort(rowSums(dig_audiobooks_10_m),decreasing=TRUE)
dig_audiobooks_10_df <- data.frame(Year = '2010',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_10_v),freq=dig_audiobooks_10_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_11_yearly <- checkouts_11_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_11 <- checkouts_11_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_11_subjects <- unique(dig_ebooks_11$Subjects)
dig_ebooks_11.corpus <- Corpus(VectorSource(dig_ebooks_11_subjects))
dig_ebooks_11.corpus <- tm_map(dig_ebooks_11.corpus, removePunctuation)
dig_ebooks_11.corpus <- tm_map(dig_ebooks_11.corpus, content_transformer(tolower))
dig_ebooks_11.corpus <- tm_map(dig_ebooks_11.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_11_tdm <- TermDocumentMatrix(dig_ebooks_11.corpus)
dig_ebooks_11_m <- as.matrix(dig_ebooks_11_tdm)
dig_ebooks_11_v <- sort(rowSums(dig_ebooks_11_m),decreasing=TRUE)
dig_ebooks_11_df <- data.frame(Year = '2011',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_11_v),freq=dig_ebooks_11_v)

phys_books_11 <- checkouts_11_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_11_subjects <- unique(phys_books_11$Subjects)
phys_books_11.corpus <- Corpus(VectorSource(phys_books_11_subjects))
phys_books_11.corpus <- tm_map(phys_books_11.corpus, removePunctuation)
phys_books_11.corpus <- tm_map(phys_books_11.corpus, content_transformer(tolower))
phys_books_11.corpus <- tm_map(phys_books_11.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_11_tdm <- TermDocumentMatrix(phys_books_11.corpus)
phys_books_11_m <- as.matrix(phys_books_11_tdm)
phys_books_11_v <- sort(rowSums(phys_books_11_m),decreasing=TRUE)
phys_books_11_df <- data.frame(Year = '2011',UsageClass = 'Physical',Material='Book',word = names(phys_books_11_v),freq=phys_books_11_v)

phys_audiobooks_11 <- checkouts_11_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_11_subjects <- unique(phys_audiobooks_11$Subjects)
phys_audiobooks_11.corpus <- Corpus(VectorSource(phys_audiobooks_11_subjects))
phys_audiobooks_11.corpus <- tm_map(phys_audiobooks_11.corpus, removePunctuation)
phys_audiobooks_11.corpus <- tm_map(phys_audiobooks_11.corpus, content_transformer(tolower))
phys_audiobooks_11.corpus <- tm_map(phys_audiobooks_11.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_11_tdm <- TermDocumentMatrix(phys_audiobooks_11.corpus)
phys_audiobooks_11_m <- as.matrix(phys_audiobooks_11_tdm)
phys_audiobooks_11_v <- sort(rowSums(phys_audiobooks_11_m),decreasing=TRUE)
phys_audiobooks_11_df <- data.frame(Year = '2011',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_11_v),freq=phys_audiobooks_11_v)

dig_audiobooks_11 <- checkouts_11_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_11_subjects <- unique(dig_audiobooks_11$Subjects)
dig_audiobooks_11.corpus <- Corpus(VectorSource(dig_audiobooks_11_subjects))
dig_audiobooks_11.corpus <- tm_map(dig_audiobooks_11.corpus, removePunctuation)
dig_audiobooks_11.corpus <- tm_map(dig_audiobooks_11.corpus, content_transformer(tolower))
dig_audiobooks_11.corpus <- tm_map(dig_audiobooks_11.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_11_tdm <- TermDocumentMatrix(dig_audiobooks_11.corpus)
dig_audiobooks_11_m <- as.matrix(dig_audiobooks_11_tdm)
dig_audiobooks_11_v <- sort(rowSums(dig_audiobooks_11_m),decreasing=TRUE)
dig_audiobooks_11_df <- data.frame(Year = '2011',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_11_v),freq=dig_audiobooks_11_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_12_yearly <- checkouts_12_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_12 <- checkouts_12_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_12_subjects <- unique(dig_ebooks_12$Subjects)
dig_ebooks_12.corpus <- Corpus(VectorSource(dig_ebooks_12_subjects))
dig_ebooks_12.corpus <- tm_map(dig_ebooks_12.corpus, removePunctuation)
dig_ebooks_12.corpus <- tm_map(dig_ebooks_12.corpus, content_transformer(tolower))
dig_ebooks_12.corpus <- tm_map(dig_ebooks_12.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_12_tdm <- TermDocumentMatrix(dig_ebooks_12.corpus)
dig_ebooks_12_m <- as.matrix(dig_ebooks_12_tdm)
dig_ebooks_12_v <- sort(rowSums(dig_ebooks_12_m),decreasing=TRUE)
dig_ebooks_12_df <- data.frame(Year = '2012',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_12_v),freq=dig_ebooks_12_v)

phys_books_12 <- checkouts_12_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_12_subjects <- unique(phys_books_12$Subjects)
phys_books_12.corpus <- Corpus(VectorSource(phys_books_12_subjects))
phys_books_12.corpus <- tm_map(phys_books_12.corpus, removePunctuation)
phys_books_12.corpus <- tm_map(phys_books_12.corpus, content_transformer(tolower))
phys_books_12.corpus <- tm_map(phys_books_12.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_12_tdm <- TermDocumentMatrix(phys_books_12.corpus)
phys_books_12_m <- as.matrix(phys_books_12_tdm)
phys_books_12_v <- sort(rowSums(phys_books_12_m),decreasing=TRUE)
phys_books_12_df <- data.frame(Year = '2012',UsageClass = 'Physical',Material='Book',word = names(phys_books_12_v),freq=phys_books_12_v)

phys_audiobooks_12 <- checkouts_12_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_12_subjects <- unique(phys_audiobooks_12$Subjects)
phys_audiobooks_12.corpus <- Corpus(VectorSource(phys_audiobooks_12_subjects))
phys_audiobooks_12.corpus <- tm_map(phys_audiobooks_12.corpus, removePunctuation)
phys_audiobooks_12.corpus <- tm_map(phys_audiobooks_12.corpus, content_transformer(tolower))
phys_audiobooks_12.corpus <- tm_map(phys_audiobooks_12.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_12_tdm <- TermDocumentMatrix(phys_audiobooks_12.corpus)
phys_audiobooks_12_m <- as.matrix(phys_audiobooks_12_tdm)
phys_audiobooks_12_v <- sort(rowSums(phys_audiobooks_12_m),decreasing=TRUE)
phys_audiobooks_12_df <- data.frame(Year = '2012',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_12_v),freq=phys_audiobooks_12_v)

dig_audiobooks_12 <- checkouts_12_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_12_subjects <- unique(dig_audiobooks_12$Subjects)
dig_audiobooks_12.corpus <- Corpus(VectorSource(dig_audiobooks_12_subjects))
dig_audiobooks_12.corpus <- tm_map(dig_audiobooks_12.corpus, removePunctuation)
dig_audiobooks_12.corpus <- tm_map(dig_audiobooks_12.corpus, content_transformer(tolower))
dig_audiobooks_12.corpus <- tm_map(dig_audiobooks_12.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_12_tdm <- TermDocumentMatrix(dig_audiobooks_12.corpus)
dig_audiobooks_12_m <- as.matrix(dig_audiobooks_12_tdm)
dig_audiobooks_12_v <- sort(rowSums(dig_audiobooks_12_m),decreasing=TRUE)
dig_audiobooks_12_df <- data.frame(Year = '2012',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_12_v),freq=dig_audiobooks_12_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_13_yearly <- checkouts_13_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_13 <- checkouts_13_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_13_subjects <- unique(dig_ebooks_13$Subjects)
dig_ebooks_13.corpus <- Corpus(VectorSource(dig_ebooks_13_subjects))
dig_ebooks_13.corpus <- tm_map(dig_ebooks_13.corpus, removePunctuation)
dig_ebooks_13.corpus <- tm_map(dig_ebooks_13.corpus, content_transformer(tolower))
dig_ebooks_13.corpus <- tm_map(dig_ebooks_13.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_13_tdm <- TermDocumentMatrix(dig_ebooks_13.corpus)
dig_ebooks_13_m <- as.matrix(dig_ebooks_13_tdm)
dig_ebooks_13_v <- sort(rowSums(dig_ebooks_13_m),decreasing=TRUE)
dig_ebooks_13_df <- data.frame(Year = '2013',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_13_v),freq=dig_ebooks_13_v)

phys_books_13 <- checkouts_13_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_13_subjects <- unique(phys_books_13$Subjects)
phys_books_13.corpus <- Corpus(VectorSource(phys_books_13_subjects))
phys_books_13.corpus <- tm_map(phys_books_13.corpus, removePunctuation)
phys_books_13.corpus <- tm_map(phys_books_13.corpus, content_transformer(tolower))
phys_books_13.corpus <- tm_map(phys_books_13.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_13_tdm <- TermDocumentMatrix(phys_books_13.corpus)
phys_books_13_m <- as.matrix(phys_books_13_tdm)
phys_books_13_v <- sort(rowSums(phys_books_13_m),decreasing=TRUE)
phys_books_13_df <- data.frame(Year = '2013',UsageClass = 'Physical',Material='Book',word = names(phys_books_13_v),freq=phys_books_13_v)

phys_audiobooks_13 <- checkouts_13_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_13_subjects <- unique(phys_audiobooks_13$Subjects)
phys_audiobooks_13.corpus <- Corpus(VectorSource(phys_audiobooks_13_subjects))
phys_audiobooks_13.corpus <- tm_map(phys_audiobooks_13.corpus, removePunctuation)
phys_audiobooks_13.corpus <- tm_map(phys_audiobooks_13.corpus, content_transformer(tolower))
phys_audiobooks_13.corpus <- tm_map(phys_audiobooks_13.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_13_tdm <- TermDocumentMatrix(phys_audiobooks_13.corpus)
phys_audiobooks_13_m <- as.matrix(phys_audiobooks_13_tdm)
phys_audiobooks_13_v <- sort(rowSums(phys_audiobooks_13_m),decreasing=TRUE)
phys_audiobooks_13_df <- data.frame(Year = '2013',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_13_v),freq=phys_audiobooks_13_v)

dig_audiobooks_13 <- checkouts_13_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_13_subjects <- unique(dig_audiobooks_13$Subjects)
dig_audiobooks_13.corpus <- Corpus(VectorSource(dig_audiobooks_13_subjects))
dig_audiobooks_13.corpus <- tm_map(dig_audiobooks_13.corpus, removePunctuation)
dig_audiobooks_13.corpus <- tm_map(dig_audiobooks_13.corpus, content_transformer(tolower))
dig_audiobooks_13.corpus <- tm_map(dig_audiobooks_13.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_13_tdm <- TermDocumentMatrix(dig_audiobooks_13.corpus)
dig_audiobooks_13_m <- as.matrix(dig_audiobooks_13_tdm)
dig_audiobooks_13_v <- sort(rowSums(dig_audiobooks_13_m),decreasing=TRUE)
dig_audiobooks_13_df <- data.frame(Year = '2013',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_13_v),freq=dig_audiobooks_13_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_14_yearly <- checkouts_14_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_14 <- checkouts_14_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_14_subjects <- unique(dig_ebooks_14$Subjects)
dig_ebooks_14.corpus <- Corpus(VectorSource(dig_ebooks_14_subjects))
dig_ebooks_14.corpus <- tm_map(dig_ebooks_14.corpus, removePunctuation)
dig_ebooks_14.corpus <- tm_map(dig_ebooks_14.corpus, content_transformer(tolower))
dig_ebooks_14.corpus <- tm_map(dig_ebooks_14.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_14_tdm <- TermDocumentMatrix(dig_ebooks_14.corpus)
dig_ebooks_14_m <- as.matrix(dig_ebooks_14_tdm)
dig_ebooks_14_v <- sort(rowSums(dig_ebooks_14_m),decreasing=TRUE)
dig_ebooks_14_df <- data.frame(Year = '2014',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_14_v),freq=dig_ebooks_14_v)

phys_books_14 <- checkouts_14_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_14_subjects <- unique(phys_books_14$Subjects)
phys_books_14.corpus <- Corpus(VectorSource(phys_books_14_subjects))
phys_books_14.corpus <- tm_map(phys_books_14.corpus, removePunctuation)
phys_books_14.corpus <- tm_map(phys_books_14.corpus, content_transformer(tolower))
phys_books_14.corpus <- tm_map(phys_books_14.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_14_tdm <- TermDocumentMatrix(phys_books_14.corpus)
phys_books_14_m <- as.matrix(phys_books_14_tdm)
phys_books_14_v <- sort(rowSums(phys_books_14_m),decreasing=TRUE)
phys_books_14_df <- data.frame(Year = '2014',UsageClass = 'Physical',Material='Book',word = names(phys_books_14_v),freq=phys_books_14_v)

phys_audiobooks_14 <- checkouts_14_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_14_subjects <- unique(phys_audiobooks_14$Subjects)
phys_audiobooks_14.corpus <- Corpus(VectorSource(phys_audiobooks_14_subjects))
phys_audiobooks_14.corpus <- tm_map(phys_audiobooks_14.corpus, removePunctuation)
phys_audiobooks_14.corpus <- tm_map(phys_audiobooks_14.corpus, content_transformer(tolower))
phys_audiobooks_14.corpus <- tm_map(phys_audiobooks_14.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_14_tdm <- TermDocumentMatrix(phys_audiobooks_14.corpus)
phys_audiobooks_14_m <- as.matrix(phys_audiobooks_14_tdm)
phys_audiobooks_14_v <- sort(rowSums(phys_audiobooks_14_m),decreasing=TRUE)
phys_audiobooks_14_df <- data.frame(Year = '2014',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_14_v),freq=phys_audiobooks_14_v)

dig_audiobooks_14 <- checkouts_14_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_14_subjects <- unique(dig_audiobooks_14$Subjects)
dig_audiobooks_14.corpus <- Corpus(VectorSource(dig_audiobooks_14_subjects))
dig_audiobooks_14.corpus <- tm_map(dig_audiobooks_14.corpus, removePunctuation)
dig_audiobooks_14.corpus <- tm_map(dig_audiobooks_14.corpus, content_transformer(tolower))
dig_audiobooks_14.corpus <- tm_map(dig_audiobooks_14.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_14_tdm <- TermDocumentMatrix(dig_audiobooks_14.corpus)
dig_audiobooks_14_m <- as.matrix(dig_audiobooks_14_tdm)
dig_audiobooks_14_v <- sort(rowSums(dig_audiobooks_14_m),decreasing=TRUE)
dig_audiobooks_14_df <- data.frame(Year = '2014',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_14_v),freq=dig_audiobooks_14_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_15_yearly <- checkouts_15_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_15 <- checkouts_15_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_15_subjects <- unique(dig_ebooks_15$Subjects)
dig_ebooks_15.corpus <- Corpus(VectorSource(dig_ebooks_15_subjects))
dig_ebooks_15.corpus <- tm_map(dig_ebooks_15.corpus, removePunctuation)
dig_ebooks_15.corpus <- tm_map(dig_ebooks_15.corpus, content_transformer(tolower))
dig_ebooks_15.corpus <- tm_map(dig_ebooks_15.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_15_tdm <- TermDocumentMatrix(dig_ebooks_15.corpus)
dig_ebooks_15_m <- as.matrix(dig_ebooks_15_tdm)
dig_ebooks_15_v <- sort(rowSums(dig_ebooks_15_m),decreasing=TRUE)
dig_ebooks_15_df <- data.frame(Year = '2015',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_15_v),freq=dig_ebooks_15_v)

phys_books_15 <- checkouts_15_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_15_subjects <- unique(phys_books_15$Subjects)
phys_books_15.corpus <- Corpus(VectorSource(phys_books_15_subjects))
phys_books_15.corpus <- tm_map(phys_books_15.corpus, removePunctuation)
phys_books_15.corpus <- tm_map(phys_books_15.corpus, content_transformer(tolower))
phys_books_15.corpus <- tm_map(phys_books_15.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_15_tdm <- TermDocumentMatrix(phys_books_15.corpus)
phys_books_15_m <- as.matrix(phys_books_15_tdm)
phys_books_15_v <- sort(rowSums(phys_books_15_m),decreasing=TRUE)
phys_books_15_df <- data.frame(Year = '2015',UsageClass = 'Physical',Material='Book',word = names(phys_books_15_v),freq=phys_books_15_v)

phys_audiobooks_15 <- checkouts_15_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_15_subjects <- unique(phys_audiobooks_15$Subjects)
phys_audiobooks_15.corpus <- Corpus(VectorSource(phys_audiobooks_15_subjects))
phys_audiobooks_15.corpus <- tm_map(phys_audiobooks_15.corpus, removePunctuation)
phys_audiobooks_15.corpus <- tm_map(phys_audiobooks_15.corpus, content_transformer(tolower))
phys_audiobooks_15.corpus <- tm_map(phys_audiobooks_15.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_15_tdm <- TermDocumentMatrix(phys_audiobooks_15.corpus)
phys_audiobooks_15_m <- as.matrix(phys_audiobooks_15_tdm)
phys_audiobooks_15_v <- sort(rowSums(phys_audiobooks_15_m),decreasing=TRUE)
phys_audiobooks_15_df <- data.frame(Year = '2015',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_15_v),freq=phys_audiobooks_15_v)

dig_audiobooks_15 <- checkouts_15_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_15_subjects <- unique(dig_audiobooks_15$Subjects)
dig_audiobooks_15.corpus <- Corpus(VectorSource(dig_audiobooks_15_subjects))
dig_audiobooks_15.corpus <- tm_map(dig_audiobooks_15.corpus, removePunctuation)
dig_audiobooks_15.corpus <- tm_map(dig_audiobooks_15.corpus, content_transformer(tolower))
dig_audiobooks_15.corpus <- tm_map(dig_audiobooks_15.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_15_tdm <- TermDocumentMatrix(dig_audiobooks_15.corpus)
dig_audiobooks_15_m <- as.matrix(dig_audiobooks_15_tdm)
dig_audiobooks_15_v <- sort(rowSums(dig_audiobooks_15_m),decreasing=TRUE)
dig_audiobooks_15_df <- data.frame(Year = '2015',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_15_v),freq=dig_audiobooks_15_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_16_yearly <- checkouts_16_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_16 <- checkouts_16_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_16_subjects <- unique(dig_ebooks_16$Subjects)
dig_ebooks_16.corpus <- Corpus(VectorSource(dig_ebooks_16_subjects))
dig_ebooks_16.corpus <- tm_map(dig_ebooks_16.corpus, removePunctuation)
dig_ebooks_16.corpus <- tm_map(dig_ebooks_16.corpus, content_transformer(tolower))
dig_ebooks_16.corpus <- tm_map(dig_ebooks_16.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_16_tdm <- TermDocumentMatrix(dig_ebooks_16.corpus)
dig_ebooks_16_m <- as.matrix(dig_ebooks_16_tdm)
dig_ebooks_16_v <- sort(rowSums(dig_ebooks_16_m),decreasing=TRUE)
dig_ebooks_16_df <- data.frame(Year = '2016',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_16_v),freq=dig_ebooks_16_v)

phys_books_16 <- checkouts_16_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_16_subjects <- unique(phys_books_16$Subjects)
phys_books_16.corpus <- Corpus(VectorSource(phys_books_16_subjects))
phys_books_16.corpus <- tm_map(phys_books_16.corpus, removePunctuation)
phys_books_16.corpus <- tm_map(phys_books_16.corpus, content_transformer(tolower))
phys_books_16.corpus <- tm_map(phys_books_16.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_16_tdm <- TermDocumentMatrix(phys_books_16.corpus)
phys_books_16_m <- as.matrix(phys_books_16_tdm)
phys_books_16_v <- sort(rowSums(phys_books_16_m),decreasing=TRUE)
phys_books_16_df <- data.frame(Year = '2016',UsageClass = 'Physical',Material='Book',word = names(phys_books_16_v),freq=phys_books_16_v)

phys_audiobooks_16 <- checkouts_16_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_16_subjects <- unique(phys_audiobooks_16$Subjects)
phys_audiobooks_16.corpus <- Corpus(VectorSource(phys_audiobooks_16_subjects))
phys_audiobooks_16.corpus <- tm_map(phys_audiobooks_16.corpus, removePunctuation)
phys_audiobooks_16.corpus <- tm_map(phys_audiobooks_16.corpus, content_transformer(tolower))
phys_audiobooks_16.corpus <- tm_map(phys_audiobooks_16.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_16_tdm <- TermDocumentMatrix(phys_audiobooks_16.corpus)
phys_audiobooks_16_m <- as.matrix(phys_audiobooks_16_tdm)
phys_audiobooks_16_v <- sort(rowSums(phys_audiobooks_16_m),decreasing=TRUE)
phys_audiobooks_16_df <- data.frame(Year = '2016',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_16_v),freq=phys_audiobooks_16_v)

dig_audiobooks_16 <- checkouts_16_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_16_subjects <- unique(dig_audiobooks_16$Subjects)
dig_audiobooks_16.corpus <- Corpus(VectorSource(dig_audiobooks_16_subjects))
dig_audiobooks_16.corpus <- tm_map(dig_audiobooks_16.corpus, removePunctuation)
dig_audiobooks_16.corpus <- tm_map(dig_audiobooks_16.corpus, content_transformer(tolower))
dig_audiobooks_16.corpus <- tm_map(dig_audiobooks_16.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_16_tdm <- TermDocumentMatrix(dig_audiobooks_16.corpus)
dig_audiobooks_16_m <- as.matrix(dig_audiobooks_16_tdm)
dig_audiobooks_16_v <- sort(rowSums(dig_audiobooks_16_m),decreasing=TRUE)
dig_audiobooks_16_df <- data.frame(Year = '2016',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_16_v),freq=dig_audiobooks_16_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_17_yearly <- checkouts_17_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

dig_ebooks_17 <- checkouts_17_category %>% 
  subset(Material == 'EBook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_ebooks_17_subjects <- unique(dig_ebooks_17$Subjects)
dig_ebooks_17.corpus <- Corpus(VectorSource(dig_ebooks_17_subjects))
dig_ebooks_17.corpus <- tm_map(dig_ebooks_17.corpus, removePunctuation)
dig_ebooks_17.corpus <- tm_map(dig_ebooks_17.corpus, content_transformer(tolower))
dig_ebooks_17.corpus <- tm_map(dig_ebooks_17.corpus, function(x) removeWords(x, stopwords("english")))
dig_ebooks_17_tdm <- TermDocumentMatrix(dig_ebooks_17.corpus)
dig_ebooks_17_m <- as.matrix(dig_ebooks_17_tdm)
dig_ebooks_17_v <- sort(rowSums(dig_ebooks_17_m),decreasing=TRUE)
dig_ebooks_17_df <- data.frame(Year = '2017',UsageClass = 'Digital',Material='EBook',word = names(dig_ebooks_17_v),freq=dig_ebooks_17_v)

phys_books_17 <- checkouts_17_category %>% 
  subset(Material == 'Book') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_books_17_subjects <- unique(phys_books_17$Subjects)
phys_books_17.corpus <- Corpus(VectorSource(phys_books_17_subjects))
phys_books_17.corpus <- tm_map(phys_books_17.corpus, removePunctuation)
phys_books_17.corpus <- tm_map(phys_books_17.corpus, content_transformer(tolower))
phys_books_17.corpus <- tm_map(phys_books_17.corpus, function(x) removeWords(x, stopwords("english")))
phys_books_17_tdm <- TermDocumentMatrix(phys_books_17.corpus)
phys_books_17_m <- as.matrix(phys_books_17_tdm)
phys_books_17_v <- sort(rowSums(phys_books_17_m),decreasing=TRUE)
phys_books_17_df <- data.frame(Year = '2017',UsageClass = 'Physical',Material='Book',word = names(phys_books_17_v),freq=phys_books_17_v)

phys_audiobooks_17 <- checkouts_17_category %>% 
  subset(UsageClass=='Physical' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
phys_audiobooks_17_subjects <- unique(phys_audiobooks_17$Subjects)
phys_audiobooks_17.corpus <- Corpus(VectorSource(phys_audiobooks_17_subjects))
phys_audiobooks_17.corpus <- tm_map(phys_audiobooks_17.corpus, removePunctuation)
phys_audiobooks_17.corpus <- tm_map(phys_audiobooks_17.corpus, content_transformer(tolower))
phys_audiobooks_17.corpus <- tm_map(phys_audiobooks_17.corpus, function(x) removeWords(x, stopwords("english")))
phys_audiobooks_17_tdm <- TermDocumentMatrix(phys_audiobooks_17.corpus)
phys_audiobooks_17_m <- as.matrix(phys_audiobooks_17_tdm)
phys_audiobooks_17_v <- sort(rowSums(phys_audiobooks_17_m),decreasing=TRUE)
phys_audiobooks_17_df <- data.frame(Year = '2017',UsageClass = 'Physical',Material='Audiobook',word = names(phys_audiobooks_17_v),freq=phys_audiobooks_17_v)

dig_audiobooks_17 <- checkouts_17_category %>% 
  subset(UsageClass=='Digital' & Material == 'Audiobook') %>% 
  group_by(CheckoutYear,UsageClass,Material,Title,Creator,Subjects) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(n=1000)
dig_audiobooks_17_subjects <- unique(dig_audiobooks_17$Subjects)
dig_audiobooks_17.corpus <- Corpus(VectorSource(dig_audiobooks_17_subjects))
dig_audiobooks_17.corpus <- tm_map(dig_audiobooks_17.corpus, removePunctuation)
dig_audiobooks_17.corpus <- tm_map(dig_audiobooks_17.corpus, content_transformer(tolower))
dig_audiobooks_17.corpus <- tm_map(dig_audiobooks_17.corpus, function(x) removeWords(x, stopwords("english")))
dig_audiobooks_17_tdm <- TermDocumentMatrix(dig_audiobooks_17.corpus)
dig_audiobooks_17_m <- as.matrix(dig_audiobooks_17_tdm)
dig_audiobooks_17_v <- sort(rowSums(dig_audiobooks_17_m),decreasing=TRUE)
dig_audiobooks_17_df <- data.frame(Year = '2017',UsageClass = 'Digital',Material='Audiobook',word = names(dig_audiobooks_17_v),freq=dig_audiobooks_17_v)


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
  mutate(MonthlyPct = round(100*(MatMthlyCheckouts/MonthlyTotal),2))

checkouts_18_yearly <- checkouts_18_category %>% 
  group_by(CheckoutYear, UsageClass,Material) %>% 
  summarise(MatYrlyCheckouts = sum(Checkouts)) %>% 
  group_by(CheckoutYear) %>% 
  mutate(YearlyTotal = sum(MatYrlyCheckouts)) %>% 
  ungroup() %>% 
  select(CheckoutYear,UsageClass,Material,MatYrlyCheckouts,YearlyTotal) %>% 
  mutate(YearlyPct = round(100*(MatYrlyCheckouts/YearlyTotal),2))

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

############ Combine and Create RDS files for Shiny App ##############
checkouts_monthly_all <-  rbind(checkouts_06_monthly,checkouts_07_monthly,checkouts_08_monthly,checkouts_09_monthly,checkouts_10_monthly,checkouts_11_monthly,checkouts_12_monthly,checkouts_13_monthly,checkouts_14_monthly,checkouts_15_monthly,checkouts_16_monthly,checkouts_17_monthly,checkouts_18_monthly)
checkouts_monthly_all <- checkouts_monthly_all %>% 
  mutate(MatMthlyCheckouts = MatMthlyCheckouts/1000) %>% 
  select(Year=CheckoutYear,MonthYear=CheckoutMonYr,UsageClass,Material,`Checkouts`= MatMthlyCheckouts,`Percentage`=MonthlyPct)

checkouts_yearly_all <-  rbind(checkouts_06_yearly,checkouts_07_yearly,checkouts_08_yearly,checkouts_09_yearly,checkouts_10_yearly,checkouts_11_yearly,checkouts_12_yearly,checkouts_13_yearly,checkouts_14_yearly,checkouts_15_yearly,checkouts_16_yearly,checkouts_17_yearly,checkouts_18_yearly)
checkouts_yearly_all <- checkouts_yearly_all %>% 
  mutate(MatYrlyCheckouts = MatYrlyCheckouts/1000) %>% 
  select(Year=CheckoutYear,UsageClass,Material,`Checkouts`= MatYrlyCheckouts,`Percentage`=YearlyPct)

test <- checkouts_yearly_all %>% 
  group_by(Year,UsageClass) %>% 
  summarise(pct = sum(Percentage))

saveRDS(checkouts_monthly_all,'checkouts_monthly_all.RDS')
saveRDS(checkouts_yearly_all,'checkouts_yearly_all.RDS')

year <- as.data.frame(top_checkouts_subjects) %>%
  select(Year) %>%
  unique() %>%
  arrange(Year)

top_checkouts <- rbind(dig_ebooks_06,dig_audiobooks_06,phys_books_06,phys_audiobooks_06,
                       dig_ebooks_07,dig_audiobooks_07,phys_books_07,phys_audiobooks_07,
                       dig_ebooks_08,dig_audiobooks_08,phys_books_08,phys_audiobooks_08,
                       dig_ebooks_09,dig_audiobooks_09,phys_books_09,phys_audiobooks_09,
                       dig_ebooks_10,dig_audiobooks_10,phys_books_10,phys_audiobooks_10,
                       dig_ebooks_11,dig_audiobooks_11,phys_books_11,phys_audiobooks_11,
                       dig_ebooks_12,dig_audiobooks_12,phys_books_12,phys_audiobooks_12,
                       dig_ebooks_13,dig_audiobooks_13,phys_books_13,phys_audiobooks_13,
                       dig_ebooks_14,dig_audiobooks_14,phys_books_14,phys_audiobooks_14,
                       dig_ebooks_15,dig_audiobooks_15,phys_books_15,phys_audiobooks_15,
                       dig_ebooks_16,dig_audiobooks_16,phys_books_16,phys_audiobooks_16,
                       dig_ebooks_17,dig_audiobooks_17,phys_books_17,phys_audiobooks_17,
                       dig_ebooks_18,dig_audiobooks_18,phys_books_18,phys_audiobooks_18)

top_checkouts <- top_checkouts %>% 
  subset(Title != '<Unknown Title>') %>% 
  subset((Title %like% 'Uncataloged Folder or Bag')) %>% 
  ungroup()

top_checkouts_subjects_dig <- rbind(dig_ebooks_06_df,dig_audiobooks_06_df,
                                    dig_ebooks_07_df,dig_audiobooks_07_df,
                                    dig_ebooks_08_df,dig_audiobooks_08_df,
                                    dig_ebooks_09_df,dig_audiobooks_09_df,
                                    dig_ebooks_10_df,dig_audiobooks_10_df,
                                    dig_ebooks_11_df,dig_audiobooks_11_df,
                                    dig_ebooks_12_df,dig_audiobooks_12_df,
                                    dig_ebooks_13_df,dig_audiobooks_13_df,
                                    dig_ebooks_14_df,dig_audiobooks_14_df,
                                    dig_ebooks_15_df,dig_audiobooks_15_df,
                                    dig_ebooks_16_df,dig_audiobooks_16_df,
                                    dig_ebooks_17_df,dig_audiobooks_17_df,
                                    dig_ebooks_18_df,dig_audiobooks_18_df)

top_checkouts_subjects_phys <- rbind(phys_books_06_df,phys_audiobooks_06_df,
                                     phys_books_07_df,phys_audiobooks_07_df,
                                     phys_books_08_df,phys_audiobooks_08_df,
                                     phys_books_09_df,phys_audiobooks_09_df,
                                     phys_books_10_df,phys_audiobooks_10_df,
                                     phys_books_11_df,phys_audiobooks_11_df,
                                     phys_books_12_df,phys_audiobooks_12_df,
                                     phys_books_13_df,phys_audiobooks_13_df,
                                     phys_books_14_df,phys_audiobooks_14_df,
                                     phys_books_15_df,phys_audiobooks_15_df,
                                     phys_books_16_df,phys_audiobooks_16_df,
                                     phys_books_17_df,phys_audiobooks_17_df,
                                     phys_books_18_df,phys_audiobooks_18_df)

top_checkouts_subjects_dig$word <- gsub('young', 'young adult', top_checkouts_subjects_dig$word)

top_checkouts_subjects_dig <- top_checkouts_subjects_dig %>% 
  subset(word != 'adult')

top_checkouts_subjects_phys$word <- gsub('united', 'united states', top_checkouts_subjects_phys$word)

top_checkouts_subjects_phys <- top_checkouts_subjects_phys %>% 
  subset(word != 'states')


top_checkouts_subjects <- rbind(top_checkouts_subjects_dig, top_checkouts_subjects_phys)

top_checkouts_subjects <- top_checkouts_subjects %>% 
  subset(!(word %in% c('fiction','nonfiction','audiobooks')))


saveRDS(top_checkouts,'top_checkouts.RDS')
saveRDS(top_checkouts_subjects,'top_checkouts_subjects.RDS')


item <- c(unique(checkouts_yearly_all$Material),unique(checkouts_yearly_all$UsageClass))

item.color <- custom_pal
names(item.color) <- item

pal <- brewer.pal(8, "Set2")
pal2 <- brewer.pal(12, "Set3")
pal <- pal[-(1:2)]
pal3 <- c(pal,pal2[5:5],pal2[1:1],pal2[10:10])
png("wordcloud.png", width=1280,height=800)
wordcloud(phys_other_18_df$word,phys_other_18_df$freq,min.freq=10,max.words=100, random.order=T, rot.per=.15,  vfont=c("sans serif","plain"))

dev.off()

ggplot(checkouts_yearly_all, aes_string(fill='Material', y='Percentage', x='Year')) + 
  geom_bar( stat="identity")+
  theme_light()+
 # ylab(if_else(input$measure=='Checkouts', paste("Number of Checkouts (in thousands)"),"Percentage of Total Checkouts"))+
  ggtitle('Checkout Trends for 2006 - 2018')


# round percent to 1/2 decimal, divide totals by 1000, 

# assign particular colors to material types,

# select yearly vs monthly view --can i click on year and get monthly view?
# take out fiction/nonfiction/audiobooks/literature? from final wordcloud dataframe
# change to wordcloud2 (finish--need to update parameters)
# add table of top titles based on wordcloud choices
# only give book/audiobook if physical selected for word cloud and ebook/audiobook if digital selected
## size  of wordcloud
## add good error message for when physical/ebook combo
## add wording/spacing
## change radio button names vs values
