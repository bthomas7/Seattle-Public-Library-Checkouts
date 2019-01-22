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

#SOUNDDISC with subject entry of Audiobooks = physical audiobooks
# Read in data
checkouts_18 <- read_csv('data/Checkouts_by_Title_2018.csv')  
ils_data <- read_csv('data/Integrated_Library_System__ILS__Data_Dictionary.csv')
#physical_inventory <- read_csv('data/Library_Collection_Inventory.csv')

item_type <- ils_data %>% 
  subset(`Code Type`=='ItemType') %>% 
  select(Code,`ItemTypeDesc`=Description,`Format Group`,`Format Subgroup`)

collection_type <- ils_data %>% 
  subset(`Code Type`=='ItemCollection') %>% 
  select(Code,`CollTypeDesc`= Description,`Coll Format Subgroup`=`Format Subgroup`)


checkouts_18$Material <- NA
checkouts_18$Material <- checkouts_18$MaterialType
checkouts_18$Material[checkouts_18$MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT")] <- 'Book'
checkouts_18$Material[tolower(checkouts_18$Subjects) %like% 'comic and graphic book'|tolower(checkouts_18$Subjects) %like% 'comic book'| checkouts_18$MaterialType == 'COMIC'] <- 'Comic'
checkouts_18$Material[tolower(checkouts_18$Subjects) %like% 'audiobook'|(tolower(checkouts_18$Publisher) %like% 'audio'& checkouts_18$UsageClass == 'Physical')|checkouts_18$MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC') ] <- 'Audiobook'


checkout_test <- checkouts_18 %>% 
  mutate(mat = case_when(MaterialType %in% c('ATLAS','BOOK','CR',"ER","NONPROJGRAPH" ,"ER, NONPROJGRAPH","ER, PRINT","ER, REGPRINT","LARGEPRINT","REGPRINT") ~"Book",
                         MaterialType == 'EBOOK' ~ "EBook",
                         (tolower(Subjects) %like% 'audiobook'|(tolower(checkouts_18$Publisher) %like% 'audio'& checkouts_18$UsageClass == 'Physical')|checkouts_18$MaterialType %in% c('AUDIOBOOK','REGPRINT, SOUNDDISC','ER, SOUNDDISC','SOUNDCASS')|(MaterialType=='SOUNDDISC' & tolower(Subjects) %like% 'fiction')) ~ "Audiobook",
                         (tolower(Subjects) %like% 'comic and graphic book'|tolower(Subjects) %like% 'comic book'| MaterialType == 'COMIC') ~ "Comic",
                         MaterialType %in% c('SONG','MUSIC','MUSICSNDREC','SOUNDCASS, SOUNDDISC','SOUNDDISC, SOUNDREC','SOUNDREC','SOUNDDISC') ~ "Music",
                         MaterialType %in% c('MOVIE','TELEVISION','VIDEO','ER, VIDEODISC','REGPRINT, VIDEOREC','SOUNDCASS, SOUNDDISC, VIDEOCASS, VIDEODISC','SOUNDDISC, VIDEOCASS','SOUNDDISC, VIDEODISC','VIDEOCART','VIDEOCASS','VIDEOCASS, VIDEODISC','VIDEODISC','VIDEOREC') ~ 'Video',
                         MaterialType %in% c('PICTURE','PRINT','VISUAL') ~ "Visual",
                         MaterialType == 'MAGAZINE' ~ "Magazine",
                         TRUE ~ "Other"))







checkouts_18_group <- checkouts_18 %>% 
  group_by(UsageClass,CheckoutType,Material) %>% 
  summarise(NumCheckouts = sum(Checkouts))

checkout_test_group <- checkout_test %>% 
  subset(mat='OTHER') %>% 
  group_by(UsageClass,CheckoutType,Material) %>% 
  summarise(NumCheckouts = sum(Checkouts))


checkouts_18_group2 <- checkouts_18 %>% 
  subset(MaterialType=='SOUNDDISC') %>% 
  subset(Material!='Audiobook') %>% 
  subset(tolower(Publiserh) %like% 'audio') %>% 
  group_by(UsageClass,CheckoutType,Material,Publisher) %>% 
  summarise(NumCheckouts = sum(Checkouts))

check_sub <- checkouts_18 %>% 
  subset(UsageClass=='Physical') %>% 
  subset(tolower(Publisher) %like% 'audio')





#current_phys_inventory <- physical_inventory %>% 
#  subset(ReportDate == '01/01/2019') %>% 
#  group_by(Title, Subjects, ItemType, ItemCollection) %>% 
#  summarise(NumItems = sum(ItemCount)) %>% 
#  select (Title, Subjects, ItemType, ItemCollection,ItemLocation, ItemCount) %>% 
drop_na(Title) %>% 
  arra

phys_checkouts <- checkouts_18 %>% 
  subset(UsageClass == 'Physical') %>% 
  left_join(current_phys_inventory,by=c("Title","Subjects"))



phys_checkouts_group <- phys_checkouts %>% 
  group_by(UsageClass) %>% 
  summarise(NumCheckouts = sum(Checkouts))

audiobooks <- checkouts_18[grep('Audiobook',checkouts_18$Subjects,ignore.case),]
audiobooks_group <- audiobooks %>% 
  group_by(UsageClass,CheckoutType,MaterialType) %>% 
  summarise(NumCheckouts = sum(Checkouts))

overdrive <- checkouts_18 %>% 
  subset(CheckoutType=='OverDrive') #%>% 
#  group_by(MaterialType,Subjects) %>% 
#  summarise(NumCheckouts = sum(Checkouts))

checkouts_18_group <- checkouts_18 %>% 
  group_by(UsageClass,CheckoutType,MaterialType) %>% 
  summarise(NumCheckouts = sum(Checkouts))
# tally()

checkouts_18_group2 <- checkouts_18 %>% 
  group_by(Title) %>% 
  summarise(NumCheckouts = sum(Checkouts))


checkouts_18_subset <- checkouts_18 %>% 
  # subset(CheckoutType=='OverDrive')
  subset(Creator %like% "Lu, Marie")

###################################

phys_checkouts_18 <- read_csv("data/Checkouts_By_Title_Data_Lens_2018.csv") 

phys_checkouts_18_summ <- phys_checkouts_18 %>% 
  group_by(CheckoutYear, ItemType, Collection, CallNumber, ItemTitle, Subjects) %>% 
  summarise(Checkouts = n())

phys_checkouts_18_summ_join <- phys_checkouts_18_summ %>% 
  left_join(item_type,by=c("ItemType"="Code")) %>% 
  left_join(collection_type, by=c("Collection"="Code"))

phys_with_aud <- phys_checkouts_18_summ_join %>% 
  mutate(Material = ifelse(grepl('Audiobook',phys_checkouts_18_summ_join$Subjects,ignore.case = TRUEr), 'Audiobook',"other"))

audiobooks <- phys_checkouts_18_summ_join[grep('AUDIOBOOK',toupper(phys_checkouts_18_summ_join$Subjects)),]

audiobk_item_coll <- audiobooks %>% 
  group_by (`Coll Format Subgroup`,Collection,CollTypeDesc,`Format Subgroup`,ItemType,ItemTypeDesc) %>% 
  summarise(Checkouts=sum(Checkouts))

summ_item_type <- phys_checkouts_18_summ_join %>% 
  group_by (`Format Subgroup`,ItemType,ItemTypeDesc) %>% 
  summarise(Checkouts=sum(Checkouts))

summ_item_coll <- phys_checkouts_18_summ_join %>% 
  group_by (`Coll Format Subgroup`,Collection,CollTypeDesc,`Format Subgroup`,ItemType,ItemTypeDesc) %>% 
  summarise(Checkouts=sum(Checkouts))
