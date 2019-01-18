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

phys_inv_changes <- physical_inventory %>% 
  group_by(ReportDate) %>% 
  summarise(NumItems = sum(ItemCount))

current_phys_inventory <- physical_inventory %>% 
  subset(ReportDate == '01/01/2019') %>% 
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

audiobooks <- checkouts_18[grep('AUDIOBOOK',toupper(checkouts_18$Subjects)),]
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

phys_checkouts_18 <- read_csv("data/Checkouts_By_Title_Data_Lens_2018.csv") 

phys_checkouts_18_summ <- phys_checkouts_18 %>% 
  group_by(CheckoutYear, ItemType, Collection, CallNumber, ItemTitle, Subjects) %>% 
  summarise(Checkouts = n())

phys_checkouts_18_summ_join <- phys_checkouts_18_summ %>% 
  left_join(item_type,by=c("ItemType"="Code")) %>% 
  left_join(collection_type, by=c("Collection"="Code"))

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
