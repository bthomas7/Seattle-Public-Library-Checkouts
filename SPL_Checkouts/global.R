library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(wordcloud)


# load datasets
checkouts_monthly_all <- readRDS('data/checkouts_monthly_all.RDS')
checkouts_yearly_all <- readRDS('data/checkouts_yearly_all.RDS')
top_checkouts_2018 <-  readRDS('data/top_checkouts_2018.RDS')
top_checkouts_subjects_2018 <- readRDS('data/top_checkouts_subjects_2018.RDS')

# # Lists for dropdowns
# category <- as.data.frame(checkouts_yearly_all) %>% 
#   select(UsageClass) %>% 
#   unique() %>% 
#   arrange(UsageClass)
# 
# #states <- sort(states$State)
# #states <- c("--Click to Select--",states)
# 
# material <- as.data.frame(checkouts_yearly_all) %>% 
#   select(Material) %>% 
#   unique() %>% 
#   arrange(Material)
# 
year <- as.data.frame(top_checkouts_subjects_2018) %>%
  select(CheckoutYear) %>%
  unique() %>%
  arrange(CheckoutYear)
# 
# month <- as.data.frame(checkouts_monthly_all) %>% 
#   select(CheckoutMonYr) %>% 
#   unique() %>% 
#   arrange(CheckoutMonYr)

