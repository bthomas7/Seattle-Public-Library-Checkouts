library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)


# load datasets
checkouts_monthly_all <- readRDS('data/checkouts_monthly_all.RDS')
checkouts_yearly_all <- readRDS('data/checkouts_yearly_all.RDS')

# Lists for dropdowns
category <- as.data.frame(checkouts_yearly_all) %>% 
  select(UsageClass) %>% 
  unique() %>% 
  arrange(UsageClass)

#states <- sort(states$State)
#states <- c("--Click to Select--",states)

material <- as.data.frame(checkouts_yearly_all) %>% 
  select(Material) %>% 
  unique() %>% 
  arrange(Material)

year <- as.data.frame(checkouts_yearly_all) %>% 
  select(CheckoutYear) %>% 
  unique() %>% 
  arrange(CheckoutYear)

month <- as.data.frame(checkouts_monthly_all) %>% 
  select(CheckoutMonYr) %>% 
  unique() %>% 
  arrange(CheckoutMonYr)

