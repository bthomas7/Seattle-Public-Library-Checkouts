library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(DT)
library(shinyjs)
library(RColorBrewer)
library(data.table)


# load datasets
checkouts_monthly_all <- readRDS('data/checkouts_monthly_all.RDS')
checkouts_yearly_all <- readRDS('data/checkouts_yearly_all.RDS')
top_checkouts <-  readRDS('data/top_checkouts.RDS')
top_checkouts_subjects <- readRDS('data/top_checkouts_subjects.RDS')

top_checkouts <- top_checkouts %>% 
  subset(Title != '<Unknown Title>') %>% 
  subset(!(Title %like% 'Uncataloged Folder or Bag')) %>% 
  ungroup()

pal <- brewer.pal(8, "Set2")
pal2 <- brewer.pal(12, "Set3")
custom_pal <- c(pal,pal2[5:5],pal2[10:10],pal2[1:1])

item <- c(sort(unique(checkouts_yearly_all$Material)),sort(unique(checkouts_yearly_all$UsageClass)))
item.color <- custom_pal
names(item.color) <- item

year <- as.data.frame(top_checkouts) %>%
  select(CheckoutYear) %>%
  unique() %>%
  arrange(-CheckoutYear)
