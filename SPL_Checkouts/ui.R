#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  fluidRow(
    column(3, radioButtons('value',"Value",choices=c('MatYrlyCheckouts',"YearlyPct"),selected = 'YearlyPct')),
    column(3, radioButtons('value2',"Value2",choices=c('UsageClass',"Material"),selected = 'Material')),
    column(3, radioButtons('value3',"Usage Class",choices=c('All','Physical','Digital'),selected = 'All'))),
  fluidRow(
    plotlyOutput("plot")),
  fluidRow(
    plotlyOutput('monthPlot')),
  fluidRow(
    column(3, radioButtons('value4','Usage Class',choices=c('Physical','Digital'),selected = 'Physical')),
    column(3, radioButtons('value5','Material',choices=c('Book','EBook','Audiobook'),selected = 'Book')),
    column(3, selectInput('year','Year',choices = year, selected='2018'))
    ),
  fluidRow(
    plotOutput('wordcloud'))
  
)

