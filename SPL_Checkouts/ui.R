#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

ui <- fluidPage(shinyjs::useShinyjs(),
                theme = shinytheme("flatly"),
                
navbarPage(  
title='Explore Seattle Public Library Checkouts  ',
id = 'nav',
windowTitle = 'Explore SPL Checkouts Data',

tabPanel('Checkout Trends',
  sidebarLayout(
    sidebarPanel(
    #  fluidRow(
    #column(4, radioButtons('usage',"Usage Class",choices=c('All','Physical','Digital'),selected = 'All')),
    #column(4, radioButtons('fill',"Fill Category",choices=c('UsageClass',"Material"),selected = 'UsageClass')),
    #column(4, radioButtons('measure',"Measure",choices=c('Checkouts','Percentage'),selected = 'Checkouts')))),

      radioButtons('usage',"Usage Class",choices=c('All','Physical','Digital'),selected = 'All'),
      br(),
      radioButtons('fill',"Fill Category",choices=c('UsageClass',"Material"),selected = 'UsageClass'),
      br(),
      radioButtons('measure',"Measure",choices=c('Checkouts','Percentage'),selected = 'Checkouts')
      , width = 2),
    mainPanel(
      fluidRow(
        h3('Yearly Trends in Checkouts'),
        h5('2006 - 2018'),
        plotlyOutput("yearPlot",height = "500px")),
  
      fluidRow(br(),
        plotlyOutput('monthPlot',height = "500px"))))
  ),



tabPanel('Explore Top Checkouts',
  sidebarLayout(
    sidebarPanel(
  # fluidRow(
  #   column(4, selectInput('year','Year',choices = year, selected='2018')),
  #   column(4, radioButtons('usage2','Usage Class',choices=c('Physical','Digital'),selected = 'Physical')),
  #   column(4, radioButtons('material','Material Type',choices=c('Book','EBook','Audiobook'),selected = 'Book')))
      selectInput('year','Year',choices = year, selected='2018'),
      br(),
      radioButtons('usage2','Usage Class',choices=c('Physical','Digital'),selected = 'Physical'),
      br(),
      radioButtons('material','Material Type',choices=c('Book','EBook','Audiobook'),selected = 'Book')
      , width = 2),
    mainPanel(
      tabsetPanel(
        tabPanel("WordCloud",
        fluidRow(
        h3("Top 100 Words in Subject of Top Checkouts"),
        column(12,
               wordcloud2Output('wordcloud',height="600px"))),
      fluidRow(column(3,offset = 9,
                      checkboxInput("exclude","Exclude 'juvenile' for better viewing",FALSE))
               )),
      tabPanel("Top Titles",
      fluidRow(
        h3("Top 1,000 Titles Checked Out in Selected Categories"),
        DT::dataTableOutput('top_titles')
        ))))
  ))
  )
  
)
#)

