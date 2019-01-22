#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # We MUST load the ECharts javascript library in advance
  radioButtons('value',"Value",choices=c('MatYrlyCheckouts',"YearlyPct"),selected = 'YearlyPct'),
  radioButtons('value2',"Value2",choices=c('UsageClass',"Material"),selected = 'Material'),
  radioButtons('value3',"Value3",choices=c('All','Physical','Digital'),selected = 'All'),
  plotlyOutput("plot"),
  plotlyOutput('monthPlot')
)

