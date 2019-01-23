#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df_subset <- reactive({
    a <- subset(checkouts_yearly_all, UsageClass == input$value3|input$value3=='All')
    return(a)
  })
   
  output$plot <- renderPlotly({ggplotly(ggplot(df_subset(), aes_string(fill=input$value2, y=input$value, x='CheckoutYear')) + 
      geom_bar( stat="identity")+
        theme_light()+
        ggtitle("Checkout Trends",subtitle='2006 - 2018')) })
  
  output$monthPlot <- renderPlotly({
    s <- event_data("plotly_click")
    if (length(s)) {
      df_subset2 <- reactive({
        a <- subset(checkouts_monthly_all,CheckoutYear== s[["x"]]) %>%
          subset(UsageClass == input$value3|input$value3=='All')
        return(a)})
      y_var <- reactive({if_else(input$value=='MatYrlyCheckouts','MatMthlyCheckouts','MonthlyPct')})
      
      
      ggplotly(ggplot(df_subset2(), aes_string(fill=input$value2, y=y_var(), x='CheckoutMonYr')) + 
                 geom_bar( stat="identity")+
                 theme_light()+
                 ggtitle("Monthly Trends for Selected Year: YYYY"))
      
    } else {
      plotly_empty()
    }
  })
  
  wordcloud_rep <- repeatable(wordcloud)

  df_subset_3 <- reactive({
    a <- subset(top_checkouts_subjects_2018, Year== input$year) %>%
      subset(UsageClass == input$value4) %>%
      subset(Material == input$value5)
    return(a)
  })

  output$wordcloud <- renderPlot({
    df <- df_subset_3()
    wordcloud_rep(df$word,df$freq
                  , min.freq=10, max.words=100
                  , random.order=T
                  , rot.per=.15
                  , vfont=c("sans serif","plain"))

  })
    
  }
  
