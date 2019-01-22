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
        theme_light()) })
  
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
                 theme_light())
      
      # vars <- c(s[["x"]], s[["y"]])
      # d <- setNames(mtcars[vars], c("x", "y"))
      # yhat <- fitted(lm(y ~ x, data = d))
      # plot_ly(d, x = ~x) %>%
      #   add_markers(y = ~y) %>%
      #   add_lines(y = ~yhat) %>%
      #   layout(xaxis = list(title = s[["x"]]), 
      #          yaxis = list(title = s[["y"]]), 
      #          showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })
    
  }
  
