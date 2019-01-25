#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  yearly_checkouts <- reactive({
    a <- subset(checkouts_yearly_all, UsageClass == input$usage|input$usage=='All')
    return(a)
  })
  
 
  output$yearPlot <- renderPlotly({ggplotly(ggplot(yearly_checkouts(), aes_string(fill=input$fill, y=input$measure, x='Year')) + 
      geom_bar( stat="identity") +
        theme_light() +
        ylab(if_else(input$measure=='Checkouts', paste("Number of Checkouts (in thousands)"),"Percentage of Total Checkouts"))+
        scale_fill_manual(values=item.color)) })
  
  output$monthPlot <- renderPlotly({
    s <- event_data("plotly_click")
    if (length(s)) {
      monthly_checkouts <- reactive({
        a <- subset(checkouts_monthly_all,Year== s[["x"]]) %>%
          subset(UsageClass == input$usage|input$usage=='All')
        return(a)})
     # y_var <- reactive({if_else(input$measure=='MatYrlyCheckouts','MatMthlyCheckouts','MonthlyPct')})
      
      ggplotly(ggplot(monthly_checkouts(), aes_string(fill=input$fill, y=input$measure, x='MonthYear')) + 
                 geom_bar( stat="identity")+
                 theme_light()+
                 ylab(if_else(input$measure=='Checkouts', paste("Number of Checkouts (in thousands)"),"Percentage of Total Checkouts"))+
                 ggtitle(paste("Monthly Trends for Selected Year:",s[["x"]])) +
                 scale_fill_manual(values=item.color))
      
    } else {
      plotly_empty()
    }
  })
  

 
  observe({
    if (input$usage2 == "Physical") {
      shinyjs::enable(selector = "[type=radio][value=Book]")
      shinyjs::runjs("$('[type=radio][value=Book]').parent().parent().addClass('enabled').css('opacity', 1)")
      shinyjs::disable(selector = "[type=radio][value=EBook]")
      shinyjs::runjs("$('[type=radio][value=EBook]').parent().parent().addClass('disabled').css('opacity', 0.4)")
    }
  })
  
  observe({
    if (input$usage2 == "Digital") {
      shinyjs::enable(selector = "[type=radio][value=EBook]")
      shinyjs::runjs("$('[type=radio][value=EBook]').parent().parent().addClass('enabled').css('opacity', 1)")
      shinyjs::disable(selector = "[type=radio][value=Book]")
      shinyjs::runjs("$('[type=radio][value=Book]').parent().parent().addClass('disabled').css('opacity', 0.4)")
    }
  })
  
  top_checkouts_wc <- reactive({
   a <- subset(top_checkouts_subjects, Year== input$year) %>%
     subset(UsageClass == input$usage2) %>%
     subset(Material == input$material) %>% 
     select(word,freq) %>% 
     subset(word != if_else(input$exclude==TRUE,'juvenile','0')) %>% 
     head(n=100)
   return(a)
   })
  

  output$wordcloud <- renderWordcloud2({
    df <- top_checkouts_wc()
    wc <- wordcloud2(df, size =1, color=rep_len(pal, nrow(df))
                     ,minRotation = -pi/2,maxRotation = -pi/2,rotateRatio = 0.4,gridSize = '5')
  })
  
  top_checkouts_dt <- reactive({
    a <- subset(top_checkouts, CheckoutYear== input$year) %>%
      subset(UsageClass == input$usage2) %>%
      subset(Material == input$material) %>% 
      select(Title,Creator,Subjects,`Number of Checkouts`=TotalCheckouts)
    return(a)
  })
    
  output$top_titles <- DT::renderDataTable({
    top_checkouts_dt()
    
  })
    
  }
  
