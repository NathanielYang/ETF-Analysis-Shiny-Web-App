library(shiny)

shinyApp(
  ui = tagList(
    
    navbarPage(
      
      tabPanel("Choose One",
               sidebarPanel(
                 helpText("Select an index or ETF"),
                 
                 selectInput("Ticker Symbol", label = "Choose a ticker symbol to display: ", 
                             choices = list("S&P500", "BJK", "FUD", "IGN", "PAGG", "PBJ", "PBS", "PEJ", "XLP", "XLY"), 
                             selected = "S&P500"),
                 
                 sliderInput("variable", "Number of Bins", min = 1, max = 100, value = 10)
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Histogram", plotOutput("histoplot")),
                             tabPanel("Normal Plot", plotOutput("qqplot")),
                             tabPanel("Confidence Intervals", verbatimTextOutput("confidenceintervals")),
                             tabPanel("Regression on Time", tableOutput("regression"))
                 )
               )
      ),
      tabPanel("Navbar 2", "This panel is intentionally left blank")
      
    )
  ),
  
  server = function(input, output, session) {
    output$histoplot <- renderPlot({
})
    output$qqplot <- renderPlot({
})
    output$confidenceintervals <- renderPrint({
})
    output$regression <- renderPrint({
})
    output$testequal <- renderPrint({
})
    output$regression2 <- renderPrint({
})
    #stop the app by closing windows
    session$onSessionEnded(function() { stopApp() })
  }
)
