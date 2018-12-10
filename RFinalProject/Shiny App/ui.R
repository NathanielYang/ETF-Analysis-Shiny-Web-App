

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("S&P 500 and Selected ETF Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       helpText("Select an index or ETF"),
       selectInput("TickerSymbol1", label = "Choose a ticker symbol to display: ", 
                   choices = list("S&P500", "BJK", "FUD", "IGN", "PAGG", "PBJ", "PBS", "PEJ", "XLP", "XLY"), 
                   selected = "S&P500"),
       sliderInput("variable",
                   "Number of Bins",
                   min = 1,
                   max = 100,
                   value = 10)
    ),
    
    # Show a plot of the selected stock
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram", plotOutput("histoplot")),
                  tabPanel("Normal Plot", plotOutput("qqplot")),
                  tabPanel("Confidence Intervals", verbatimTextOutput("confidenceintervals")),
                  tabPanel("Regression on Time", verbatimTextOutput("regression1"))
      )
    )
  ), 
    sidebarPanel(
      helpText("Select Two indexes or ETFs"),
      selectInput(
        "datasetA", label = "Choose stock A: ", 
        choices = c("BJK", "GSPC", "FUD", "IGN","PAGG", "PBJ","PBS","PEJ","XLP","XLY")),
      selectInput(
        "datasetB", label = "Choose stock B: ", 
        choices = c("BJK", "GSPC", "FUD", "IGN","PAGG", "PBJ","PBS","PEJ","XLP","XLY")),
      actionButton("update", "Update View")
      
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Testing Equality", verbatimTextOutput("ttest")),
                tabPanel("Regression", verbatimTextOutput("regression2"))

    )
  )
  
 )
)