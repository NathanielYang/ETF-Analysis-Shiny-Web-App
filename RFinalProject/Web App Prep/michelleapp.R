shinyApp(
  ui = tagList(
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "shinythemes",
      tabPanel("Navbar 1",
               sidebarPanel(
                 fileInput("file", "File input:"),
                 textInput("txt", "Text input:", "general"),
                 sliderInput("slider", "Slider input:", 1, 100, 30),
                 tags$h5("Deafult actionButton:"),
                 actionButton("action", "Search"),
                 
                 tags$h5("actionButton with CSS class:"),
                 actionButton("action2", "Action button", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Tab 1",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("txtout"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Tab 2", "This panel is intentionally left blank"),
                   tabPanel("Tab 3", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Michelle", 
        sidebarPanel(
        selectInput(
          "datasetA", label = "Choose stock A: ", 
          choices = c("BJK", "GSPC", "FUD", "IGN","PAGG", "PBJ","PBS","PEJ","XLP","XLY")),
        selectInput(
          "datasetB", label = "Choose stock B: ", 
          choices = c("BJK", "GSPC", "FUD", "IGN","PAGG", "PBJ","PBS","PEJ","XLP","XLY")),
        actionButton("update", "Update View")
        ),
        
        mainPanel(
          h4("Test Equality"),
          verbatimTextOutput("summary"),
          h4("Regression"),
          verbatimTextOutput("reg")
          
        )
              ),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
  ),
  server = function(input, output, session) {
    datasetInput1 <- eventReactive(input$update, {
      switch(input$datasetA,
             "BJK" = BJK,
             "GSPC" = `^GSPC`,
             "FUD" = FUD,
             "IGN" = IGN,
             "PAGG" = PAGG,
             "PBJ" = PBJ,
             "PBS" = PBS,
             "PEJ" = PEJ,
             "XLP" = XLP,
             "XLY" = XLY)
    }, ignoreNULL = FALSE)
    datasetInput2 <- eventReactive(input$update, {
      switch(input$datasetB,
             "BJK" = BJK,
             "GSPC" = `^GSPC`,
             "FUD" = FUD,
             "IGN" = IGN,
             "PAGG" = PAGG,
             "PBJ" = PBJ,
             "PBS" = PBS,
             "PEJ" = PEJ,
             "XLP" = XLP,
             "XLY" = XLY)
    }, ignoreNULL = FALSE)
    
    output$summary <- renderPrint({
      dataset1 <- datasetInput1()
      dataset2 <- datasetInput2()
      t.test(dataset1$logreturn,dataset2$logreturn, alternative = c("less"), var.equal = FALSE)
    })
    output$reg <- renderPrint({
      dataset1 <- datasetInput1()
      dataset2 <- datasetInput2()
      summary(lm(dataset1$logreturn~dataset2$logreturn))
    })
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
    session$onSessionEnded(function() { stopApp() })
    }
)
