library(shiny)


shinyServer <- function(input, output, session) {
   
output$histoplot <- renderPlot({
  
stock <- input$TickerSymbol1

    if(stock == "S&P500"){
      hist(logreturn_SP, 
           main="Histogram for SP Log Returns", 
           xlab="Log Returns", 
           col="blue",
           breaks = seq(-0.07, 0.07, by=0.14/input$variable)
      )
    }
   
   else if(stock == "BJK"){
     hist(logreturn_BJK, 
          main="Histogram for BJK Log Returns", 
          xlab="Log Returns", 
          col="blue",
          breaks = seq(-0.13, 0.08, by=0.21/input$variable)
     )}
   
    else if(stock == "FUD"){
      hist(logreturn_FUD, 
           main="Histogram for FUD Log Returns", 
           xlab="Log Returns", 
           col="blue",
           breaks = seq(-0.18, 0.08, by=0.26/input$variable)
      )}
    
    else if(stock == "IGN"){
      
      hist(logreturn_IGN, 
           main="Histogram for IGN Log Returns", 
           xlab="Log Returns", 
           col="blue",
           breaks = seq(-0.06, 0.06, by=0.12/input$variable)
      )}
    
    else if(stock == "PAGG"){
      
      hist(logreturn_PAGG, 
           main="Histogram for PAGG Log Returns", 
           xlab="Log Returns", 
           col="blue",
           breaks = seq(-0.18, 0.08, by=0.26/input$variable)
      )}
      
    
    else if(stock == "PBS"){
      hist(logreturn_PBS, 
           main="Histogram for PBS Log Returns", 
           xlab="Log Returns", 
           col="blue",
           breaks = seq(-0.1, 0.1, by=0.2/input$variable)
      )}

   else if(stock == "PEJ"){
     
     hist(logreturn_PEJ, 
          main="Histogram for PEJ Log Returns", 
          xlab="Log Returns", 
          col="blue",
          breaks = seq(-0.12, 0.07, by=0.19/input$variable)
     )}
   
   else if(stock == "XLP"){
     
     hist(logreturn_XLP, 
          main="Histogram for XLP Log Returns", 
          xlab="Log Returns", 
          col="blue",
          breaks = seq(-0.05, 0.05, by=0.1/input$variable)
          )}
    
    else if(stock == "XLY"){
      
      hist(logreturn_XLY, 
           main="Histogram for XLY Log Returns", 
           xlab="Log Returns", 
           col="blue",
           breaks = seq(-0.07, 0.07, by=0.14/input$variable)
      )}

  })
  
  
  output$qqplot <- renderPlot({
    stock <- input$TickerSymbol1
    
    if(stock == "S&P500"){
      qqnorm(logreturn_SP)
      qqline(logreturn_SP) 
      
    }
    
    else if(stock == "BJK"){
      qqnorm(logreturn_BJK) 
      qqline(logreturn_BJK)}
    
    else if(stock == "FUD"){
      qqnorm(logreturn_FUD)
      qqline(logreturn_FUD)}
    
    else if(stock == "IGN"){
      
      qqnorm(logreturn_IGN) 
      qqline(logreturn_IGN)}
    
    else if(stock == "PAGG"){
      
      qqnorm(logreturn_PAGG)
      qqline(logreturn_PAGG)}
    
    
    else if(stock == "PBS"){
      qqnorm(logreturn_PBS)
      qqline(logreturn_PBS)}
    
    else if(stock == "PEJ"){
      qqnorm(logreturn_PEJ) 
      qqline(logreturn_PEJ)}
    
    else if(stock == "PBJ"){
      qqnorm(logreturn_PBJ) 
      qqline(logreturn_PBJ)}
    
    else if(stock == "XLP"){
      
      qqnorm(logreturn_XLP) 
      qqline(logreturn_XLP)}
    
    else if(stock == "XLY"){
      
      qqnorm(logreturn_XLY) 
      qqline(logreturn_XLY)}
    
  })

  
  output$confidenceintervals <- renderPrint({
    
    stock <- input$TickerSymbol1
    
    if(stock == "BJK"){
      c(mean = mean(logreturn_BJK), lower = lower_BJK_mean, upper = upper_BJK_mean, variance = var(logreturn_BJK), lower = lower_BJK_var, upper = upper_BJK_var)
      }
    else if(stock == "S&P500"){
      c(mean = mean(logreturn_SP), lower = lower_SP_mean, upper = upper_SP_mean, variance = var(logreturn_SP), lower = lower_SP_var, upper = upper_SP_var)
      }
    else if(stock == "FUD"){
      c(mean = mean(logreturn_FUD), lower = lower_FUD_mean, upper = upper_FUD_mean, variance = var(logreturn_FUD), lower = lower_FUD_var, upper = upper_FUD_var)
      }
    else if(stock == "IGN"){
      c(mean = mean(logreturn_IGN), lower = lower_IGN_mean, upper = upper_IGN_mean, variance = var(logreturn_IGN), lower = lower_IGN_var, upper = upper_IGN_var)
      }
    else if(stock == "PAGG"){
      c(mean = mean(logreturn_PAGG), lower = lower_PAGG_mean, upper = upper_PAGG_mean, variance = var(logreturn_PAGG), lower = lower_PAGG_var, upper = upper_PAGG_var)
        }
    else if(stock == "PBJ"){
      c(mean = mean(logreturn_PBJ), lower = lower_PBJ_mean, upper = upper_PBJ_mean, variance = var(logreturn_PBJ), lower = lower_PBJ_var, upper = upper_PBJ_var)
      }
    else if(stock == "PBS"){ 
      c(mean = mean(logreturn_PBS), lower = lower_PBS_mean, upper = upper_PBS_mean, variance = var(logreturn_PBS), lower = lower_PBS_var, upper = upper_PBS_var)
      }
    else if(stock == "PEJ"){ 
      c(mean = mean(logreturn_PEJ), lower = lower_PEJ_mean, upper = upper_PEJ_mean, variance = var(logreturn_PEJ), lower = lower_PEJ_var, upper = upper_PEJ_var)
      }
    else if(stock == "XLP"){ 
      c(mean = mean(logreturn_XLP), lower = lower_XLP_mean, upper = upper_XLP_mean, variance = var(logreturn_XLP), lower = lower_XLP_var, upper = upper_XLP_var)
      }
    else if(stock == "XLY"){
      c(mean = mean(logreturn_XLY), lower = lower_XLY_mean, upper = upper_XLY_mean, variance = var(logreturn_XLY), lower = lower_FUD_var, upper = upper_FUD_var)
      }
    
  })
  
  output$regression1 <- renderPrint({
    
    stock <- input$TickerSymbol1
    
  if(stock == "BJK"){
    summary(lm(tslogreturn_BJK~tstime_BJK))}
  else if(stock == "S&P500"){
    summary(lm(tslogreturn_SP~tstime_SP))}
  else if(stock == "FUD"){
    summary(lm(tslogreturn_FUD~tstime_FUD))}
  else if(stock == "IGN"){
    summary(lm(tslogreturn_IGN~tstime_IGN))}
  else if(stock == "PAGG"){
    summary(lm(tslogreturn_PAGG~tstime_PAGG))}
  else if(stock == "PBJ"){ 
    summary(lm(tslogreturn_PBJ~tstime_PBJ))}
  else if(stock == "PBS"){ 
    summary(lm(tslogreturn_PBS~tstime_PBS))}
  else if(stock == "PEJ"){ 
    summary(lm(tslogreturn_PEJ~tstime_PEJ))}
  else if(stock == "XLP"){ 
    summary(lm(tslogreturn_XLP~tstime_XLP))}
  else if(stock == "XLY"){
    summary(lm(tslogreturn_XLY~tstime_XLY))}
    
  })
  
  
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
  
  output$ttest <- renderPrint({
    dataset1 <- datasetInput1()
    dataset2 <- datasetInput2()
    t.test(dataset1$logreturn,dataset2$logreturn, alternative = c("less"), var.equal = FALSE)
    
    })
  
  output$regression2 <- renderPrint({
  
    dataset1 <- datasetInput1()
    dataset2 <- datasetInput2()
    summary(lm(dataset1$logreturn~dataset2$logreturn))
  })
  

   
    #stop the app by closing windows
    session$onSessionEnded(function() { stopApp() })
}


