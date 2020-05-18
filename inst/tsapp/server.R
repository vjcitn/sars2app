library(shiny)
library(sars2app)
library(dplyr)
library(forecast)


 basedate = "2020-03-15" # data prior to this date are dropped completely
 lookback_days = 29 # from present date
 if (!exists(".nyd.global")) .nyd.global <<- nytimes_state_data() # cumulative
 if (!exists(".jhu.global")) .jhu.global <<- enriched_jhu_data() # cumulative
 allst = sort(unique(.nyd.global$state))

 server = function(input, output) {
  dofit = reactive({
   if (input$source == "fullusa" & input$excl == "no") curfit = Arima_nation(.jhu.global, Difforder=input$Difforder, MAorder=input$MAorder)
   else if (input$source == "fullusa" & input$excl != "no") 
        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=input$excl, Difforder=input$Difforder, MAorder=input$MAorder)
   else if (input$source != "fullusa") curfit = Arima_by_state(.nyd.global, state.in=input$source, Difforder=input$Difforder, MAorder=input$MAorder)
   list(fit=curfit, pred=fitted.values(forecast(curfit$fit)), tsfull=curfit$tsfull, dates29=curfit$dates29)
   })
  output$traj = renderPlot({
   ans = dofit()
   plot(ans$fit)
   })
  output$rept = renderPrint({ 
    ans = dofit()
    ans$fit
   })
  observeEvent(input$stopper, {
       ans = dofit()
       stopApp(returnValue=ans$fit)
       })  

  }
