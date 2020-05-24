library(shiny)
library(sars2app)
library(dplyr)
library(forecast)


 basedate = "2020-02-15" # data prior to this date are dropped completely
 lookback_days = 29 # from present date
 if (!exists(".nyd.global")) .nyd.global <<- nytimes_state_data() # cumulative
 if (!exists(".jhu.global")) .jhu.global <<- enriched_jhu_data() # cumulative
 allst = sort(unique(.nyd.global$state))
 data(list="min_bic_2020_05_20", package="sars2app")

 server = function(input, output) {
  dofit = reactive({
   if (input$source == "fullusa" & input$excl == "no") curfit = Arima_nation(.jhu.global, Difforder=input$Difforder, MAorder=input$MAorder, ARorder=input$ARorder, max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl != "no") 
        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=input$excl, Difforder=input$Difforder, MAorder=input$MAorder, ARorder=input$ARorder, max_date=input$maxdate)
   else if (input$source != "fullusa") curfit = Arima_by_state(.nyd.global, state.in=input$source, Difforder=input$Difforder, MAorder=input$MAorder, ARorder=input$ARorder, max_date=input$maxdate)
   validate(need(!inherits(curfit, "try-error"), "please alter AR or MA order"))
   list(fit=curfit, pred=fitted.values(forecast(curfit$fit)), tsfull=curfit$tsfull, dates29=curfit$dates29)
   })
  output$traj = renderPlot({
   ans = dofit()
   validate(need(!inherits(ans$fit, "try-error"), "please alter AR order"))
   plot(ans$fit, main="Incidence and time series model for selected source/parameters")
   })
  output$Rtplot = renderPlot({
   ans = dofit()
   ee = est_Rt(ans$fit)
   plot(ee, main="EpiEstim R[t] using MCMC-based Gamma model for SI")
   })
  output$rept = renderPrint({ 
    ans = dofit()
   validate(need(!inherits(ans$fit, "try-error"), "please alter AR order"))
    ans$fit
   })
  output$tsdiag = renderPlot({ 
    ans = dofit()
   validate(need(!inherits(ans$fit, "try-error"), "please alter AR order"))
    tsdiag(ans$fit$fit)
   })
  dometa = reactive({
    run_meta(.nyd.global, opt_parms=min_bic_2020_05_20, Difforder=input$Difforder, 
            max_date=input$maxdate)  # note that AR/MA parms from opt_parms
  })
  output$meta.rept = renderPrint({ 
    m1 = dometa()
    summ1 = rmeta::meta.summaries(m1$drifts, m1$se.drifts)
    names(m1$drifts) = gsub(".drift", "", names(m1$drifts))
    nyind = which(names(m1$drifts) %in% c("New York", "New Jersey"))
    summ2 = rmeta::meta.summaries(m1$drifts[-nyind], m1$se.drifts[-nyind])
    list(overall=summ1, exclNYNJ=summ2)
   })
  output$metaplot = renderPlot({
    m1 = dometa()
    names(m1$drifts) = gsub(".drift", "", names(m1$drifts))
    o = order(m1$drifts)
    rmeta::metaplot(m1$drifts[o], m1$se.drifts[o], labels=names(m1$drifts)[o], cex=.7, 
      xlab="Infection velocity (CHANGE in number of confirmed cases/day)", ylab="State")
    segments(rep(-350,46), seq(-49,-4), rep(-50,46), seq(-49,-4), lty=3, col="gray")
   })
  observeEvent(input$stopper, {
       ans = dofit()
       validate(need(!inherits(ans$fit, "try-error"), "please alter AR order"))
       stopApp(returnValue=ans$fit)
       })  

  }
