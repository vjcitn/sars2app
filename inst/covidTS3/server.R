library(shiny)
library(sars2app)
library(dplyr)
library(forecast)
library(shinytoastr)


 basedate = "2020-02-15" # data prior to this date are dropped completely
 lookback_days = 29 # from present date
 if (!exists(".nyd.global")) .nyd.global <<- nytimes_state_data() # cumulative
 if (!exists(".jhu.global")) .jhu.global <<- jhu_data() # cumulative
 allst = sort(unique(.nyd.global$state))
 data(list="min_bic_2022_01_01", package="sars2app")

 server = function(input, output, session) {
  dofit = reactive({
   toastr_info("optimizing BIC; computing ARIMA model")
   if (input$source == "fullusa" & input$excl == "none") curfit = Arima_nation(.jhu.global, Difforder=1, MAorder=NULL, ARorder=NULL, max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl == "New York") 
        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=input$excl, max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl == "TX") 
        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=c("Texas"), 
                 max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl == "TX,CA")
        curfit = Arima_drop_state(.jhu.global, .nyd.global, 
                   state.in=c("Texas", "California"),
                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
   else if (input$source == "fullusa" & input$excl == "TX,CA,GA")
        curfit = Arima_drop_states(.jhu.global, .nyd.global, 
                   states.in=c("Texas", "California", "Georgia"),
                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
   else if (input$source == "fullusa" & input$excl == "TX,CA,GA,FL")
        curfit = Arima_drop_states(.jhu.global, .nyd.global, 
                   states.in=c("Texas", "Califonia", "Georgia", "Florida"),
                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
#   else if (input$source == "fullusa" & input$excl == "New York") 
#        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=input$excl, max_date=input$maxdate,
#           MAorder=NULL, ARorder=NULL)
#   else if (input$source == "fullusa" & input$excl == "NY,NJ")
#        curfit = Arima_drop_states(.jhu.global, .nyd.global, states.in=c("New York", "New Jersey"), 
#                   max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
#   else if (input$source == "fullusa" & input$excl == "IL")
#        curfit = Arima_drop_state(.jhu.global, .nyd.global, 
#                   state.in=c("Illinois"),
#                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
#   else if (input$source == "fullusa" & input$excl == "MI,MN,IL")
#        curfit = Arima_drop_states(.jhu.global, .nyd.global, 
#                   states.in=c("Michigan", "Minnesota", "Illinois"),
#                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
#   else if (input$source == "fullusa" & input$excl == "MI,MN,IL,OH,CO")
#        curfit = Arima_drop_states(.jhu.global, .nyd.global, 
#                   states.in=c("Michigan", "Minnesota", "Illinois", "Ohio", "Colorado"),
#                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
   else if (input$source != "fullusa") 
        curfit = Arima_by_state(.nyd.global, state.in=input$source, 
                   Difforder=1, MAorder=NULL, 
                       ARorder=NULL, max_date=input$maxdate)
   validate(need(!inherits(curfit, "try-error"), "please alter AR or MA order (1)"))
   validate(need(all(is.finite(coef(curfit$fit))), "non-finite coefficient produced; please alter AR or MA order"))
   validate(need(all(diag(curfit$fit$var.coef)>0), "covariance matrix has diagonal element < 0; please alter AR or MA order"))
   list(fit=curfit, pred=fitted.values(forecast(curfit$fit)), tsfull=curfit$tsfull, dates29=curfit$dates29)
   })
# following can prevent needless refitting of R[t] model, which does not depend on ARIMA tuning
  dofit_simple = reactive({
   if (input$source == "fullusa" & input$excl == "none") curfit = Arima_nation(.jhu.global, max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl == "New York") 
        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=input$excl, max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl == "TX") 
        curfit = Arima_drop_state(.jhu.global, .nyd.global, state.in=c("Texas"), 
                 max_date=input$maxdate)
   else if (input$source == "fullusa" & input$excl == "TX,CA")
        curfit = Arima_drop_state(.jhu.global, .nyd.global, 
                   state.in=c("Texas", "California"),
                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
   else if (input$source == "fullusa" & input$excl == "TX,CA,GA")
        curfit = Arima_drop_states(.jhu.global, .nyd.global, 
                   states.in=c("Texas", "California", "Georgia"),
                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
   else if (input$source == "fullusa" & input$excl == "TX,CA,GA,FL")
        curfit = Arima_drop_states(.jhu.global, .nyd.global, 
                   states.in=c("Texas", "Califonia", "Georgia", "Florida"),
                       max_date=input$maxdate, MAorder=NULL, ARorder=NULL)
   else if (input$source != "fullusa") curfit = Arima_by_state(.nyd.global, state.in=input$source, max_date=input$maxdate)
   validate(need(!inherits(curfit, "try-error"), "please alter AR or MA order (2)"))
   list(fit=curfit, pred=fitted.values(forecast(curfit$fit)), tsfull=curfit$tsfull, dates29=curfit$dates29)
   })
  output$traj = renderPlot({
   ans = dofit()
   validate(need(!inherits(ans$fit, "try-error"), "please alter AR order"))
   plot(ans$fit, main="Incidence and time series model for selected source/parameters")
   })
  output$Rtplot = renderPlot({
   ans = dofit_simple()
   toastr_info("estimating R[t]")
   ee = est_Rt(ans$fit)
   plot(ee, main="EpiEstim R[t] using MCMC-based Gamma model for SI")
   })
  output$rept = renderPrint({ 
    ans = dofit()
   validate(need(!inherits(ans$fit, "try-error"), "please alter AR or MA order (3)"))
    ans$fit
   })
  output$tsdiag = renderPlot({ 
    ans = dofit()
   validate(need(!inherits(ans$fit, "try-error"), "please alter AR or MA order (4)"))
    tsdiag(ans$fit$fit)
   })
  dometa = reactive({
    z = try(run_meta(.nyd.global, opt_parms=min_bic_2022_01_01, Difforder=1,
            max_date=input$maxdate), silent=TRUE)  # note that AR/MA parms from opt_parms
    if (inherits(z, "try-error")) {
         toastr_info("for selected date, we need to rerun state-specific BICs... hold on")
         suppressMessages({
         mball = min_bic_all_states(.nyd.global, max_date = input$maxdate)
         })
         z = run_meta(.nyd.global, opt_parms=mball, Difforder=1, max_date=input$maxdate)
         }
    z
  })
  output$meta.rept = renderPrint({ 
    m1 = dometa()
    summ1 = rmeta::meta.summaries(m1$drifts, m1$se.drifts)
    names(m1$drifts) = gsub(".drift", "", names(m1$drifts))
    nyind = which(names(m1$drifts) %in% c("Texas"))
    summ2 = rmeta::meta.summaries(m1$drifts[-nyind], m1$se.drifts[-nyind])
    augst=c("Texas", "California")
    augind = which(names(m1$drifts) %in% augst)
    summ3 = rmeta::meta.summaries(m1$drifts[-augind], m1$se.drifts[-augind])
    augst2=c("Texas", "California", "Georgia", "Florida")
    augind2 = which(names(m1$drifts) %in% augst2)
    summ4 = rmeta::meta.summaries(m1$drifts[-augind2], m1$se.drifts[-augind2])
    augst3=c("Texas", "California", "Georgia", "Florida", "South Carolina",
         "Arizona", "North Carolina", "New York") 
    augind3 = which(names(m1$drifts) %in% augst3)
    summ5 = rmeta::meta.summaries(m1$drifts[-augind3], m1$se.drifts[-augind3])
    list(overall=summ1) #, exclTX=summ2, exclTXCA=summ3, exclTXCAGAFL=summ4,
        #exclmo=summ5)
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
