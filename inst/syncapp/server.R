


#plot_sync = function(dfr, basedate = "2020-05-01", maxdate=Inf, shift=26, winsize=14, na_pad=TRUE)

library(sars2app)
library(shiny)
server = function(input, output) {
 get_data = reactive({
  if (!exists("jh")) jh <<- enriched_jhu_data()
  us = dplyr::filter(jh, alpha2Code=="US")
  usd =  dplyr::filter(us, subset=="deaths")
  usc =  dplyr::filter(us, subset=="confirmed")
  usd2 = usd[-1,]
  usd2$inc = diff(usd$count)
  usc2 = usc[-1,]
  usc2$inc = diff(usc$count)
  data.frame(date=usc2$date, confirmed=usc2$inc, deaths=usd2$inc)
 })
 get_state_data = reactive({
  if (!exists("nyd")) nyd <<- nytimes_state_data()
  nyd = dplyr::filter(nyd, state==input$region)
  nydd =  dplyr::filter(nyd, subset=="deaths")
  nydc =  dplyr::filter(nyd, subset=="confirmed")
  nydd2 = nydd[-1,]
  nydd2$inc = diff(nydd$count)
  nydc2 = nydc[-1,]
  nydc2$inc = diff(nydc$count)
  data.frame(date=nydc2$date, state=nydd2$state, confirmed=pmax(1, nydc2$inc), deaths=pmax(1,nydd2$inc))
 })

 output$sync = renderPlot({
  if (input$region == "USA") dfr = get_data()
  else if (input$region != "USA") dfr = get_state_data()
  validate(need(!is.na(input$winsize), "specify window size > 0"))
  plot_sync(dfr, basedate=input$basedate, maxdate=input$maxdate, shift=input$shift, winsize=input$winsize,
      xlim.in=c(input$basedate, input$maxdate))
 })
 output$msek = renderPlot({
  if (input$region == "USA") dfr = get_data()
  else if (input$region != "USA") dfr = get_state_data()
  validate(need(!is.na(input$winsize), "specify window size > 0"))
  ktotry = 9:35
  msek = function(k) { tmp = plot_sync(dfr, basedate=input$basedate, maxdate=input$maxdate, shift=k, winsize=input$winsize,
      xlim.in=c(input$basedate, input$maxdate)) ;
                       c(k=k, msd=mean((tmp$x-tmp$y)^2, na.rm=TRUE)) }
  mses = lapply(ktotry, msek)
  ks = sapply(mses, function(x) x["k"])
  ms = sapply(mses, function(x) x["msd"])
  plot(ks, ms, pch=19, cex=1.5, main="mean square discrepancy B^k d_t - c_t", xlab="k=lag", ylab="MSD(k)")
 })
}

