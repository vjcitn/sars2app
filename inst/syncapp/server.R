


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
  uss = rbind(usd2, usc2)
  data.frame(date=usc2$date, confirmed=usc2$inc, deaths=usd2$inc)
 })

 output$sync = renderPlot({
  dfr = get_data()
  print(head(dfr))
  validate(need(!is.na(input$winsize), "specify window size > 0"))
  plot_sync(dfr, basedate=input$basedate, maxdate=input$maxdate, shift=input$shift, winsize=input$winsize)
 })
}

