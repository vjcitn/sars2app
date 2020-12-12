
#plot_sync = function(dfr, basedate = "2020-05-01", maxdate=Inf, shift=26, winsize=14, na_pad=TRUE)

library(sars2app)
library(shiny)
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("'Synchronize' COVID-19 case
and death incidence series"),
   dateInput("basedate", "start", min="2020-04-01",
     max="2020-12-11", value="2020-06-15"),
   dateInput("maxdate", "end", min="2020-05-01",
     max="2020-12-31", value="2020-12-25"),
   sliderInput("shift", "backshift for deaths", min=1, max=40, value=26, step=1),
   numericInput("winsize", "window for series", min=1, max=40, value=14, step=1)
  ),
  mainPanel(
   tabsetPanel(
    tabPanel("plot",
     plotOutput("sync")
    ),
    tabPanel("about",
     helpText("This app uses github.com/vjcitn/sars2app with enriched_jhu_data to
 produce incidence of confirmed COVID-19 and COVID-19 attributed demise.")
    )
   )
  )
 )
)
