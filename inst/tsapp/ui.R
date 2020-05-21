
library(shiny)
library(sars2app)
library(dplyr)
library(forecast)

# private
#make_cumul_events = function(count, dates, alpha3="USA", source="NYT", regtag=NA) {
#    ans = list(count = count, dates = dates)
#    attr(ans, "ProvinceState") = regtag
#    attr(ans, "source") = source
#    attr(ans, "alpha3") = alpha3
#    attr(ans, "dtype") = "cumulative"
#    class(ans) = c("cumulative_events", "covid_events")
#    ans 
#}
#form_inc = function(src, regtag) {
# fullsumm = src %>% 
#  select(state,date,count) %>% group_by(date) %>% 
#   summarise(count=sum(count))  # counts by date collapsed over states
# thecum = make_cumul_events(count=fullsumm$count, dates=fullsumm$date, regtag=regtag)
# form_incident_events(thecum)
#}
#
# ui code starts here
 basedate = "2020-02-15" # data prior to this date are dropped completely
 lookback_days = 29 # from present date
 if (!exists(".nyd.global")).nyd.global <<- nytimes_state_data() # cumulative
 if (!exists(".jhu.global")) .jhu.global <<- enriched_jhu_data() # cumulative
 allst = sort(unique(.nyd.global$state))
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("COVID-19 incidence trajectories with simple time series models.
Code derived from the ", a(href="https://seandavi.github.io/sars2pack", "sars2pack"),
" data science and documentation portal.
See 'About' tab for additional details."),
    selectInput("source", "source", choices=c("fullusa", allst), selected="fullusa"),
    radioButtons("excl", "exclude from fullusa", choices=c("no", "New York", "Washington"), selected="no"),
    dateInput("maxdate", "look back from", min="2020-03-15", max=max(lubridate::as_date(.nyd.global$date)),
         value=max(lubridate::as_date(.nyd.global$date))),
    numericInput("MAorder", "MA order", min=0, max=6, value=3), 
    numericInput("ARorder", "AR order", min=0, max=6, value=0), 
    numericInput("Difforder", "Difforder", min=0, max=2, value=1),
    actionButton("stopper", "stop app"),
    width=3),
   mainPanel(
    tabsetPanel(
     tabPanel("traj",
      plotOutput("traj")
      ),
     tabPanel("rept",
      verbatimTextOutput("rept")
      ),
     tabPanel("meta",
      verbatimTextOutput("meta.rept"),
      plotOutput("metaplot", height=700)
      ),
     tabPanel("about",
      helpText("This app was produced to help evaluate a claim that
an apparent decline in COVID-19 incidence for USA as a whole is driven by
the actual decline in incidence in New York.  See ",
a(href="https://www.erinbromage.com/post/the-risks-know-them-avoid-them",
"Erin Bromage's blog post"),"  As a by-product, models can
be fit for any US state.  The data are generated using `nytimes_state_data()`
in sars2pack."),
      helpText("Tab 'traj' is a plot of the last 29 days of incidence reports
with a trace of the time series model as selected using the input controls."),
      helpText("Tab 'rept' reports statistics from the `forecast::Arima` function for
the selected model."),
      )
     )
    )
   )
  )
