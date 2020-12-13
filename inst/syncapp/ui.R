
#plot_sync = function(dfr, basedate = "2020-05-01", maxdate=Inf, shift=26, winsize=14, na_pad=TRUE)

library(sars2app)
library(shiny)

sts <- c("USA", "Alabama", "Alaska", "Arizona", "Arkansas", "California", 
"Colorado", "Connecticut", "Delaware", "District of Columbia", 
"Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", 
"Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
"Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
"Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
"New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
"Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
"Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", 
"Tennessee", "Texas", "Utah", "Vermont", "Virgin Islands", "Virginia", 
"Washington", "West Virginia", "Wisconsin", "Wyoming")

ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("'Synchronize' COVID-19 case
and death incidence series"),
   selectInput("region", "region", choices=sts, selected="USA"),
   dateInput("basedate", "start", min="2020-02-01",
     max="2020-12-11", value="2020-06-15"),
   dateInput("maxdate", "end", min="2020-05-01",
     max="2020-12-31", value="2020-12-25"),
   sliderInput("shift", "backshift for deaths", min=1, max=40, value=26, step=1,
     animate=TRUE),
   numericInput("winsize", "window for series", min=1, max=40, value=14, step=1)
  ),
  mainPanel(
   tabsetPanel(
    tabPanel("plot",
     plotOutput("sync")
    ),
    tabPanel("about",
     helpText("This app uses github.com/vjcitn/sars2app with enriched_jhu_data to
 produce incidence of confirmed COVID-19 and COVID-19 attributed demise.  cIFR
 denotes 'confirmed infection fatality rate.'")
    )
   )
  )
 )
)
