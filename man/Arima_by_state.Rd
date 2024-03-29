% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Arima.R
\name{Arima_by_state}
\alias{Arima_by_state}
\title{Use Rob Hyndman's forecast package to estimate drift in ARIMA models}
\usage{
Arima_by_state(
  src,
  state.in = "New York",
  MAorder = NULL,
  Difforder = 1,
  basedate = "2020-02-15",
  lookback_days = 29,
  ARorder = NULL,
  max_date = NULL
)
}
\arguments{
\item{src}{a tibble as returned by nytimes_state_data() or jhu_us_data()}

\item{state.in}{character(1) state name}

\item{MAorder}{numeric(1) order of moving average component}

\item{Difforder}{numeric(1) order of differencing d in ARIMA(p,d,q)}

\item{basedate}{character(1) used by lubridate::as_date to filter away all earlier records}

\item{lookback_days}{numeric(1) only uses this many days from most recent in src}

\item{ARorder}{order of autoregressive component}

\item{max_date}{a date from which to start lookback ... defaults to NULL in which}
}
\value{
instance of S3 class Arima_sars2pack
}
\description{
Use Rob Hyndman's forecast package to estimate drift in ARIMA models
}
\note{
If ARIMA model fails, another attempt is made with lookback days halved.
case the latest available date is used
}
\examples{
nyd = nytimes_state_data()
mb = min_bic(nyd, state.in="New York")
lkny = Arima_by_state(nyd, ARorder=mb$opt["ARord"], MAorder=mb$opt["MAord"])
lkut = Arima_by_state(nyd, "Utah")
lkny
plot(lkny)
usd = jhu_us_data()
lkny2 = Arima_by_state(usd, ARorder=mb$opt["ARord"], MAorder=mb$opt["MAord"])
lkny2
plot(lkny2)
lknyNULL = Arima_by_state(nyd, ARorder=NULL, MAorder=NULL)
lknyNULL
}
