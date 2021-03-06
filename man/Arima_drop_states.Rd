% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Arima.R
\name{Arima_drop_states}
\alias{Arima_drop_states}
\title{multistate exclusion prior to ARIMA}
\usage{
Arima_drop_states(
  src_us,
  src_st,
  states.in = c("New York", "New Jersey"),
  MAorder = 3,
  Difforder = 1,
  basedate = "2020-02-15",
  lookback_days = 29,
  ARorder = 0,
  max_date = NULL,
  ARorder.nat = 3
)
}
\arguments{
\item{src_us}{tibble for national level data like that of enriched_jhu_data()}

\item{src_st}{tibble for state level data like that of nytimes_state_data()}

\item{states.in}{character() vector of state names}

\item{MAorder}{numeric(1) order of moving average component}

\item{Difforder}{numeric(1) differencing order d of ARIMA(p,d,q)}

\item{basedate}{character(1) used by lubridate::as_date to filter away all earlier records}

\item{lookback_days}{numeric(1) only uses this many days from most recent in src}

\item{ARorder}{order of autoregressive component}

\item{max_date}{character(1) or date}

\item{ARorder.nat}{order of autoregressive component for entire nation}
}
\description{
multistate exclusion prior to ARIMA
}
