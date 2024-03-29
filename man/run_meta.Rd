% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dometa.R
\name{run_meta}
\alias{run_meta}
\title{obtain input to rmeta meta.summaries, metaplot}
\usage{
run_meta(nyd, opt_parms, ...)
}
\arguments{
\item{nyd}{tibble, data source assumed to be nytimes_state_data()}

\item{opt_parms}{list, such as that produced by \code{min_bic_all_states()}}

\item{\dots}{passed to \code{Arima_by_state}}
}
\description{
obtain input to rmeta meta.summaries, metaplot
}
\examples{
nyd = nytimes_state_data()
data(min_bic_2022_01_01)
m1 = run_meta(nyd, opt_parms=min_bic_2022_01_01) # must be relatively current
rmeta::meta.summaries(m1$drifts, m1$se.drifts)
names(m1$drifts) = gsub(".drift", "", names(m1$drifts))
nyind = which(names(m1$drifts) \%in\% c("New York", "New Jersey"))
rmeta::meta.summaries(m1$drifts[-nyind], m1$se.drifts[-nyind])
o = order(m1$drifts)
rmeta::metaplot(m1$drifts[o], m1$se.drifts[o], labels=names(m1$drifts)[o], cex=.7, 
  xlab="Infection velocity (CHANGE in number of confirmed cases/day)", ylab="State")
segments(rep(-350,46), seq(-49,-4), rep(-50,46), seq(-49,-4), lty=3, col="gray")
}
