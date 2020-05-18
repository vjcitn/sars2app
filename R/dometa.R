#' obtain input to rmeta meta.summaries, metaplot
#' @import rmeta
#' @param nyd tibble, data source assumed to be nytimes_state_data()
#' @param \dots passed to `Arima_by_state`
#' @examples
#' nyd = nytimes_state_data()
#' m1 = run_meta(nyd)
#' rmeta::meta.summaries(m1$drifts, m1$se.drifts)
#' names(m1$drifts) = gsub(".drift", "", names(m1$drifts))
#' nyind = which(names(m1$drifts) %in% c("New York", "New Jersey"))
#' rmeta::meta.summaries(m1$drifts[-nyind], m1$se.drifts[-nyind])
#' o = order(m1$drifts)
#' rmeta::metaplot(m1$drifts[o], m1$se.drifts[o], labels=names(m1$drifts)[o], cex=.7, 
#'   xlab="Infection velocity (CHANGE in number of confirmed cases/day)", ylab="State")
#' segments(rep(-350,46), seq(-49,-4), rep(-50,46), seq(-49,-4), lty=3, col="gray")
#' @export
run_meta = function(nyd, ...) {
 allst = contig_states_dc()
 allarima = lapply(allst, function(x) Arima_by_state(nyd, x, ...))
 names(allarima) = allst
 drifts = sapply(allarima, function(x) coef(x$fit)["drift"])
 searima = function(a) sqrt(a$fit$var.coef["drift", "drift"])
 se.drifts = sapply(allarima, searima)
 list(drifts=drifts, se.drifts=se.drifts)
}

