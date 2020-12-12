#' shift the death records in dfr `shift` days into the past
#' @param dfr a data.frame with date, deaths, confirmed
#' @param shift numeric(1) coerced to integer
#' @examples
#' ind = data.frame(date=1:5, confirmed=letters[1:5], deaths=LETTERS[1:5])
#' resync(ind, 2)
#' @export
resync = function(dfr, shift=26) {
   shift = as.integer(shift)
   nrec = nrow(dfr)
   dat = dfr[seq_len(nrec-shift),]
   dat$deaths = dfr$deaths[-seq_len(shift)]
   dat
}
#' display synchronized death and confirmed case series
#' @importFrom dplyr filter
#' @importFrom runner mean_run
#' @param dfr data.frame with columns 'date', 'deaths', 'confirmed', where the latter are 'incident',
#' formed by differencing the cumulative counts provided in sars2pack
#' @param basedate minimum date to consider
#' @param maxdate maximum date to use, Inf for no limit relative to available dates
#' @param shift number of days to backshift death counts, coerced to integer
#' @param winsize size of running window for averaging incident counts for plotting
#' @param na_pad logical(1) passed to runner::mean_run
#' @note The 'IFR' is crudely estimated as exp(mean(log(deaths)-log(confirmed))).
#' 
#' @examples
#' if (!exists("jh")) jh = enriched_jhu_data()
#' requireNamespace("dplyr")
#' us = dplyr::filter(jh, alpha2Code=="US")
#' usd =  dplyr::filter(us, subset=="deaths")
#' usc =  dplyr::filter(us, subset=="confirmed")
#' usd2 = usd[-1,]
#' usd2$inc = diff(usd$count)
#' usc2 = usc[-1,]
#' usc2$inc = diff(usc$count)
#' uss = rbind(usd2, usc2)
#' uscd = data.frame(date=usc2$date, confirmed=usc2$inc, deaths=usd2$inc)
#' plot_sync(uscd)
#' @export
plot_sync = function(dfr, basedate = "2020-05-01", maxdate=Inf, shift=26, winsize=14, na_pad=TRUE) {
   dfr = dplyr::filter(dfr, date >= basedate & date < maxdate)
   dfr = resync( dfr, shift )
   phi = mean(log(dfr$deaths)-log(dfr$confirmed))
   smc = mean_run(dfr$confirmed, winsize, na_pad=na_pad)
   smd = mean_run(dfr$deaths, winsize, na_pad=na_pad)
   plot(I(log(smc)+phi)~dfr$date, type="l", lty=1, lwd=2, col="gray", xlab="Date", ylab="log 'incidence'")
   lines(log(smd)~dfr$date, lty=2, lwd=2)
   legend(x="topleft", lty=1:2, lwd=2, legend=c(paste("log confirmed", round(phi,3), "[IFR", 100*round(exp(phi),3), "%]"), 
          paste("deaths (shifted by", shift, "days)")))
}
#
#lines(log(deaths)~date, data=sh)
#nls(log(deaths)~log(phi)+log(confirmed), data=resync(uscd,26), start=list(phi=-4.5))
#with(resync(uscd, 26) mean(log(deaths)-log(confirmed))
#with(resync(uscd, 26), mean(log(deaths)-log(confirmed))
#)
#summary(resync(ucsd,26)$deaths)
#summary(resync(ucsdn,26)$deaths)
#summary(resync(uscdn,26)$deaths)
#with(resync(uscdn, 26), mean(log(deaths)-log(confirmed))
#)
#with(resync(uscdn, 26), mean(log(deaths)-log(confirmed)))
#with(resync(uscdn, 26), sd(log(deaths)-log(confirmed)))
#with(resync(uscdn, 24), sd(log(deaths)-log(confirmed)))
#with(resync(uscdn, 28), sd(log(deaths)-log(confirmed)))
#with(resync(uscdn, 30), sd(log(deaths)-log(confirmed)))
#sds = sapply(24:30, function(x) with(resync(uscdn, x), sd(log(deaths)-log(confirmed)))
#)
#plot(24:30, sds)
#sds = sapply(22:32, function(x) with(resync(uscdn, x), sd(log(deaths)-log(confirmed))))
#plot(22:32, sds)
#sds = sapply(20:34, function(x) with(resync(uscdn, x), sd(log(deaths)-log(confirmed))))
#plot(20:34, sds, pch=19, cex=1.5)
#sds = sapply(15:40, function(x) with(resync(uscdn, x), sd(log(deaths)-log(confirmed))))
#plot(15:40, sds, pch=19, cex=1.5)
#with(resync(uscdn, 26), mean(log(deaths)-log(confirmed))))
#with(resync(uscdn, 26), mean(log(deaths)-log(confirmed)))
#with(resync(uscdn, 21), mean(log(deaths)-log(confirmed)))
#with(resync(uscdn, 28), mean(log(deaths)-log(confirmed)))
#exp(-4.14)
#exp(-4.09)
#with(resync(uscdn, 21), hist(log(deaths)-log(confirmed)))
#with(resync(uscdn, 28), hist(log(deaths)-log(confirmed)))
#with(resync(uscdn, 28), qqnorm(log(deaths)-log(confirmed)))
#sds = sapply(10:50, function(x) with(resync(uscdn, x), sd(log(deaths)-log(confirmed))))
#plot(10:50, sds)
#library(runner)
#ls(2)
#?window_run
#?mean_run
#D28 = resync(uscdn, 28)
#c28 = mean_run(D28$confirmed, 14)
#plot(c28)
#d28 = mean_run(D28$deaths, 14)
#plot(log(c28)-4.5)
#lines(d28)
#lines(y=d28)
#lines(x=1:length(d28),y=d28)
#lines(x=1:length(d28),y=log(d28))
#mean(log(c28)-log(d28))
#plot(log(c28)-4)
#lines(x=1:length(d28),y=log(d28))
#head(d28)
#head(c28)
#D21 = resync(uscdn, 21)
#c21 = mean_run(D21$confirmed, 14)
#d21 = mean_run(D21$deaths, 14)
#mean(log(c21)-log(d21))
#plot(log(c21)-4.06)
#lines(x=1:length(d21),y=log(d21))
