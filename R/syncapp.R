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
#' @param xlim.in defaults to NULL, standard graphics::plot xlim, or can be a numeric(2)
#' @param yspan log-scale extra padding for y extent, expanding range of log(incident cases)+log cIFR
#' @note The 'IFR' is crudely estimated as exp(mean(log(deaths)-log(confirmed))).
#' @return a list with elements x, y, date, invisibly
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
plot_sync = function(dfr, basedate = "2020-05-01", maxdate=Inf, shift=26, winsize=14, 
     na_pad=TRUE, xlim.in=NULL, yspan=.25) {
   dfr = dplyr::filter(dfr, date >= basedate & date < maxdate)
   dfr = resync( dfr, shift )
   phi = mean(log(dfr$deaths)-log(dfr$confirmed))
   smc = mean_run(dfr$confirmed, winsize, na_pad=na_pad)
   smd = mean_run(dfr$deaths, winsize, na_pad=na_pad)
   #yrng = range(c(1,log(smc)+phi, log(smd)), na.rm=TRUE)
   yrng = range(c(log(smc)+phi, log(smd)), na.rm=TRUE)
   #if (yrng[2] > 6) yrng[1] = max(c(4, yrng[1]))
   plot(I(log(smc)+phi)~dfr$date, type="l", lty=1, lwd=2, col="gray", xlab="Date", ylab="log 'incidence'",
           ylim=yrng+c(-yspan,yspan), xlim=xlim.in)
   lines(log(smd)~dfr$date, lty=2, lwd=2)
   dvals = c(seq(10,200,20),500, 1000, 2000)
   axis(4, at = log(dvals), labels=dvals)
   mtext("deaths", side=4)
   legend(x="topleft", lty=1:2, lwd=2, 
        col = c("gray", "black"),
        legend=c(paste("log confirmed", round(phi,3), "[cIFR", 100*round(exp(phi),3), "%]"), 
          paste("deaths (shifted by", shift, "days)")))
   invisible(list(x=log(smc)+phi, y=log(smd), date=dfr$date))
}


#' shiny app for appraisal of synchronization of death incidence and confirmed infection incidence peaks
#' @export
syncapp = function() {
 od = getwd()
 on.exit(setwd(od))
 uif = system.file("syncapp/ui.R", package="sars2app")
 servf = system.file("syncapp/server.R", package="sars2app")
 td = tempdir()
 setwd(td)
 file.copy(uif, ".", overwrite=TRUE)
 file.copy(servf, ".", overwrite=TRUE)
 shiny::runApp()
}

