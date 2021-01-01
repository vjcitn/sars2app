
 get_us_data_mob = function() {
  if (!exists("jh")) jh <<- enriched_jhu_data()
  us = dplyr::filter(jh, alpha2Code=="US")
  usd =  dplyr::filter(us, subset=="deaths")
  usc =  dplyr::filter(us, subset=="confirmed")
  usd2 = usd[-1,]
  usd2$inc = diff(usd$count)
  usc2 = usc[-1,]
  usc2$inc = diff(usc$count)
  epi = data.frame(date=usc2$date, confirmed=usc2$inc, deaths=usd2$inc)
  if (!exists("mob")) mob <<- descartes_mobility_data()
  mob = dplyr::filter(mob, admin_level==0)
  dplyr::left_join(epi, mob, by="date")
 }



 get_state_data_mob = function(region = "New York") {
  if (!exists("nyd")) nyd <<- nytimes_state_data()
  nyd = dplyr::filter(nyd, state==region)
  nydd =  dplyr::filter(nyd, subset=="deaths")
  nydc =  dplyr::filter(nyd, subset=="confirmed")
  nydd2 = nydd[-1,]
  nydd2$inc = diff(nydd$count)
  nydc2 = nydc[-1,]
  nydc2$inc = diff(nydc$count)
  epi = data.frame(date=nydc2$date, state=nydd2$state, confirmed=pmax(1, nydc2$inc), deaths=pmax(1,nydd2$inc))
  if (!exists("mob")) mob <<- descartes_mobility_data()
  mob = dplyr::filter(mob, admin_level==1)
  mob = dplyr::mutate(mob, state=admin1) %>% filter(state==region)
  dplyr::left_join(epi, mob, by="date")
 }

resync_mob = function (dfr, shift = 26) 
{
    if (shift==0) return(dfr)
    shift = as.integer(shift)
    nrec = nrow(dfr)
    dat = dfr[seq_len(nrec - shift), ]
    dat$confirmed = dfr$confirmed[-seq_len(shift)]
    dat
}

#' illustrate joint behavior of mobility data and COVID-19 incidence over time in states
#' @param state character(1) capitalized
#' @param shift numeric(1) optional, would add a trace if > 0
#' @examples
#' op = par(no.readonly=TRUE)
#' par(mfrow=c(2,2), mar=c(4,3,3,2))
#' plot_sync_mob()
#' plot_sync_mob("New York")
#' plot_sync_mob("Massachusetts")
#' plot_sync_mob("Texas")
#' par(op)
#' @export
plot_sync_mob = function(state="California", shift=0) {
 yy = get_state_data_mob(region=state)
 plot(I(mean_run(m50,14)+5)~date, data=yy, type="l", lwd=2, main=state, ylab="log incidence; m50+5")
 zz0 = resync_mob(yy, shift=0)
 lines(zz0$date, log(mean_run(zz0$confirmed,14)+1), col="darkgreen", lwd=2, lty=2)
 legend("topleft", lwd=2, lty=1:2, col=c("black", "darkgreen"), legend=c("Descartes m50", "confirmed"))
 if (shift>0) {
   zzsh = resync_mob(yy, shift=shift)
   lines(zzsh$date, log(mean_run(zzsh$confirmed,14)+1), col="blue", lwd=2, lty=2)
   }
}

