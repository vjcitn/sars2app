
#' shiny app for appraisal of simple time-series models for COVID-19 incidence trajectories
#' @export
tsapp = function() {
 od = getwd()
 on.exit(setwd(od))
 uif = system.file("tsapp/ui.R", package="sars2app")
 servf = system.file("tsapp/server.R", package="sars2app")
 td = tempdir()
 setwd(td)
 file.copy(uif, ".", overwrite=TRUE)
 file.copy(servf, ".", overwrite=TRUE)
 shiny::runApp()
}

