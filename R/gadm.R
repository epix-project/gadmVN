#' Maps of Vietnam
#'
#' Generates a country or provinces map of Vietnam for a given date and
#' resolution.
#'
#' This function generates a country or provinces map of Vietnam at a given date.
#' Indeed the delimitations of Vietnamese provinces have changed through the
#' history with the number of provinces increasing from 40 in 1979 to 63 in 2008.
#' Most of the events are province splits and one merging.
#'
#' @param date Text in the "YYYY-MM-DD" format. For example "2015-01-17" for the
#' 17th of January 2017. By default "2015-01-01".
#' @param level Text ("country" or "provinces"). By default "provinces".
#' @param resolution Text ("low" or "high"). By default "low".
#' @source GADM data base from \url{www.gadm.org}.
#' @author Marc Choisy
#' @example
#' ## Some basic functions (sp methods):
#' prov_viet <- gadm()
#' slotNames(prov_viet)
#' length(prov_viet)
#' dim(prov_viet)
#' names(prov_viet)
#' prov_viet$province
#' bbox(prov_viet)
#' coordinates(prov_viet)
#' plot(prov_viet)
#' plot(subset(prov_viet,province=="Nghe An"),add=T,col="red")
#' ## The history of provinces delimitations:
#' @export
gadm <- function(date="2015-01-01",level="provinces",resolution="low") {
  dates <- as.Date(paste0(c(1990,1991,1992,1997,2004,2008),"-01-01"))
  resolution <- c(low="",high="r")
  period <- c("08_20","04_07","97_03","92_96","91_91","90_90","79_89")
  if(level=="provinces") middle <- paste0("1_",period[sum(as.Date(date)<dates)+1])
  else middle <- "0"
  get(paste0("gadm",middle,resolution))
}
