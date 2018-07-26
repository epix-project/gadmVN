#' Maps of Vietnam
#'
#' Generates a country or provinces map of Vietnam for a given date and
#' resolution.
#'
#' This function generates a country or provinces map of Vietnam at a given
#' date, and for a given resolution. Indeed, the delimitations of Vietnamese
#' provinces have changed through the history with the number of provinces
#' increasing from 40 in 1979 to 63 in 2008. Most of the events are province
#' splits and one merging. Changes is provinces boundaries occured on the 1st of
#' January of 1979, 1990, 1991, 1992, 1997, 2004 and 2008.
#'
#' @note argument \code{merge_hanoi} makes a difference only for \code{date}
#' before 2008-01-01.
#'
#' @param date either text in the "YYYY-MM-DD" format (for example "2015-01-17"
#' for the 17th of January 2017), or a numeric format of the year (for example
#' 2015). By default "2015-01-01".
#' @param level text ("country" or "provinces"). By default "provinces".
#' @param resolution text ("low" or "high"). By default "low".
#' @param merge_hanoi boolean indicating whether the province of Ha Noi should
#' be merged with the province of Ha Son Binh (before 1992) or the province of
#' Ha Tay (after 1991).
#' @source GADM data base from \url{www.gadm.org}.
#' @author Marc Choisy
#' @note Provinces are plotted in alphabetical order. This is important
#' information to know whenever the user wishes to use these maps to plot
#' covariables.
#' @export
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @return An object of class "SpatialPolygonsDataFrame".
#' @examples
#' # Plotting the map of Vietnamese provinces as of today:
#' pr <- gadm()
#' sp::plot(pr)
#'
#' # The same, with random colors:
#' sp::plot(pr, col = 1:4)
#'
#' # Plotting only the country boundaries:
#' vn <- gadm(level = "country")
#' sp::plot(vn, col = "grey")
#'
#' # Visualizing the maps of provinces for all the years where their boundaries
#' # changed:
#' vn <- lapply(c(1979, 1990:1992, 1997, 2004, 2008), gadm)
#' opar <- par(mfrow = c(2, 4))
#' for(i in vn) sp::plot(i)
#' par(opar)
#'
#' # ploting the province of Ha Noi before and after 2008, for 2 values of
#' # resolution:
#' vn1_low <- gadm(2007)
#' vn2_low <- gadm(2008)
#' vn1_high <- gadm(2007, res = "high")
#' vn2_high <- gadm(2008, res = "high")
#' opar <- par(mfrow = c(2, 2))
#' sp::plot(vn1_low[vn1_low$province %in% c("Ha Noi", "Ha Tay"), ])
#' title("2007, low resolution")
#' sp::plot(vn2_low[vn2_low$province == "Ha Noi", ])
#' title("2008, low resolution")
#' sp::plot(vn1_high[vn1_high$province %in% c("Ha Noi", "Ha Tay"), ])
#' title("2007, high resolution")
#' sp::plot(vn2_high[vn2_high$province == "Ha Noi", ])
#' title("2008, high resolution")
#' par(opar)
#' sp::plot(vn1_low)
#'
#' # Showing the same thing on the whole map of Vietnam:
#' sp::plot(vn1_low)
#' sp::plot(vn1_high[vn1_high$province %in% c("Ha Noi", "Ha Tay"), ],
#'      col = c("red","blue"), add=TRUE)
#' sp::plot(vn2_low)
#' sp::plot(vn2_high[vn2_high$province == "Ha Noi", ], col = "grey", add = TRUE)
#'
#' # Here we can see that the 2008 delimitation of the province of Ha Noi is
#' # more than the merging of the provinces of Ha Tay and Ha Noi:
#' sp::plot(vn2_high[vn2_high$province == "Ha Noi", ], col = "grey")
#' sp::plot(vn1_high[vn1_high$province %in% c("Ha Noi", "Ha Tay"), ],
#'      col = c("red", "blue"), add = TRUE)
gadm <- function(date = "2015-01-01", level = c("provinces", "country"), resolution = c("low", "high"), merge_hanoi = FALSE) {
  dates <- as.Date(paste0(c(1990:1992, 1997, 2004, 2008), "-01-01"))
  if(is.numeric(date)) date <- paste0(date, "-01-01")
  level <- match.arg(level)
  resolution <- match.arg(resolution)
  resolution <- c(low = "", high = "r")[resolution]
  period <- c("08_20", "04_07", "97_03", "92_96", "91_91", "90_90", "79_89")
  if (level == "provinces")
    middle <- paste0("1_", period[sum(as.Date(date) < dates) + 1])
  else middle <- "0"
  hanoi <- ifelse(merge_hanoi & date < as.Date("2008-01-01"), "_hn", "")
  get(paste0("gadm", middle, resolution, hanoi))
}
