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
#' January of 1979, 1990, 1991, 1992, 1997, 2004 and 2008. \cr\cr
#' The maps generated for a time after 2008 contained different information:
#' \tabular{ll}{
#'   Column Names \tab Description \cr
#'   province \tab name of the province in English \cr
#'   region \tab name of the ecologic region in English \cr
#'   color_ecologic \tab color associated with the ecologic region \cr
#'   region_economic \tab name of the region economic in English  \cr
#'   color_economic \tab color associated with the economic region\cr
#'   geometry \tab list-column with geometries \cr
#'  }
#'  Wheras the map generated for a time before 2008, only the columns
#'  \code{"province"} and \code{"geometry"}.
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
#' @return An object of class "sf" and "data.frame".
#' @examples
#'
#' library(sf)
#'
#' # BASIC USAGE ---------------------------------------------------------------
#'
#' # Plotting the map of Vietnamese provinces as of today:
#' pr <- gadm()
#' plot(st_geometry(pr))
#' # other way of plotting
#' plot(pr["province"]) # by default a color key is given
#'
#' # The same, with random colors:
#' plot(st_geometry(pr), col = 1:4)
#'
#' # Plotting only the country boundaries:
#' vn <- gadm(level = "country")
#' plot(st_geometry(vn), col = "grey")
#'
#' # SF TO SP ------------------------------------------------------------------
#'
#' # The `pr` object is an object of class `sf` and `data.frame`. For more
#' # complexe analyses, it a be interresting to work with an object of class
#' # `SpatialPolygonsDataFrame` from the `sp` package:
#' sp_vn <- sf::as_Spatial(pr$geometry)
#' sp::plot(sp_vn)
#'
#'
#' # MORE COMPLEXE EXAMPLE OF VISUALISATION ------------------------------------
#'
#' # Visualizing the maps of provinces for all the years where their boundaries
#' # changed:
#' vn <- lapply(c(1979, 1990:1992, 1997, 2004, 2008), gadm)
#' opar <- par(mfrow = c(2, 4))
#' for(i in vn) plot(st_geometry(i))
#' par(opar)
#'
#' # ploting the province of Ha Noi before and after 2008, for 2 values of
#' # resolution:
#' vn1_low <- gadm(2007)
#' vn2_low <- gadm(2008)
#' vn1_high <- gadm(2007, res = "high")
#' vn2_high <- gadm(2008, res = "high")
#' opar <- par(mfrow = c(2, 2))
#' plot(st_geometry(vn1_low[vn1_low$province %in% c("Ha Noi", "Ha Tay"), ]))
#' title("2007, low resolution")
#' plot(st_geometry(vn2_low[vn2_low$province == "Ha Noi", ]))
#' title("2008, low resolution")
#' plot(st_geometry(vn1_high[vn1_high$province %in% c("Ha Noi", "Ha Tay"), ]))
#' title("2007, high resolution")
#' plot(st_geometry(vn2_high[vn2_high$province == "Ha Noi", ]))
#' title("2008, high resolution")
#' par(opar)
#' plot(st_geometry(vn1_low))
#'
#' # Showing the same thing on the whole map of Vietnam:
#' plot(st_geometry(vn1_low))
#' plot(st_geometry(vn1_high[vn1_high$province %in% c("Ha Noi", "Ha Tay"), ]),
#'      col = c("red","blue"), add=TRUE)
#' plot(st_geometry(vn2_low))
#' plot(st_geometry(vn2_high[vn2_high$province == "Ha Noi", ]),
#'      col = "grey", add = TRUE)
#'
#' # Here we can see that the 2008 delimitation of the province of Ha Noi is
#' # more than the merging of the provinces of Ha Tay and Ha Noi:
#' plot(st_geometry(vn2_high[vn2_high$province == "Ha Noi", ]), col = "grey")
#' plot(st_geometry(vn1_high[vn1_high$province %in% c("Ha Noi", "Ha Tay"), ]),
#'      col = c("red", "blue"), add = TRUE)
gadm <- function(date = "2015-01-01",
                 level = c("provinces", "country"),
                 resolution = c("low", "high"),
                 merge_hanoi = FALSE) {
  dates <- as.Date(paste0(c(1990:1992, 1997, 2004, 2008), "-01-01"))
  if (is.numeric(date)) date <- paste0(date, "-01-01")
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
