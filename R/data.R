#' GADM maps for Vietnam
#'
#' The GADM maps of the country and provinces borders of Vietnam.
#'
#' \code{gadm0} and \code{gadm0ir} are the GADM maps of the country border and
#' \code{gadm1} and \code{gadm1r} are the GADM maps of the provinces borders.
#' \code{gadm0r} and \code{gadm1r} are the raw GADM objects (heavy in memory)
#' whereas \code{gadm0} and \code{gadm1} are the "thinned" GADM objects for
#' easier plotting.
#'
#' @source GADM data base from \url{www.gadm.org}.
#' @author Marc Choisy
#' @examples
#' ## Plotting the country border of Vietnam:
#' plot(gadm0)
#' ## Adding the provinces borders:
#' plot(gadm1,add=T)
#' @name gadm
NULL

#' @rdname gadm
"gadm0"

#' @rdname gadm
"gadm0r"

#' @rdname gadm
"gadm1"

#' @rdname gadm
"gadm1r"
