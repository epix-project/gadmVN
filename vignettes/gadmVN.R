## ----include = FALSE-----------------------------------------------------
knitr::knit_hooks$set(margin = function(before, options, envir) {
  if (before) par(mgp = c(1.5, .5, 0), bty = "n", plt = c(.105, .97, .13, .97))
  else NULL
})

knitr::opts_chunk$set(margin = TRUE, prompt = TRUE, comment = "",
                      collapse = TRUE, cache = FALSE,
                      dev.args = list(pointsize = 11), fig.height = 3.5,
                      fig.width = 4.24725, fig.retina = 2, fig.align = "center")

## ----eval = FALSE--------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("choisy/gadmVN", build_vignettes = TRUE)

## ------------------------------------------------------------------------
library(gadmVN)

## ------------------------------------------------------------------------
pr <- gadm(1992)

## ------------------------------------------------------------------------
latitudes <- coordinates(pr)[, 2]
breaks <- seq(min(latitudes), max(latitudes), le = 5)
palette <- c("red", "blue", "green", "orange")
colors <- palette[as.numeric(cut(latitudes, breaks, include = TRUE))]
plot(pr, col = colors)

## ------------------------------------------------------------------------
provinces <- gadm()
plot(provinces, col = provinces$color)
leg <- unique(provinces@data[, c("region", "color")])
legend("topright", legend = leg$region, fill = leg$color, bty = "n")

