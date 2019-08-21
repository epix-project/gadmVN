# gadmVN 

[![Travis build status](https://travis-ci.org/epix-project/gadmVN.svg?branch=master)](https://travis-ci.org/epix-project/gadmVN)
[![Codecov test coverage](https://codecov.io/gh/epix-project/gadmVN/branch/master/graph/badge.svg)](https://codecov.io/gh/epix-project/gadmVN?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/epix-project/gadmVN?branch=master&svg=true)](https://ci.appveyor.com/project/epix-project/gadmVN)

This package contains the polygons of Vietnam and its provinces at different
points in time since 1979, reflecting the changes in the administrative
divisions along the history of Vietnam.

## Installation and loading

You can install `gadmVN` from GitHub with:

```{r eval = FALSE}
# install.packages("devtools")
devtools::install_github("epix-project/gadmVN", build_vignettes = TRUE)
```

Once installed, you can load the package:

```{r}
library(gadmVN)
```


## Usage examples

`SpatialPolygonsDataFrame` can be retrieved thanks to the `gadm` function. The
first argument is the date at which we want the map (either a text in the
"YYYY-MM-DD" format or a 4-digit numeric format of the year). The second
argument specifies the level we are interested in (either "provinces" or
"country"), the third argument is the level of resolution we want (either "low"
or "high") and the last argument tells whether we want the province of Ha Noi
and Ha Son Binh to be merged. This latter argument makes a difference only for
date before 1992-01-01.

The map of the provinces in 1992

```{r}
pr <- gadm(1992)
```

Maping 4 categories of provinces according to latitude:

```{r}
latitudes <- coordinates(pr)[, 2]
breaks <- seq(min(latitudes), max(latitudes), le = 5)
palette <- c("red", "blue", "green", "orange")
colors <- palette[as.numeric(cut(latitudes, breaks, include = TRUE))]
plot(pr, col = colors)
```

Plotting the 8 regions of Vietnam:

```{r}
provinces <- gadm()
plot(provinces, col = provinces$color)
leg <- unique(provinces@data[, c("region", "color")])
legend("topright", legend = leg$region, fill = leg$color, bty = "n")
```

