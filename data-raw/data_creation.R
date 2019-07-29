# Packages and System ----------------------------------------------------------
library(sptools)     # for "gadm", "thin_polygons", "define_bbox_proj"
# (package from "github/choisy")
library(dictionary)  # for "vn_admin1"
library(sf)          # for "st_union", "st_cast", "as_Spatial", "st_as_sf"
tolerance <- .01    # the tolerance parameter of the thinning function

# Functions --------------------------------------------------------------------

# Function to define new map with the Hanoi, Ha Tay, Hoa Binh, Hoa Son Binh
# merged
merge_hanoi <- function(sf_obj) {
  # Create a list special to merge Hanoi, Ha Tay, Hoa Son Binh as Ha Noi
  list_ha <- list(list(year = "2008-01-01", event = "split", before = "Ha Noi",
                       after = c("Ha Son Binh", "Ha Tay", "Ha Noi")))
  # Create a new map
  new_map <- sf_aggregate_lst(sf_obj, list_ha, from = "1979", to = "2008")
}

# rename one column to another name (default admin1 to province)
rename_col <- function(df,ancient = "admin1", new = "province") {
  colnames(df)[colnames(df) == ancient] <- new
  df
}

# Downloading the actual country and provinces maps from GADM (www.gadm.org) ---

# Actual administrative boundaries:
gadm0r <- sptools::gadm("Vietnam", "sf", 0, save = FALSE, intlib = FALSE)
gadm0r <- gadm0r[, -which(names(gadm0r) == "GID_0")]
gadm0r <- rename_col(gadm0r, "NAME_0", "country")
gadm1_08_20r <- sptools::gadm("Vietnam", "sf", 1, save = FALSE, intlib = FALSE)

# Coming from old gadm file:
gadm1_04_07r <- readRDS("data-raw/gadm_vn_0407.rds")
# the 64 provinces from 2004 to 2007

# Translate the province from Vietnamese to a column "province" in English -----
gadm1_08_20r <- transform(gadm1_08_20r,
                          admin1 = as.character(
                            translate(gadm1_08_20r$NAME_1, vn_admin1)))
gadm1_08_20r <- gadm1_08_20r[ , c("admin1", "geometry")]
gadm1_08_20r$admin1 <- as.character(gadm1_08_20r$admin1)
gadm1_08_20r <- sf::st_sf(gadm1_08_20r)

gadm1_04_07r <- transform(gadm1_04_07r,
                          admin1 = translate(gadm1_04_07r$NAME_2, vn_admin1))
gadm1_04_07r <- gadm1_04_07r[ , c("admin1", "geometry")]
gadm1_04_07r$admin1 <- as.character(gadm1_04_07r$admin1)
gadm1_04_07r <- sf::st_sf(gadm1_04_07r)

# Generating the historical provinces maps -------------------------------------

gadm1_97_03r <- sf_aggregate_lst(gadm1_04_07r, vn_history, from = "1997",
                                 to = "2004")
gadm1_92_96r <- sf_aggregate_lst(gadm1_97_03r, vn_history, from = "1992",
                                 to = "1997")
gadm1_91_91r <- sf_aggregate_lst(gadm1_04_07r, vn_history, from = "1991",
                                 to = "2004")
gadm1_90_90r <- sf_aggregate_lst(gadm1_04_07r, vn_history, from = "1990",
                                 to = "2004")
gadm1_79_89r <- sf_aggregate_lst(gadm1_04_07r, vn_history, from = "1979",
                                 to = "2004")

# tests if province are corresponding
setdiff(gadm1_79_89r$province, vn_admin1_year$`1979-1990`)
setdiff(gadm1_90_90r$province, vn_admin1_year$`1990-1991`)
setdiff(gadm1_91_91r$province, vn_admin1_year$`1991-1992`)
setdiff(gadm1_92_96r$province, vn_admin1_year$`1992-1997`)
setdiff(gadm1_97_03r$province, vn_admin1_year$`1997-2004`)
setdiff(gadm1_04_07r$province, vn_admin1_year$`2004-2008`)
setdiff(gadm1_08_20r$province, vn_admin1_year$`2008-2020`)

# Thinning ---------------------------------------------------------------------

gadm0 <- thin_polygons(gadm0r, tolerance)
gadm1_08_20 <- thin_polygons(gadm1_08_20r, tolerance)
gadm1_04_07 <- thin_polygons(gadm1_04_07r, tolerance)
gadm1_97_03 <- thin_polygons(gadm1_97_03r, tolerance)
gadm1_92_96 <- thin_polygons(gadm1_92_96r, tolerance)
gadm1_91_91 <- thin_polygons(gadm1_91_91r, tolerance)
gadm1_90_90 <- thin_polygons(gadm1_90_90r, tolerance)
gadm1_79_89 <- thin_polygons(gadm1_79_89r, tolerance)

# Defining the same boundaries box and projections for all years: --------------

boundbox <- st_bbox(gadm1_04_07)
crs <- st_crs(gadm1_04_07)

gadm1_08_20r <- define_bbox_proj(gadm1_08_20r, boundbox, crs)
gadm1_04_07r <- define_bbox_proj(gadm1_04_07r, boundbox, crs)
gadm1_97_03r <- define_bbox_proj(gadm1_97_03r, boundbox, crs)
gadm1_92_96r <- define_bbox_proj(gadm1_92_96r, boundbox, crs)
gadm1_91_91r <- define_bbox_proj(gadm1_91_91r, boundbox, crs)
gadm1_90_90r <- define_bbox_proj(gadm1_90_90r, boundbox, crs)
gadm1_79_89r <- define_bbox_proj(gadm1_79_89r, boundbox, crs)
gadm1_08_20 <- define_bbox_proj(gadm1_08_20, boundbox, crs)
gadm1_97_03 <- define_bbox_proj(gadm1_97_03, boundbox, crs)
gadm1_92_96 <- define_bbox_proj(gadm1_92_96, boundbox, crs)
gadm1_91_91 <- define_bbox_proj(gadm1_91_91, boundbox, crs)
gadm1_90_90 <- define_bbox_proj(gadm1_90_90, boundbox, crs)
gadm1_79_89 <- define_bbox_proj(gadm1_79_89, boundbox, crs)

# Defining the maps with the Hanoi, Ha Tay, Hoa Binh, Hoa Son Binh merged: -----
# these maps are useful in case of time series that start before 1992-01-01 and
# end after 2007-12-31.

gadm1_79_89_hn <- define_bbox_proj(merge_hanoi(gadm1_79_89), boundbox, crs)
gadm1_79_89r_hn <- define_bbox_proj(merge_hanoi(gadm1_79_89r), boundbox, crs)
gadm1_90_90_hn <- define_bbox_proj(merge_hanoi(gadm1_90_90), boundbox, crs)
gadm1_90_90r_hn <- define_bbox_proj(merge_hanoi(gadm1_90_90r), boundbox, crs)
gadm1_91_91_hn <- define_bbox_proj(merge_hanoi(gadm1_91_91), boundbox, crs)
gadm1_91_91r_hn <- define_bbox_proj(merge_hanoi(gadm1_91_91r), boundbox, crs)
gadm1_92_96_hn <- define_bbox_proj(merge_hanoi(gadm1_92_96), boundbox, crs)
gadm1_92_96r_hn <- define_bbox_proj(merge_hanoi(gadm1_92_96r), boundbox, crs)
gadm1_97_03_hn <- define_bbox_proj(merge_hanoi(gadm1_97_03), boundbox, crs)
gadm1_97_03r_hn <- define_bbox_proj(merge_hanoi(gadm1_97_03r), boundbox, crs)
gadm1_04_07_hn <- define_bbox_proj(merge_hanoi(gadm1_04_07), boundbox, crs)
gadm1_04_07r_hn <- define_bbox_proj(merge_hanoi(gadm1_04_07r), boundbox, crs)

# Rename admin1 to province ----------------------------------------------------

gadm1_08_20r <- rename_col(gadm1_08_20r)
gadm1_04_07r <- rename_col(gadm1_04_07r)
gadm1_97_03r <- rename_col(gadm1_97_03r)
gadm1_92_96r <- rename_col(gadm1_92_96r)
gadm1_91_91r <- rename_col(gadm1_91_91r)
gadm1_90_90r <- rename_col(gadm1_90_90r)
gadm1_79_89r <- rename_col(gadm1_79_89r)
gadm1_08_20 <- rename_col(gadm1_08_20)
gadm1_04_07 <- rename_col(gadm1_04_07)
gadm1_97_03 <- rename_col(gadm1_97_03)
gadm1_92_96 <- rename_col(gadm1_92_96)
gadm1_91_91 <- rename_col(gadm1_91_91)
gadm1_90_90 <- rename_col(gadm1_90_90)
gadm1_79_89 <- rename_col(gadm1_79_89)

gadm1_04_07r_hn <- rename_col(gadm1_04_07r_hn)
gadm1_97_03r_hn <- rename_col(gadm1_97_03r_hn)
gadm1_92_96r_hn <- rename_col(gadm1_92_96r_hn)
gadm1_91_91r_hn <- rename_col(gadm1_91_91r_hn)
gadm1_90_90r_hn <- rename_col(gadm1_90_90r_hn)
gadm1_79_89r_hn <- rename_col(gadm1_79_89r_hn)
gadm1_04_07_hn <- rename_col(gadm1_04_07_hn)
gadm1_97_03_hn <- rename_col(gadm1_97_03_hn)
gadm1_92_96_hn <- rename_col(gadm1_92_96_hn)
gadm1_91_91_hn <- rename_col(gadm1_91_91_hn)
gadm1_90_90_hn <- rename_col(gadm1_90_90_hn)
gadm1_79_89_hn <- rename_col(gadm1_79_89_hn)

# Defining the ecologic and economic regions: ----------------------------------

colors_reg <- list(Northwest = c(243, 225, 0),
                   Northeast = c(255, 175, 26),
                   "Red River Delta" = c(255, 103, 103),
                   "North Central Coast" = c(0, 214, 0),
                   "South Central Coast" = c(0, 221, 217),
                   "Central Highlands" = c(36, 135, 255),
                   Southeast  = c(195, 36, 255),
                   "Mekong Delta" = c(255, 36, 196))
colors_reg <- sapply(colors_reg,
                     function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))

colors_eco <- list("Northern Midlands & Mountains" = c(243, 225,   0),
                   "Red River Delta"               = c(255, 103, 103),
                   "Central Coast"                 = c(  0, 214,   0),
                   "Central Highlands"             = c( 36, 135, 255),
                   Southeast                       = c(195,  36, 255),
                   "Mekong Delta"                  = c(255,  36, 196))
colors_eco <- sapply(colors_eco,
                     function(x) rgb(x[1], x[2], x[3], max = 255))

regions <- read.table("data-raw/provinces_wikipedia.txt", sep = "\t",
                      stringsAsFactors = FALSE)[, c(1, 7)]
colnames(regions)[colnames(regions) == "V1"] <- "province"
colnames(regions)[colnames(regions) == "V7"] <- "region"
regions <- transform(regions,
                     province =
                       translate(gsub(" Province| City", "", regions$province),
                                 vn_admin1),
                     colors_ecologic = colors_reg[regions$region],
                     region_economic =
                       gsub("^North....", "Northern Midlands & Mountains",
                            gsub("North |South ", "", regions$region)))

regions[regions$province == "Quang Ninh", "region_economic"] <-
  "Red River Delta"

regions <- transform(regions,
                     color_economic = colors_eco[regions$region_economic])

gadm1_08_20 <- merge(gadm1_08_20, regions, stringsAsFactors = FALSE)
gadm1_08_20r <- merge(gadm1_08_20r, regions, stringsAsFactors = FALSE)

# Saving -----------------------------------------------------------------------

eval(parse(text = paste0("usethis::use_data(",
                   paste(grep("gadm\\d", ls(), value = TRUE), collapse = ", "),
                   ", internal = TRUE, overwrite = TRUE)")))

# erase everything #############################################################

rm(list = ls())
