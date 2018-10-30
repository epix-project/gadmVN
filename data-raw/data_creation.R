# Packages and System ----------------------------------------------------------
library(maptools)    # for "thinnedSpatialPoly"
library(sptools)     # for "gadm" (package from "github/choisy")
library(dictionary)  # for "vn_province"
library(dplyr)       # for "select", "filter", "mutate","arrange", "left_join",
# "bind_rows"
library(sf)          # for "st_union", "st_cast", "as_Spatial", "st_as_sf"
tolerance <- .01    # the tolerance parameter of the thinning function

# Functions --------------------------------------------------------------------

# Thinning (simplification)
thin_polygons <- function(sf_obj, tolerance) {
  sf_obj %<>% as_Spatial(.) %>%
    thinnedSpatialPoly(tolerance) %>%
    st_as_sf(.)
}

# Define new boundaries box and projections of a sf object
define_bbox_proj <- function(sf_obj, boundbox, crs) {
  attr(sf_obj[["geometry"]], "bbox") <- boundbox
  attr(sf_obj[["geometry"]], "crs") <- crs
  sf_obj
}

# Function to define new map with the Hanoi, Ha Tay, Hoa Binh, Hoa Son Binh
# merged
merge_hanoi <- function(sf_obj){

  # Create a list special to merge Hanoi, Ha Tay, Hoa Son Binh as Ha Noi
  list_ha <- list(list(year = "2008-01-01", event = "split", before = "Ha Noi",
                      after = c("Ha Son Binh", "Ha Tay", "Ha Noi")))
  # Create a new map
  new_map <- sf_aggregate_lst(sf_obj, list_ha, from = "1979", to = "2008") %>%
    arrange(province)
}

# Downloading the actual country and provinces maps from GADM (www.gadm.org) ---

# Actual administrative boundaries:
gadm0r <- sptools::gadm("Vietnam", "sf", 0) %>% select(-GID_0) %>%
  rename(country = NAME_0)
gadm1_08_20r <- sptools::gadm("Vietnam", "sf", 1)

# Coming from old gadm file:
gadm1_04_07r <- readRDS("data-raw/gadm_vn_0407.rds")  # the 64 provinces from 2004 to 2007

# Translate the province from Vietnamese to a column "province" in English -----
gadm1_08_20r %<>%
  mutate(province = stringi::stri_escape_unicode(NAME_1) %>%
           vn_province[.]) %>%
  select(province, geometry)

gadm1_04_07r %<>%
  mutate(province = stringi::stri_escape_unicode(NAME_2) %>%
           vn_province[.]) %>%
  select(province, geometry)


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
setdiff(gadm1_79_89r$province, vn_province_year$`1979-1990`)
setdiff(gadm1_90_90r$province, vn_province_year$`1990-1991`)
setdiff(gadm1_91_91r$province, vn_province_year$`1991-1992`)
setdiff(gadm1_92_96r$province, vn_province_year$`1992-1997`)
setdiff(gadm1_97_03r$province, vn_province_year$`1997-2004`)
setdiff(gadm1_04_07r$province, vn_province_year$`2004-2008`)
setdiff(gadm1_08_20r$province, vn_province_year$`2008-2020`)

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

gadm1_08_20r %<>% define_bbox_proj(boundbox, crs)
gadm1_04_07r %<>% define_bbox_proj(boundbox, crs)
gadm1_97_03r %<>% define_bbox_proj(boundbox, crs)
gadm1_92_96r %<>% define_bbox_proj(boundbox, crs)
gadm1_91_91r %<>% define_bbox_proj(boundbox, crs)
gadm1_90_90r %<>% define_bbox_proj(boundbox, crs)
gadm1_79_89r %<>% define_bbox_proj(boundbox, crs)
gadm1_08_20 %<>% define_bbox_proj(boundbox, crs)
gadm1_97_03 %<>% define_bbox_proj(boundbox, crs)
gadm1_92_96 %<>% define_bbox_proj(boundbox, crs)
gadm1_91_91 %<>% define_bbox_proj(boundbox, crs)
gadm1_90_90 %<>% define_bbox_proj(boundbox, crs)
gadm1_79_89 %<>% define_bbox_proj(boundbox, crs)

# Defining the maps with the Hanoi, Ha Tay, Hoa Binh, Hoa Son Binh merged: -----
# these maps are useful in case of time series that start before 1992-01-01 and
# end after 2007-12-31.

gadm1_79_89_hn <- merge_hanoi(gadm1_79_89) %>% define_bbox_proj(boundbox, crs)
gadm1_79_89r_hn <- merge_hanoi(gadm1_79_89r) %>% define_bbox_proj(boundbox, crs)
gadm1_90_90_hn <- merge_hanoi(gadm1_90_90) %>% define_bbox_proj(boundbox, crs)
gadm1_90_90r_hn <- merge_hanoi(gadm1_90_90r) %>% define_bbox_proj(boundbox, crs)
gadm1_91_91_hn <- merge_hanoi(gadm1_91_91) %>% define_bbox_proj(boundbox, crs)
gadm1_91_91r_hn <- merge_hanoi(gadm1_91_91r) %>% define_bbox_proj(boundbox, crs)
gadm1_92_96_hn <- merge_hanoi(gadm1_92_96) %>% define_bbox_proj(boundbox, crs)
gadm1_92_96r_hn <- merge_hanoi(gadm1_92_96r) %>% define_bbox_proj(boundbox, crs)
gadm1_97_03_hn <- merge_hanoi(gadm1_97_03) %>% define_bbox_proj(boundbox, crs)
gadm1_97_03r_hn <- merge_hanoi(gadm1_97_03r) %>% define_bbox_proj(boundbox, crs)
gadm1_04_07_hn <- merge_hanoi(gadm1_04_07) %>% define_bbox_proj(boundbox, crs)
gadm1_04_07r_hn <- merge_hanoi(gadm1_04_07r) %>% define_bbox_proj(boundbox, crs)

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
                      stringsAsFactors = FALSE)[, c(1, 7)] %>%
  rename(province = V1, region = V7) %>%
  mutate(province = gsub(" Province| City", "", province) %>%
           stringi::stri_escape_unicode(.) %>%
           vn_province[.],
         colors_ecologic = colors_reg[region],
         region_economic = region %>%
           gsub("North |South ", "", .) %>%
           gsub("^North....", "Northern Midlands & Mountains", .)
         )

regions[regions$province == "Quang Ninh", "region_economic"] <-
  "Red River Delta"

regions %<>% mutate(color_economic = colors_eco[region_economic])

gadm1_08_20 %<>% left_join(regions, by = "province") %>% arrange(province)
gadm1_08_20r %<>% left_join(regions, by = "province") %>% arrange(province)

# Defining population centroid -------------------------------------------------

gadm1_08_20 %<>% as_Spatial() %>%
  sptools::as_list() %>%
  purrr::map(crop_on_poly, rstr = worldpopVN::getpop()) %>%
  purrr::map(raster::rasterToPoints, spatial = TRUE) %>%
  purrr::map(weighted_centroid) %>%
  purrr::reduce(bind_rows) %>%
  mutate(province = gadm1_08_20$province) %>%
  left_join(gadm1_08_20r, ., by = "province") %>%
  select("province", everything())

gadm1_08_20r %<>% as_Spatial() %>%
  sptools::as_list() %>%
  purrr::map(crop_on_poly, rstr = worldpopVN::getpop()) %>%
  purrr::map(raster::rasterToPoints, spatial = TRUE) %>%
  purrr::map(weighted_centroid) %>%
  purrr::reduce(bind_rows) %>%
  mutate(province = gadm1_08_20r$province) %>%
  left_join(gadm1_08_20r, ., by = "province") %>%
  select("province", everything())

# Saving -----------------------------------------------------------------------

eply::evals(paste0("devtools::use_data(",
            paste(grep("gadm\\d", ls(), value = TRUE), collapse = ", "),
            ", internal = TRUE, overwrite = TRUE)"))

# erase everything #############################################################

rm(list = ls())
