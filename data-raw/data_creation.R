library(maptools)   # for "thinnedSpatialPoly", "unionSpatialPolygons"
#library(vietnam63)  # for "provinces" and "provinces_r"                   #####
library(censusVN2009)  # for "provinces" and "provinces_r"                 #####
tolerance <- .01    # the tolerance parameter of the thinning function


# Downloading the country and provinces maps from GADM (www.gadm.org) ----------

#getdata <- function(x)
#  raster::getData("GADM", country = "VNM", level = x, path = "data-raw")
#gadm0r <- getdata(0)
gadm0r <- raster::getData("GADM", country = "VNM", level = 0, path = "data-raw")
## gadm1_08_20r <- getdata(1)
data("provinces_r", "provinces")                                           #####
gadm1_08_20r <- provinces_r                                                #####
gadm1_08_20 <- provinces                                                   #####
load("data-raw/VNM_adm2.RData")  # the 64 provinces from 2004 to 2007
gadm1_04_07r <- gadm



# The hash table for the new provinces names variable --------------------------

dictionary <- c(              "Bac Kan|Bac Can" = "Bac Kan",
                         "Da Nang City|Da Nang" = "Da Nang",
                              "Dak Lak|Dac Lac" = "Dak Lak",
                                     "Dac Nong" = "Dak Nong",
                            "Ha Noi City|Hanoi" = "Ha Noi",
                      "Hai Phong City|Haiphong" = "Hai Phong",
                 "Ho Chi Minh City|Ho Chi Minh" = "Ho Chi Minh",
                "Ba Ria - VTau|Ba Ria-Vung Tau" = "Ba Ria - Vung Tau")



# ------------------------------------------------------------------------------

#province <- sub("Ba Ria-Vung Tau", "Ba Ria - Vung Tau", gadm1_08_20r@data$NAME_ENG)
#province <- sub("Thua Thien Hue", "Thua Thien - Hue", province)            #####
#pr <- as.data.frame(province, stringsAsFactors = FALSE)                    #####
#rownames(pr) <- province                                                   #####
#gadm1_08_20@data <- pr                                                     #####
#gadm1_08_20r@data <- pr                                                    #####
#for (i in seq_along(gadm1_08_20@polygons))                                 #####
#  gadm1_08_20@polygons[[i]]@ID <- province[i]                              #####
#for (i in seq_along(gadm1_08_20r@polygons))                                #####
#  gadm1_08_20r@polygons[[i]]@ID <- province[i]                             #####



# ------------------------------------------------------------------------------


# Fixing gadm1_08_20r ----------------------------------------------------------

# The object gadm1_08_20r cannot be thinned right away because there is an issue
# in this object: 2 provinces are duplicated. The code below fixes that issue:
## dataframe <- gadm1_08_20r@data   # the data frame of the provinces
## province <- dataframe$VARNAME_1  # the names of the provinces
# Merging the polygons by provinces.
# This has the side effects of
#   * removing the data frame;
#   * putting province as IDs of the polygons.
## tmp <- province[!province %in% names(dictionary)]
## hash <- c(dictionary, setNames(tmp, tmp))
## province <- hash[province]
## dataframe$province <- province
## gadm1_08_20r <- unionSpatialPolygons(gadm1_08_20r, province)



# thinning gadm1_08_20r:
## dataframe <- subset(dataframe, CCA_1 != "0") # remove the provinces with CCA_1 = 0
# Putting back the fixed data frame (i.e. fixing the first side effect).
# This in turn has the side effects of
#   * changing the order of the rows of the data frame so that its variable
#     "province" is in the same order as the ID of the polygons);
#   * putting the variable "province" as the rownames of the data frame.
## gadm1_08_20r <- SpatialPolygonsDataFrame(gadm1_08_20r, dataframe, "province")



# Same for gadm1_04_07r --------------------------------------------------------

dataframe <- gadm1_04_07r@data
province <- dataframe$VARNAME_2
tmp <- province[!province %in% names(dictionary)]
hash <- c(dictionary, setNames(tmp, tmp))
province <- hash[province]
dataframe$province <- province
gadm1_04_07r <- unionSpatialPolygons(gadm1_04_07r, province)
gadm1_04_07r <- SpatialPolygonsDataFrame(gadm1_04_07r, dataframe, "province")



# Generating the historical provinces maps -------------------------------------

# From 2004 to 2007:
prov <- sub("^Dien Bien$", "Lai Chau", gadm1_04_07r$province)
prov <- sub("^Dak Lak$",   "Dack Lak", prov)
prov <- sub("^Dak Nong$",  "Dack Lak", prov)
prov <- sub("^Hau Giang$",   "Can Tho", prov)
gadm1_97_03r <- unionSpatialPolygons(gadm1_04_07r, prov)
IDs <- sapply(gadm1_97_03r@polygons, function(x) x@ID)
gadm1_97_03r <- SpatialPolygonsDataFrame(gadm1_97_03r,
                                         data.frame(province = IDs,
                                                    row.names = IDs))

# From 1997 to 2003:
prov <- sub("^Thai Nguyen$", "Bac Thai", gadm1_97_03r$province)
prov <- sub("^Bac Kan$",     "Bac Thai", prov)
prov <- sub("^Vinh Phuc$",   "Vinh Phu", prov)
prov <- sub("^Phu Tho$",     "Vinh Phu", prov)
prov <- sub("^Bac Giang$",   "Ha Bac", prov)
prov <- sub("^Bac Ninh$",    "Ha Bac", prov)
prov <- sub("^Hai Duong$",   "Hai Hung", prov)
prov <- sub("^Hung Yen$",    "Hai Hung", prov)
prov <- sub("^Nam Dinh$",    "Nam Ha", prov)
prov <- sub("^Ha Nam$",      "Nam Ha", prov)
prov <- sub("^Quang Nam$",   "Quang Nam - Da Nang", prov)
prov <- sub("^Da Nang$",     "Quang Nam - Da Nang", prov)
prov <- sub("^Binh Duong$",  "Song Be", prov)
prov <- sub("^Binh Phuoc$",  "Song Be", prov)
prov <- sub("^Ca Mau$",      "Minh Hai", prov)
prov <- sub("^Bac Lieu$",    "Minh Hai", prov)
gadm1_92_96r <- unionSpatialPolygons(gadm1_97_03r, prov)
IDs <- sapply(gadm1_92_96r@polygons, function(x) x@ID)
gadm1_92_96r <- SpatialPolygonsDataFrame(gadm1_92_96r,
                                         data.frame(province = IDs,
                                                    row.names = IDs))

# From 1992 to 1996:
prov <- sub("^Ha Giang$",    "Ha Tuyen", gadm1_92_96r$province)
prov <- sub("^Tuyen Quang$", "Ha Tuyen", prov)
prov <- sub("^Yen Bai$",     "Hoang Lien Son", prov)
prov <- sub("^Lao Cai$",     "Hoang Lien Son", prov)
prov <- sub("^Hoa Binh$",    "Ha Son Binh", prov)
prov <- sub("^Ha Tay$",      "Ha Son Binh", prov)
prov <- sub("^Nam Ha$",      "Ha Nam Ninh", prov)
prov <- sub("^Ninh Binh$",   "Ha Nam Ninh", prov)
prov <- sub("^Gia Lai$",     "Gia Lai - Kon Tum", prov)
prov <- sub("^Kon Tum$",     "Gia Lai - Kon Tum", prov)
prov <- sub("^Binh Thuan$",  "Thuan Hai", prov)
prov <- sub("^Ninh Thuan$",  "Thuan Hai", prov)
prov <- sub("^Tra Vinh$",    "Cuu Long", prov)
prov <- sub("^Vinh Long$",   "Cuu Long", prov)
prov <- sub("^Can Tho$",     "Hau Giang", prov)
prov <- sub("^Soc Trang$",   "Hau Giang", prov)
gadm1_91_91r <- unionSpatialPolygons(gadm1_92_96r, prov)
IDs <- sapply(gadm1_91_91r@polygons, function(x) x@ID)
gadm1_91_91r <- SpatialPolygonsDataFrame(gadm1_91_91r,
                                         data.frame(province = IDs,
                                                    row.names = IDs))

# For 1991:
prov <- sub("^Nghe An$", "Nghe Tinh", gadm1_91_91r$province)
prov <- sub("^Ha Tinh$", "Nghe Tinh", prov)
gadm1_90_90r <- unionSpatialPolygons(gadm1_91_91r, prov)
IDs <- sapply(gadm1_90_90r@polygons, function(x) x@ID)
gadm1_90_90r <- SpatialPolygonsDataFrame(gadm1_90_90r,
                                         data.frame(province = IDs,
                                                    row.names = IDs))

# For 1990:
prov <- sub("^Quang Binh$",       "Binh Tri Thien", gadm1_90_90r$province)
prov <- sub("^Quang Tri$",        "Binh Tri Thien", prov)
prov <- sub("^Thua Thien - Hue$", "Binh Tri Thien", prov)
prov <- sub("^Quang Ngai$",       "Nghia Binh", prov)
prov <- sub("^Binh Dinh$",        "Nghia Binh", prov)
prov <- sub("^Phu Yen$",          "Phu Khanh", prov)
prov <- sub("^Khanh Hoa$",        "Phu Khanh", prov)
gadm1_79_89r <- unionSpatialPolygons(gadm1_90_90r, prov)
IDs <- sapply(gadm1_79_89r@polygons, function(x) x@ID)
gadm1_79_89r <- SpatialPolygonsDataFrame(gadm1_79_89r,
                                         data.frame(province = IDs,
                                                    row.names = IDs))



# Redefining the data slots for the most 2 recent maps:
## gadm1_08_20r@data <- gadm1_08_20r@data[, "province", drop = FALSE]
gadm1_04_07r@data <- gadm1_04_07r@data[, "province", drop = FALSE]

# Making province variable in the data frame a character variable:
gadm1_97_03r@data <- data.frame(province = as.character(gadm1_97_03r@data$province),
                                row.names = as.character(gadm1_97_03r@data$province),
                                stringsAsFactors = FALSE)
gadm1_92_96r@data <- data.frame(province = as.character(gadm1_92_96r@data$province),
                                row.names = as.character(gadm1_92_96r@data$province),
                                stringsAsFactors = FALSE)
gadm1_91_91r@data <- data.frame(province = as.character(gadm1_91_91r@data$province),
                                row.names = as.character(gadm1_91_91r@data$province),
                                stringsAsFactors = FALSE)
gadm1_90_90r@data <- data.frame(province = as.character(gadm1_90_90r@data$province),
                                row.names = as.character(gadm1_90_90r@data$province),
                                stringsAsFactors = FALSE)
gadm1_79_89r@data <- data.frame(province = as.character(gadm1_79_89r@data$province),
                                row.names = as.character(gadm1_79_89r@data$province),
                                stringsAsFactors = FALSE)

# Features IDs -----------------------------------------------------------------

gadm1_04_07r <- spChFIDs(gadm1_04_07r, as.character(seq_len(length(gadm1_04_07r))))
gadm1_97_03r <- spChFIDs(gadm1_97_03r, as.character(seq_len(length(gadm1_97_03r))))
gadm1_92_96r <- spChFIDs(gadm1_92_96r, as.character(seq_len(length(gadm1_92_96r))))
gadm1_91_91r <- spChFIDs(gadm1_91_91r, as.character(seq_len(length(gadm1_91_91r))))
gadm1_90_90r <- spChFIDs(gadm1_90_90r, as.character(seq_len(length(gadm1_90_90r))))
gadm1_79_89r <- spChFIDs(gadm1_79_89r, as.character(seq_len(length(gadm1_79_89r))))

# Thinning ---------------------------------------------------------------------

gadm0 <- thinnedSpatialPoly(gadm0r, tolerance)
## gadm1_08_20 <- thinnedSpatialPoly(gadm1_08_20r, tolerance)
gadm1_04_07 <- thinnedSpatialPoly(gadm1_04_07r, tolerance)
gadm1_97_03 <- thinnedSpatialPoly(gadm1_97_03r, tolerance)
gadm1_92_96 <- thinnedSpatialPoly(gadm1_92_96r, tolerance)
gadm1_91_91 <- thinnedSpatialPoly(gadm1_91_91r, tolerance)
gadm1_90_90 <- thinnedSpatialPoly(gadm1_90_90r, tolerance)
gadm1_79_89 <- thinnedSpatialPoly(gadm1_79_89r, tolerance)

# Defining the same boundaries box for all years: ------------------------------

boundbox <- bbox(gadm1_04_07)
gadm1_08_20r@bbox <- gadm1_97_03r@bbox <- gadm1_92_96r@bbox <- boundbox
gadm1_91_91r@bbox <- gadm1_90_90r@bbox <- gadm1_79_89r@bbox <- boundbox
gadm1_04_07r@bbox <- gadm1_08_20@bbox  <- gadm1_97_03@bbox  <- boundbox
gadm1_92_96@bbox  <- gadm1_91_91@bbox  <- gadm1_90_90@bbox  <- boundbox
gadm1_79_89@bbox  <- boundbox

# Defining the same projections for all map objects: ---------------------------

pj <- proj4string(gadm1_04_07)
proj4string(gadm1_79_89)  <- proj4string(gadm1_90_90)  <- proj4string(gadm1_91_91)  <- pj
proj4string(gadm1_92_96)  <- proj4string(gadm1_97_03)  <- proj4string(gadm1_08_20)  <- pj
proj4string(gadm1_04_07r) <- proj4string(gadm1_79_89r) <- proj4string(gadm1_90_90r) <- pj
proj4string(gadm1_91_91r) <- proj4string(gadm1_92_96r) <- proj4string(gadm1_97_03r) <- pj
proj4string(gadm1_08_20r) <- pj

# defining the maps with the Hanoi, Ha Tay, Hoa Binh, Hoa Son Binh merged: -----
# these maps are useful in case of time series that start before 1992-01-01 and
# end after 2007-12-31.

prov <- sub("^Ha Son Binh$", "Ha Noi", gadm1_79_89$province)
gadm1_79_89_hn <- unionSpatialPolygons(gadm1_79_89, prov)
IDs <- sapply(gadm1_79_89_hn@polygons, function(x) x@ID)
gadm1_79_89_hn <- SpatialPolygonsDataFrame(gadm1_79_89_hn,
                                           data.frame(province = IDs,
                                                      row.names = IDs))
prov <- sub("^Ha Son Binh$", "Ha Noi", gadm1_90_90$province)
gadm1_90_90_hn <- unionSpatialPolygons(gadm1_90_90, prov)
IDs <- sapply(gadm1_90_90_hn@polygons, function(x) x@ID)
gadm1_90_90_hn <- SpatialPolygonsDataFrame(gadm1_90_90_hn,
                                           data.frame(province = IDs,
                                                      row.names = IDs))
prov <- sub("^Ha Son Binh$", "Ha Noi", gadm1_79_89r$province)
gadm1_79_89r_hn <- unionSpatialPolygons(gadm1_79_89r, prov)
IDs <- sapply(gadm1_79_89r_hn@polygons, function(x) x@ID)
gadm1_79_89r_hn <- SpatialPolygonsDataFrame(gadm1_79_89r_hn,
                                            data.frame(province = IDs,
                                                       row.names = IDs))
prov <- sub("^Ha Son Binh$", "Ha Noi", gadm1_90_90r$province)
gadm1_90_90r_hn <- unionSpatialPolygons(gadm1_90_90r, prov)
IDs <- sapply(gadm1_90_90r_hn@polygons, function(x) x@ID)
gadm1_90_90r_hn <- SpatialPolygonsDataFrame(gadm1_90_90r_hn,
                                            data.frame(province = IDs,
                                                       row.names = IDs))
prov <- sub("^Ha Son Binh$", "Ha Noi", gadm1_91_91$province)
gadm1_91_91_hn <- unionSpatialPolygons(gadm1_91_91, prov)
IDs <- sapply(gadm1_91_91_hn@polygons, function(x) x@ID)
gadm1_91_91_hn <- SpatialPolygonsDataFrame(gadm1_91_91_hn,
                                           data.frame(province = IDs,
                                                      row.names = IDs))
prov <- sub("^Ha Tay$", "Ha Noi", gadm1_92_96$province)
gadm1_92_96_hn <- unionSpatialPolygons(gadm1_92_96, prov)
IDs <- sapply(gadm1_92_96_hn@polygons, function(x) x@ID)
gadm1_92_96_hn <- SpatialPolygonsDataFrame(gadm1_92_96_hn,
                                           data.frame(province = IDs,
                                                      row.names = IDs))
prov <- sub("^Ha Tay$", "Ha Noi", gadm1_97_03$province)
gadm1_97_03_hn <- unionSpatialPolygons(gadm1_97_03, prov)
IDs <- sapply(gadm1_97_03_hn@polygons, function(x) x@ID)
gadm1_97_03_hn <- SpatialPolygonsDataFrame(gadm1_97_03_hn,
                                           data.frame(province = IDs,
                                                      row.names = IDs))
prov <- sub("^Ha Tay$", "Ha Noi", gadm1_04_07$province)
gadm1_04_07_hn <- unionSpatialPolygons(gadm1_04_07, prov)
IDs <- sapply(gadm1_04_07_hn@polygons, function(x) x@ID)
gadm1_04_07_hn <- SpatialPolygonsDataFrame(gadm1_04_07_hn,
                                           data.frame(province = IDs,
                                                      row.names = IDs))
prov <- sub("^Ha Son Binh$", "Ha Noi", gadm1_91_91r$province)                ###
gadm1_91_91r_hn <- unionSpatialPolygons(gadm1_91_91r, prov)
IDs <- sapply(gadm1_91_91r_hn@polygons, function(x) x@ID)
gadm1_91_91r_hn <- SpatialPolygonsDataFrame(gadm1_91_91r_hn,
                                            data.frame(province = IDs,
                                                       row.names = IDs))
prov <- sub("^Ha Tay$", "Ha Noi", gadm1_92_96r$province)
gadm1_92_96r_hn <- unionSpatialPolygons(gadm1_92_96r, prov)
IDs <- sapply(gadm1_92_96r_hn@polygons, function(x) x@ID)
gadm1_92_96r_hn <- SpatialPolygonsDataFrame(gadm1_92_96r_hn,
                                            data.frame(province = IDs,
                                                       row.names = IDs))
prov <- sub("^Ha Tay$", "Ha Noi", gadm1_97_03r$province)
gadm1_97_03r_hn <- unionSpatialPolygons(gadm1_97_03r, prov)
IDs <- sapply(gadm1_97_03r_hn@polygons, function(x) x@ID)
gadm1_97_03r_hn <- SpatialPolygonsDataFrame(gadm1_97_03r_hn,
                                            data.frame(province = IDs,
                                                       row.names = IDs))
prov <- sub("^Ha Tay$", "Ha Noi", gadm1_04_07r$province)
gadm1_04_07r_hn <- unionSpatialPolygons(gadm1_04_07r, prov)
IDs <- sapply(gadm1_04_07r_hn@polygons, function(x) x@ID)
gadm1_04_07r_hn <- SpatialPolygonsDataFrame(gadm1_04_07r_hn,
                                            data.frame(province = IDs,
                                                       row.names = IDs))
gadm1_79_89_hn <- spChFIDs(gadm1_79_89_hn, as.character(seq_len(length(gadm1_79_89_hn))))
gadm1_90_90_hn <- spChFIDs(gadm1_90_90_hn, as.character(seq_len(length(gadm1_90_90_hn))))
gadm1_91_91_hn <- spChFIDs(gadm1_91_91_hn, as.character(seq_len(length(gadm1_91_91_hn))))
gadm1_92_96_hn <- spChFIDs(gadm1_92_96_hn, as.character(seq_len(length(gadm1_92_96_hn))))
gadm1_97_03_hn <- spChFIDs(gadm1_97_03_hn, as.character(seq_len(length(gadm1_97_03_hn))))
gadm1_04_07_hn <- spChFIDs(gadm1_04_07_hn, as.character(seq_len(length(gadm1_04_07_hn))))
gadm1_79_89r_hn <- spChFIDs(gadm1_79_89r_hn, as.character(seq_len(length(gadm1_79_89r_hn))))
gadm1_90_90r_hn <- spChFIDs(gadm1_90_90r_hn, as.character(seq_len(length(gadm1_90_90r_hn))))
gadm1_91_91r_hn <- spChFIDs(gadm1_91_91r_hn, as.character(seq_len(length(gadm1_91_91r_hn))))
gadm1_92_96r_hn <- spChFIDs(gadm1_92_96r_hn, as.character(seq_len(length(gadm1_92_96r_hn))))
gadm1_97_03r_hn <- spChFIDs(gadm1_97_03r_hn, as.character(seq_len(length(gadm1_97_03r_hn))))
gadm1_04_07r_hn <- spChFIDs(gadm1_04_07r_hn, as.character(seq_len(length(gadm1_04_07r_hn))))
gadm1_79_89_hn@bbox  <- boundbox
gadm1_90_90_hn@bbox  <- boundbox
gadm1_91_91_hn@bbox  <- boundbox
gadm1_92_96_hn@bbox  <- boundbox
gadm1_97_03_hn@bbox  <- boundbox
gadm1_04_07_hn@bbox  <- boundbox
gadm1_79_89r_hn@bbox  <- boundbox
gadm1_90_90r_hn@bbox  <- boundbox
gadm1_91_91r_hn@bbox  <- boundbox
gadm1_92_96r_hn@bbox  <- boundbox
gadm1_97_03r_hn@bbox  <- boundbox
gadm1_04_07r_hn@bbox  <- boundbox
proj4string(gadm1_79_89_hn) <- pj
proj4string(gadm1_90_90_hn) <- pj
proj4string(gadm1_91_91_hn) <- pj
proj4string(gadm1_92_96_hn) <- pj
proj4string(gadm1_97_03_hn) <- pj
proj4string(gadm1_04_07_hn) <- pj
proj4string(gadm1_79_89r_hn) <- pj
proj4string(gadm1_90_90r_hn) <- pj
proj4string(gadm1_91_91r_hn) <- pj
proj4string(gadm1_92_96r_hn) <- pj
proj4string(gadm1_97_03r_hn) <- pj
proj4string(gadm1_04_07r_hn) <- pj

# Defining the regions: --------------------------------------------------------

regions <- read.table("data-raw/regions.txt", sep = "\t", stringsAsFactors = FALSE)[, c(1, 7)]
names(regions) <- c("province", "region")
regions[, 1] <- sub(" Province", "", regions[, 1])
regions[, 1] <- sub(" City", "", regions[, 1])
colors <- list(Northwest             = c(243, 225,   0),
               Northeast             = c(255, 175,  26),
               "Red River Delta"     = c(255, 103, 103),
               "North Central Coast" = c(  0, 214,   0),
               "South Central Coast" = c(  0, 221, 217),
               "Central Highlands"   = c( 36, 135, 255),
               Southeast             = c(195,  36, 255),
               "Mekong Delta"        = c(255,  36, 196))
colors <- sapply(colors, function(x) rgb(x[1], x[2], x[3], max = 255))
regions$color <- colors[regions$region]
gadm1_08_20 <- merge(gadm1_08_20, regions)
gadm1_08_20r <- merge(gadm1_08_20r, regions)

# Saving -----------------------------------------------------------------------

eply::evals(paste0("devtools::use_data(",
            paste(grep("gadm\\d", ls(), value = TRUE), collapse = ", "),
            ", internal = TRUE, overwrite = TRUE)"))





