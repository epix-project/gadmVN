tolerance <- .01 # the tolerance parameter of the thinning function
# Downloading the country and provinces maps from GADM (www.gadm.org):
gadm0r <- raster::getData("GADM",country="VNM",level=0)
gadm1r <- raster::getData("GADM",country="VNM",level=1)
library(maptools) # for "thinnedSpatialPoly", "unionSpatialPolygons"
gadm0 <- thinnedSpatialPoly(gadm0r,tolerance) # thinning vietnam

# The object gadm1r cannot be thinned right away because there is an issue
# in this object: 2 provinces are duplicated. The code below fixes that issue:

# ----------------------- Fixing the provinces object: -------------------------
dataframe <- gadm1r@data # the data frame of the provinces
varname_1 <- dataframe$VARNAME_1 # the names of the provinces
# Merging the polygons by provinces.
# This has the side effects of
#   * removing the data frame;
#   * putting varname_1 as IDs of the polygons.
gadm1r <- unionSpatialPolygons(gadm1r,varname_1)
# thinning gadm1r:
dataframe <- subset(dataframe,CCA_1!="0") # remove the provinces with CCA_1 = 0
# Putting back the fixed data frame (i.e. fixing the first side effect).
# This in turn has the side effects of
#   * changing the order of the rows of the data frame so that its variable
#     VARNAME_1 is in the same order as the ID of the polygons);
#   * putting the variable "VARNAME_1" as the rownames of the data frame.
gadm1r <- SpatialPolygonsDataFrame(gadm1r,dataframe,"VARNAME_1")
# ------------------------------------------------------------------------------

# Now that provincesVN0 is fixed, we can thin it:
gadm1 <- thinnedSpatialPoly(gadm1r,tolerance)
# Adding a new column name for the provinces names:
sel <- c(1,3:15,17,16,18:28,30:58,29,59:62,2,63)
hash <- c(              "Bac Kan|Bac Can" = "Bac Kan",
                   "Da Nang City|Da Nang" = "Da Nang",
                        "Dak Lak|Dac Lac" = "Dak Lak",
                               "Dac Nong" = "Dak Nong",
                      "Ha Noi City|Hanoi" = "Hanoi",
                "Hai Phong City|Haiphong" = "Hai Phong",
           "Ho Chi Minh City|Ho Chi Minh" = "Tp. Ho Chi Minh",
          "Ba Ria - VTau|Ba Ria-Vung Tau" = "Vung Tau - Ba Ria")
tmp <- gadm1$VARNAME_1
tmp <- tmp[!tmp %in% names(hash)]
hash <- c(hash,setNames(tmp,tmp))
gadm1$province <- hash[gadm1$VARNAME_1]
gadm1r$province <- hash[gadm1r$VARNAME_1]
# Saving:
devtools::use_data(gadm0r,gadm0,gadm1r,gadm1,overwrite=T)
# cleaning:
rm(dataframe,tolerance,varname_1)
