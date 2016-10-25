library(maptools) # for "thinnedSpatialPoly", "unionSpatialPolygons"
tolerance <- .01 # the tolerance parameter of the thinning function


# ---- Downloading the country and provinces maps from GADM (www.gadm.org): ----
gadm0r <- raster::getData("GADM",country="VNM",level=0)
gadm1_08_20r <- raster::getData("GADM",country="VNM",level=1)
load("VNM_adm2.RData") # the 64 provinces from 2004 to 2007
gadm1_04_07r <- gadm


# The hash table for the new provinces names variable: -------------------------
dictionary <- c(              "Bac Kan|Bac Can" = "Bac Kan",
                         "Da Nang City|Da Nang" = "Da Nang",
                              "Dak Lak|Dac Lac" = "Dak Lak",
                                     "Dac Nong" = "Dak Nong",
                            "Ha Noi City|Hanoi" = "Hanoi",
                      "Hai Phong City|Haiphong" = "Hai Phong",
                 "Ho Chi Minh City|Ho Chi Minh" = "Tp. Ho Chi Minh",
                "Ba Ria - VTau|Ba Ria-Vung Tau" = "Vung Tau - Ba Ria")


# --------------------------- Fixing gadm1_08_20r: -----------------------------
# The object gadm1_08_20r cannot be thinned right away because there is an issue
# in this object: 2 provinces are duplicated. The code below fixes that issue:
dataframe <- gadm1_08_20r@data # the data frame of the provinces
province <- dataframe$VARNAME_1 # the names of the provinces
# Merging the polygons by provinces.
# This has the side effects of
#   * removing the data frame;
#   * putting province as IDs of the polygons.
tmp <- province[!province %in% names(dictionary)]
hash <- c(dictionary,setNames(tmp,tmp))
province <- hash[province]
dataframe$province <- province
gadm1_08_20r <- unionSpatialPolygons(gadm1_08_20r,province)
# thinning gadm1_08_20r:
dataframe <- subset(dataframe,CCA_1!="0") # remove the provinces with CCA_1 = 0
# Putting back the fixed data frame (i.e. fixing the first side effect).
# This in turn has the side effects of
#   * changing the order of the rows of the data frame so that its variable
#     "province" is in the same order as the ID of the polygons);
#   * putting the variable "province" as the rownames of the data frame.
gadm1_08_20r <- SpatialPolygonsDataFrame(gadm1_08_20r,dataframe,"province")


# Same for gadm1_04_07r: -------------------------------------------------------
dataframe <- gadm1_04_07r@data
province <- dataframe$VARNAME_2
tmp <- province[!province %in% names(dictionary)]
hash <- c(dictionary,setNames(tmp,tmp))
province <- hash[province]
dataframe$province <- province
gadm1_04_07r <- unionSpatialPolygons(gadm1_04_07r,province)
gadm1_04_07r <- SpatialPolygonsDataFrame(gadm1_04_07r,dataframe,"province")


# ------------------ Generating the historical provinces maps: -----------------
## From 2004 to 2007:
prov <- sub("^Dien Bien$","Lai Chau",gadm1_04_07r$province)
prov <- sub("^Dak Lak$","Dack Lak",prov)
prov <- sub("^Dak Nong$","Dack Lak",prov)
prov <- sub("^Can Tho$","Hau Giang",prov)
gadm1_97_03r <- unionSpatialPolygons(gadm1_04_07r,prov)
IDs <- sapply(gadm1_97_03r@polygons,function(x)x@ID)
gadm1_97_03r <- SpatialPolygonsDataFrame(gadm1_97_03r,
                                        data.frame(province=IDs,row.names=IDs))
## From 1997 to 2003:
prov <- sub("^Thai Nguyen$","Bac Thai",gadm1_97_03r$province)
prov <- sub("^Bac Kan$","Bac Thai",prov)
prov <- sub("^Vinh Phuc$","Vinh Phu",prov)
prov <- sub("^Phu Tho$","Vinh Phu",prov)
prov <- sub("^Bac Giang$","Ha Bac",prov)
prov <- sub("^Bac Ninh$","Ha Bac",prov)
prov <- sub("^Hai Duong$","Hai Hung",prov)
prov <- sub("^Hung Yen$","Hai Hung",prov)
prov <- sub("^Nam Dinh$","Nam Ha",prov)
prov <- sub("^Ha Nam$","Nam Ha",prov)
prov <- sub("^Quang Nam$","Q. Nam - Da Nang",prov)
prov <- sub("^Da Nang$","Q. Nam - Da Nang",prov)
prov <- sub("^Binh Duong$","Song Be",prov)
prov <- sub("^Binh Phuoc$","Song Be",prov)
prov <- sub("^Ca Mau$","Minh Hai",prov)
prov <- sub("^Bac Lieu$","Minh Hai",prov)
gadm1_92_96r <- unionSpatialPolygons(gadm1_97_03r,prov)
IDs <- sapply(gadm1_92_96r@polygons,function(x)x@ID)
gadm1_92_96r <- SpatialPolygonsDataFrame(gadm1_92_96r,
                                        data.frame(province=IDs,row.names=IDs))
## From 1992 to 1996:
prov <- sub("^Ha Giang$","Ha Tuyen",gadm1_92_96r$province)
prov <- sub("^Tuyen Quang$","Ha Tuyen",prov)
prov <- sub("^Yen Bai$","Hoang Lien Son",prov)
prov <- sub("^Lao Cai$","Hoang Lien Son",prov)
prov <- sub("^Hoa Binh$","Ha Son Binh",prov)
prov <- sub("^Ha Tay$","Ha Son Binh",prov)
prov <- sub("^Nam Ha$","Ha Nam Ninh",prov)
prov <- sub("^Ninh Binh$","Ha Nam Ninh",prov)
prov <- sub("^Gia Lai$","Gia Lai - Kon Tum",prov)
prov <- sub("^Kon Tum$","Gia Lai - Kon Tum",prov)
prov <- sub("^Binh Thuan$","Thuan Hai",prov)
prov <- sub("^Ninh Thuan$","Thuan Hai",prov)
prov <- sub("^Tra Vinh$","Cuu Long",prov)
prov <- sub("^Vinh Long$","Cuu Long",prov)
prov <- sub("^Soc Trang$","Hau Giang",prov)
gadm1_91_91r <- unionSpatialPolygons(gadm1_92_96r,prov)
IDs <- sapply(gadm1_91_91r@polygons,function(x)x@ID)
gadm1_91_91r <- SpatialPolygonsDataFrame(gadm1_91_91r,
                                        data.frame(province=IDs,row.names=IDs))
## For 1991:
prov <- sub("^Nghe An$","Nghe Tinh",gadm1_91_91r$province)
prov <- sub("^Ha Tinh$","Nghe Tinh",prov)
gadm1_90_90r <- unionSpatialPolygons(gadm1_91_91r,prov)
IDs <- sapply(gadm1_90_90r@polygons,function(x)x@ID)
gadm1_90_90r <- SpatialPolygonsDataFrame(gadm1_90_90r,
                                        data.frame(province=IDs,row.names=IDs))
## For 1990:
prov <- sub("^Quang Binh$","Binh Tri Thien",gadm1_90_90r$province)
prov <- sub("^Quang Tri$","Binh Tri Thien",prov)
prov <- sub("^Thua Thien - Hue$","Binh Tri Thien",prov)
prov <- sub("^Quang Ngai$","Nghia Binh",prov)
prov <- sub("^Binh Dinh$","Nghia Binh",prov)
prov <- sub("^Phu Yen$","Phu Khanh",prov)
prov <- sub("^Khanh Hoa$","Phu Khanh",prov)
gadm1_79_89r <- unionSpatialPolygons(gadm1_90_90r,prov)
IDs <- sapply(gadm1_79_89r@polygons,function(x)x@ID)
gadm1_79_89r <- SpatialPolygonsDataFrame(gadm1_79_89r,
                                        data.frame(province=IDs,row.names=IDs))


# --------------------------------- Thinning: ----------------------------------
gadm0 <- thinnedSpatialPoly(gadm0r,tolerance)
gadm1_08_20 <- thinnedSpatialPoly(gadm1_08_20r,tolerance)
gadm1_04_07 <- thinnedSpatialPoly(gadm1_04_07r,tolerance)
gadm1_97_03 <- thinnedSpatialPoly(gadm1_97_03r,tolerance)
gadm1_92_96 <- thinnedSpatialPoly(gadm1_92_96r,tolerance)
gadm1_91_91 <- thinnedSpatialPoly(gadm1_91_91r,tolerance)
gadm1_90_90 <- thinnedSpatialPoly(gadm1_90_90r,tolerance)
gadm1_79_89 <- thinnedSpatialPoly(gadm1_79_89r,tolerance)


# ---------------------------------- Saving: -----------------------------------
devtools::use_data(gadm0r,gadm0,gadm1_08_20r,gadm1_08_20,gadm1_04_07,gadm1_04_07r,
                   gadm1_97_03,gadm1_97_03r,gadm1_92_96,gadm1_92_96r,gadm1_91_91,
                   gadm1_91_91r,gadm1_90_90,gadm1_90_90r,gadm1_79_89,gadm1_79_89r,
                   internal=T,overwrite=T)


# --------------------------------- cleaning: ----------------------------------
rm(dataframe,tolerance,varname_2,province,gadm,tmp,prov,IDs,dictionary,hash)
