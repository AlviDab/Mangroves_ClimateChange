#Author: Alvise Dabalà
#Date: 18/04/2023

# Updated by Jason Everett (UQ) 14th march 2024

# install.packages("devtools")
devtools::install_github("https://github.com/MathMarEcol/spatialplanr")

# install.packages("devtools")
devtools::install_github("emlab-ucsb/spatialgridr")

# devtools::install_github("emlab-ucsb/spatialgridr")
pacman::p_load(sf, tidyverse, spatialgridr, spatialplanr, parallelly, future.apply)

#I move the lon_0 of the projection so that overlapping PUs
#are in the middle of the Pacific
cCRS <- "+proj=cea +lat_ts=30 +lon_0=+44 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

# It takes too long to start with a global set of PUs.
# Lets try getting the PUs within the bbox of the GMW data
# then check the intersection of the coverage of the PUs for the actual GMW data.

# biotyp <- sf::st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp") %>%
#   st_make_valid() %>%
#   st_transform(cCRS) %>%
#   st_make_valid()

#biotyp_moll <- saveRDS(biotyp, "Results/RDS/mangroves_biotyp_mollweide.rds")

biotyp <- sf::st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp") %>%
  st_make_valid()

biotyp_area_geod <- biotyp %>%
  lwgeom::st_geod_area() #Use ellipsoid calculation of area

bb <- st_bbox(c(xmin = -180.0000, ymin = -39.0000, xmax = 180.000, ymax = 33.0000))

bndry <- spatialplanr::splnr_get_boundary(bb, cCRS = cCRS)

# grid <- dggridR::dgconstruct(projection = "ISEA",
#                              spacing = 27.75) %>%
#   dggridR::dgearthgrid()

PUs_all <- st_make_grid(bndry,
                        cellsize = 27750,
                        crs = cCRS,
                        square = FALSE) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number()) %>%
  st_transform("EPSG:4326") %>%
  st_make_valid() %>%
  st_wrap_dateline()

saveRDS(PUs, "Results/RDS/PUs_00_all_EPSG4326.rds")

overlap <- sf::st_intersects(PUs_all, biotyp) %>%
  lengths() > 0

PUs <- PUs_all[overlap,]

PUs_area <- lwgeom::st_geod_area(PUs)

#Percentage difference in area
(max(PUs_area) - min(PUs_area)) / max(PUs_area)

st_write(PUs, "Results/gpkg/00_PUs.gpkg", append = FALSE)
saveRDS(PUs, "Results/RDS/PUs_00_EPSG4326.rds")

# Get the larger PUs for the visualisation
PUs_large_all <- sf::st_make_grid(x = bndry, cellsize = 2755000,
                                  crs = cCRS,
                                  square = FALSE) %>%
  sf::st_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number()) %>%
  st_transform("EPSG:4326") %>%
  st_make_valid() %>%
  st_wrap_dateline()

overlap_large <- sf::st_intersects(PUs_large_all, biotyp) %>%
  lengths() > 0

PUs_large <- PUs_large_all[overlap_large,]

#Intersect with biotyp and calculate area
biotyp <- biotyp[1:500,]

#Fist we cast biotyp to "POLYGON" to make a faster intersection
biotyp <- biotyp %>%
  st_cast("POLYGON")

# #Split by the number of cores to parallelise
n_cores <- parallelly::availableCores() - 8

split_biotyp <- biotyp %>%
  split(seq(1:n_cores))

rm(biotyp)

#We intersect
plan(multisession, workers = n_cores)

PUs <- readRDS("Results/RDS/PUs_00_EPSG4326.rds")

tictoc::tic()
intersection_biotyp_PUs <- split_biotyp %>%
      future_lapply(function(x) {
        st_intersection(x, PUs)
      })
tictoc::toc()

plan(sequential)

#We save the result as RDS (needed to also calculate the area of each biophysical typology in each PU)
saveRDS(intersection_biotyp_PUs, "Results/RDS/PUs_00_intersection_biotyp_1_500.rds")

# Next we can run an intersection to return the actual overlap for each PU to calculate cutoffs
area <- intersection_biotyp_PUs %>%
  dplyr::mutate(MangroveArea_km2 = as.numeric(units::set_units(lwgeom::st_geod_area(.), "km2"))) %>%
  dplyr::group_by(cellID) %>%
  sf::st_drop_geometry() %>%
  summarise(MangroveArea_km2 = sum(MangroveArea_km2))

#TODO::Still need to calculate the area for each class of biophysical typology (as it was in 01_CalculateArea...)

PUs <- PUs %>%
  left_join(area, by = "cellID") %>%
  dplyr::mutate(PUArea_km2 = as.numeric(units::set_units(sf::st_area(.), "km2")),
                MangroveProp = MangroveArea_km2/PUArea_km2)

saveRDS(PUs, file = "Results/RDS/PUs_00_EPSG4326_mangrove_area.rds")
st_write(PUs, "Results/gpkg/PUs_00_EPSG4326_mangrove_area.gpkg")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.