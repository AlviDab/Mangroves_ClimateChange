pacman::p_load(sf, tidyverse, reprex)

#Mollweide projection
moll_proj <- "ESRI:54009"

sf_use_s2(FALSE)

#Open the GMW map
biotyp <- sf::st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp")

bb <- biotyp %>%
  st_bbox()

bb["ymin"] <- floor(bb["ymin"]) # Round the limits or they won't form a complete boundary
bb["ymax"] = ceiling(bb["ymax"])

bndry <- spatialplanr::splnr_get_boundary(bb, cCRS = moll_proj)

PUs <- sf::st_make_grid(x = bndry, cellsize = 27000,
                        crs = moll_proj,
                        square = FALSE) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  st_area()

