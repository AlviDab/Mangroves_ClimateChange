#Author: Dabalà Alvise
#Date: 2023-09-15
################################################################################

f_worldmap <- function(scale_map = "large") {
  pacman::p_load(rnaturalearth, tidyverse, sf)

  moll_proj <- "ESRI:54009"

  #Layers needed to plot the maps
  world_map <- rnaturalearth::ne_countries(scale = scale_map,
                                           type = "map_units",
                                           returnclass = "sf") %>%
    st_break_antimeridian(lon_0=0) %>%
    st_transform(crs = moll_proj) %>%
    st_make_valid()
}