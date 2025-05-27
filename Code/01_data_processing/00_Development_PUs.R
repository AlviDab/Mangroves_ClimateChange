#Author: Alvise Dabalà
#Date: 18/04/2023

# Updated by Jason Everett (UQ) 14th march 2024

# install.packages("devtools")
devtools::install_github("https://github.com/MathMarEcol/spatialplanr")

# install.packages("devtools")
devtools::install_github("emlab-ucsb/spatialgridr")

# devtools::install_github("emlab-ucsb/spatialgridr")
pacman::p_load(sf, tidyverse, spatialgridr, spatialplanr)

moll_proj <- "ESRI:54009"

gmw <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
  st_transform(moll_proj) %>%
  st_make_valid()

# It takes too long to start with a global set of PUs.
# Lets try getting the PUs within the bbox of the GMW data
# then check the intersection of the coverage of the PUs for the actual GMW data.

bbox_coords <- c(0, 0, 5, 5)
names(bbox_coords) = c("xmin","ymin","xmax","ymax")

bb = st_bbox(bbox_coords)
st_crs(bb) = 4326

bb <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
  st_bbox()

bb["ymin"] <- floor(bb["ymin"]) # Round the limits or they won't form a complete boundary
bb["ymax"] = ceiling(bb["ymax"])

bndry <- spatialplanr::splnr_get_boundary(bb, res = 1) %>%
  st_sf() # Get a boundary

# Get the PUs for the broader bounding box
PUs <- spatialgridr::get_grid(boundary = bb,
                              crs = moll_proj,
                              output = "sf_hex",
                              resolution = 27000) %>%
  sf::st_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number())

# Get the larger PUs for the visualisation
PUs_large <- spatialgridr::get_grid(bndry,
                                    output = "sf_hex",
                                    crs = moll_proj,
                                    resolution = 270000) %>%
  sf::st_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number())

gg <- ggplot() +
  geom_sf(data = PUs, linewidth = 0.00001)

ggsave("Figures/00_bbox_PUs.pdf", gg)

# Now we only want the ones that intersect with
overlap <- sf::st_intersects(PUs, gmw) %>%
  lengths() > 0

overlap_large <- sf::st_intersects(PUs_large, gmw) %>%
  lengths() > 0

PUs <- PUs[overlap,]
PUs_large <- PUs_large[overlap_large,]

# Lets check it's working ok
gg <- ggplot() +
  geom_sf(data = gmw, linewidth = 0.0001, colour = "red", fill = NA) +
  geom_sf(data = PUs, linewidth = 0.0001, fill = NA, colour = "blue")

ggsave("Figures/00_mangrove_PUs.pdf", gg, width = 20, height = 5)

# Next we can run an intersection to return the actual overlap for each PU to calculate cutoffs
area <- sf::st_intersection(gmw, PUs) %>%
  dplyr::mutate(MangroveArea_km2 = as.numeric(units::set_units(sf::st_area(.), "km2"))) %>%
  dplyr::group_by(cellID) %>%
  sf::st_drop_geometry() %>%
  summarise(MangroveArea_km2 = sum(MangroveArea_km2))

PUs <- PUs %>%
  left_join(area, by = "cellID") %>%
  dplyr::mutate(PUArea_km2 = as.numeric(units::set_units(sf::st_area(.), "km2")),
                MangroveProp = MangroveArea_km2/PUArea_km2)

saveRDS(PUs, file = "Results/RDS/00_PUs_mollweide.rds")
st_write(PUs, "Results/gpkg/00_PUs_mollweide.gpkg")

saveRDS(PUs_large, file = "Results/RDS/00_PUs_large_mollweide.rds")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
