#Author: Alvise Dabalà
#Date: 18/04/2023

# Updated by Jason Everett (UQ) 14th march 2023

# devtools::install_github("emlab-ucsb/spatialgridr")
pacman::p_load(sf, tidyverse, spatialgridr)

moll_proj <- "ESRI:54009"

# I used this uinioned dataset to run the intersection, but I am not sure if it is needed.
# I can send the file if you decide you need it.

# Here I use the un-unioned data
gmw <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
      st_transform(moll_proj) %>%
      st_make_valid()

# It takes too long to start with a global set of PUs.
# Lets try getting the PUs within the bbox of the GMW data
# then check the intersection of the coverage of the PUs for the actual GMW data.

bb <- sf::st_read("Data/gmw_v3_2020/vector/gmw_v3_2020_vec.shp") %>%
  st_bbox()
bb["ymin"] <- floor(bb["ymin"]) # Round the limits or they won't form a complete boundary
bb["ymax"] = ceiling(bb["ymax"])


bndry <- spatialplanr::splnr_get_boundary(bb, res = 1) # Get a boundary

# Get the PUs for the broader bounding box
PUs <- spatialgridr::get_grid(bndry, option = "sf_hex", projection_crs = moll_proj, resolution = 27000) %>%
  sf::st_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number())

gg <- ggplot() +
  geom_sf(data = PUs, linewidth = 0.00001)
ggsave("Figures/bbox_PUs.pdf", gg)

# Now we only want the ones that intersect with
overlap <- sf::st_intersects(PUs, gmw) %>%
  lengths() > 0

PUs <- PUs[overlap,]

# Lets check it's working ok
gg <- ggplot() +
  geom_sf(data = gmw, linewidth = 0.0001, colour = "red", fill = NA) +
  geom_sf(data = PUs, linewidth = 0.0001, fill = NA, colour = "blue")

ggsave("Figures/MangrovePUs.pdf", gg, width = 20, height = 5)

# Next we can run an intersection to return the actual overlap for each PU to calculate cutoffs.
area <- sf::st_intersection(gmw, PUs) %>%
  dplyr::mutate(MangroveArea_km2 = as.numeric(units::set_units(sf::st_area(.), "km2"))) %>%
  dplyr::group_by(cellID) %>%
  sf::st_drop_geometry() %>%
  summarise(MangroveArea_km2 = sum(MangroveArea_km2))

PUs <- PUs %>%
  left_join(area, by = "cellID") %>%
  dplyr::mutate(PUArea_km2 = as.numeric(units::set_units(sf::st_area(.), "km2")),
                MangroveProp = MangroveArea_km2/PUArea_km2)


saveRDS(PUs, file = "Output/PUs.rds")


###### END JASE CODE #######


# Figure out Mollweide coordinates

region <- rnaturalearth::ne_countries(scale = "medium") %>%
  sf::st_make_valid() %>%
  sf::st_crop(c(xmin = 120, xmax = 130, ymin = 0, ymax = 20)) %>%
  sf::st_transform(crs = moll_proj)


gmw4 <- gmw3 %>%
  sf::st_crop(sf::st_bbox())

rPUs <- get_grid2(gmw4, option = "sf_hex", projection_crs = moll_proj, resolution = 27000) %>%
  sf::st_sf()


(gg <- ggplot() +
    # geom_sf(data = gmw4, linewidth = 0.1, colour = "red", fill = NA) +
    geom_sf(data = rPUs, linewidth = 0.1, fill = NA) # +
  # coord_sf(xlim = c(), ylim = c())
)

ggsave("Figures/MargrovesOverlay.pdf", gg)









mangroves_distribution <- st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp") %>%
  st_transform(moll_proj) #%>%
# st_make_valid()

bndry <- mangroves_distribution %>%
  sf::st_make_valid() %>%
  sf::st_union()

#sf::sf_use_s2(FALSE)

dir.create("Results/RDS/", recursive = TRUE)
saveRDS(mangroves_distribution, "Results/RDS/mangroves_distribution_mollweide.rds")

source("Code/Functions/fCreate_PUs.R")

# PUs <- fCreate_PUs(mangroves_distribution, 655) #~0.25° at the equator, apothem of 13.5 km

dir.create("Results/gpkg", recursive = TRUE) j
saveRDS(PUs, "Results/RDS/00_PUs_mollweide.rds")
st_write(PUs, "Results/gpkg/00_PUs_mollweide.gpkg")

st_area(PUs[1,])

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()