#Author: Alvise Dabalà
#Date: 18/04/2023

# Updated by Jason Everett (UQ) 14th march 2024
# Edited by Tin Buenafe 20 May 2025 for HPC functionality

# install.packages("devtools")
# devtools::install_github("https://github.com/MathMarEcol/spatialplanr")

# install.packages("devtools")
# devtools::install_github("emlab-ucsb/spatialgridr")

# devtools::install_github("emlab-ucsb/spatialgridr")
# pacman::p_load(sf, tidyverse, spatialgridr, spatialplanr)

# Load libraries
library(sf)
library(tidyverse)
library(spatialgridr)
library(spatialplanr)

cat("Loaded libraries\n")

# Define directories
args = commandArgs(trailingOnly = TRUE)
INPUT_DIR = args[1] # 1st argument in the srun Rscript function is the the input directory
TMP_DIR = Sys.getenv("TMPDIR")
FIG_DIR = file.path(TMP_DIR, "Figures")
RESULTS_DIR = file.path(TMP_DIR, "Results")

cat("Defined directories\n")

# Create new directories
htr_make_folder <- function(folder) { # Function is from hotrstuff
  if (!isTRUE(file.info(folder)$isdir)) dir.create(folder, recursive = TRUE)
}
htr_make_folder(FIG_DIR)
htr_make_folder(RESULTS_DIR)

cat("Create new directories\n")

# Use this planning unit area
# Function to calculate diameter from area
area_in_km2 <- 1000
CellArea <- area_in_km2*(1000*1000) # in m is 1000km2
calculate_diameter <- function(CellArea){ # calculated in m
  height <- sqrt(2*CellArea/(sqrt(3))) # Height in m's
}

PU_HEIGHT <- calculate_diameter(CellArea)
# PU_HEIGHT_LARGE <- PU_HEIGHT*10 # removing this for HPC runs

cat("Calculated area\n")

#original values
#PU_HEIGHT = 27000
#PU_HEIGHT_LARGE = 270000

# Define projection
moll_proj <- "ESRI:54009"

# Load GMW data
gmw_data <- sf::st_read(file.path(INPUT_DIR, "gmw_v3_2020", "vector", "gmw_v3_2020_vec.shp"))

gmw <- gmw_data %>%
  st_transform(moll_proj) %>%
  st_make_valid()

# It takes too long to start with a global set of PUs.
# Lets try getting the PUs within the bbox of the GMW data
# then check the intersection of the coverage of the PUs for the actual GMW data.

bb <- gmw_data %>%
  st_bbox()
bb["ymin"] <- floor(bb["ymin"]) # Round the limits or they won't form a complete boundary
bb["ymax"] = ceiling(bb["ymax"])

bndry <- spatialplanr::splnr_get_boundary(bb, res = 1) # Get a boundary

# Get the PUs for the broader bounding box
PUs <- spatialgridr::get_grid(bndry, output = "sf_hex",
                              crs = moll_proj,
                              resolution = PU_HEIGHT) %>%
  sf::st_sf() %>%
  dplyr::mutate(cellID = dplyr::row_number())

# Get the larger PUs for the visualisation
# PUs_large <- spatialgridr::get_grid(bndry, output = "sf_hex",
#                               crs = moll_proj,
#                               resolution = PU_HEIGHT_LARGE) %>%
#   sf::st_sf() %>%
#   dplyr::mutate(cellID = dplyr::row_number())

gg <- ggplot() +
  geom_sf(data = PUs, linewidth = 0.00001)

ggsave(file.path(FIG_DIR, "00_bbox_PUs.pdf"), gg)

cat("Created planning units")

# Now we only want the ones that intersect with
overlap <- sf::st_intersects(PUs, gmw) %>%
  lengths() > 0

# overlap_large <- sf::st_intersects(PUs_large, gmw) %>%
#   lengths() > 0

PUs <- PUs[overlap,]
# PUs_large <- PUs_large[overlap_large,]

# Lets check it's working ok
gg <- ggplot() +
  geom_sf(data = gmw, linewidth = 0.0001, colour = "red", fill = NA) +
  geom_sf(data = PUs, linewidth = 0.0001, fill = NA, colour = "blue")

ggsave(file.path(FIG_DIR, "00_mangrove_PUs.pdf"), gg, width = 20, height = 5)

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

saveRDS(PUs, file = file.path(RESULTS_DIR, "00_PUs_mollweide.rds"))
st_write(PUs, file.path(RESULTS_DIR, "00_PUs_mollweide.gpkg"))

# saveRDS(PUs_large, file = file.path(RESULTS_DIR, "00_PUs_large_mollweide.rds"))

cat("Finished analysis")

# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
# gc() #free up memrory and report the memory usage.
# .rs.restartR()
