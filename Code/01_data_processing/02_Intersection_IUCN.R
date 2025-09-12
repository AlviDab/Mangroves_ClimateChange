#Author: Alvise Dabalà
#Date: 18/04/2023
#Description: Intersect the planning units with IUCN mangroves distribution to
#             include mangrove plant species

################################################################################

# Load libraries
pacman::p_load(tidyverse, sf)

# Set projection
moll_proj <- "ESRI:54009"

# Load planning units
PUs <- read_rds("Results/RDS/PUs_01_mangroves_biotyp_cc.rds")

tictoc::tic()

# Load IUCN mangroves distribution and intersect with planning units
IUCN_mangroves <- st_read("Data/IUCN_Distribution_Mangroves/MANGROVES.shp") %>%
  st_transform(moll_proj) %>%
  st_make_valid()

# Define negation of %in% operator
`%!in%` = Negate(`%in%`)

# Prepare IUCN species names
IUCN_mangroves <- IUCN_mangroves %>%
  dplyr::select(sci_name) %>%
  filter(sci_name %!in% c("Acanthus ilicifolius", "Cynometra iripa")) %>%
  mutate(sci_name = str_replace(sci_name, " ", "_")) %>%
  mutate(sci_name = paste0("Sp_", sci_name))

species_names <- IUCN_mangroves$sci_name

# Intersect PUs with IUCN mangroves
PUs_IUCN_index <- PUs %>%
  st_intersects(IUCN_mangroves, sparse = FALSE) %>%
  as_tibble()

tictoc::toc()

# Add species names to the columns
names(PUs_IUCN_index) <- species_names

# Count number of intersections and set to NA the species columns for PUs with no intersections
PUs_IUCN_index <- PUs_IUCN_index %>%
  mutate(n_intersections = rowSums(.)) %>%
  relocate(n_intersections, .before = 'Sp_Rhizophora_apiculata') %>%
  mutate(across(starts_with("Sp_"),
                ~case_when(n_intersections == 0 ~ NA,
                           .default = .)))

# Combine with PUs
PUs_IUCN <- PUs %>%
  cbind(PUs_IUCN_index) %>%
  dplyr::select("ID", "n_intersections",
                starts_with("Sp_"))

#Add species using nearest neighborhood for the planning units that don't intersect
source("Code/Functions/f_remove_zeros_nearestneighborhood_IUCN.R")

# Apply function and apply the area from the previous calculation
PUs_IUCN <- fNN_zeros_IUCN(PUs_IUCN, "n_intersections") %>%
  st_drop_geometry() %>%
  left_join(PUs, by = "ID") %>%
  mutate(across(starts_with("Sp_"),
                ~case_when(.x == "TRUE" ~ as.numeric(area_km2),
                           .default = 0))) %>%
  dplyr::select(!n_intersections) %>%
  st_as_sf()

# Save results
saveRDS(PUs_IUCN, "Results/RDS/PUs_02_mangroves_biotyp_cc_IUCN.rds")

# Clear environment and restart R session
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
