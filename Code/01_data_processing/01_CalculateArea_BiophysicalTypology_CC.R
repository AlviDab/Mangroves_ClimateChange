#Author: Alvise Dabalà
#Date: 18/04/2023
#Description: Code to calculate the area of mangroves in each biophysical typology
#             and the area-weighted average of the climate change predictions

################################################################################

# Load libraries
pacman::p_load(sf, terra, tidyverse)

# Set projection
moll_proj <- "ESRI:54009"

# Load the planning units
PUs <- readRDS("Results/RDS/PUs_00_mollweide.rds")

# Load the climate change predictions
predictions_CC <- read.csv("Data/Predictions_Buelow/mangrove-forecasts.csv") %>%
  as_tibble() %>%
  dplyr::select("ID", "Prob_gain_stability_landward", "Prob_gain_stability_seaward")

# Process the mangrove biophysical typology shapefile
mangroves_biotyp <- st_read("Data/MangroveTypology/Mangrove_Typology_v3_2020.shp") %>%
st_transform(moll_proj) %>%
st_make_valid()

# # Save a copy as an rds file so we don't have to keep reading the shapefile
dir.create("Results/RDS/", recursive = TRUE)
# saveRDS(mangroves_biotyp,
#         "Results/RDS/mangroves_distribution_mollweide.rds")

# mangroves_biotyp <- readRDS("Results/RDS/mangroves_distribution_mollweide.rds")

# Join the climate change predictions to the mangrove biophysical typology
mangroves_biotyp_cc <- mangroves_biotyp %>%
  left_join(predictions_CC, by = "ID")

# Intersect the planning units with the mangrove biophysical typology
PUs <- PUs %>%
  rowid_to_column("ID")

PUs_mangroves_biotyp_cc_intersection <- PUs %>%
  st_intersection(mangroves_biotyp_cc %>%
                    dplyr::select("Class", "Prob_gain_stability_landward",
                                  "Prob_gain_stability_seaward"))

# Calculate the area of each biophysical typology in each planning unit
PUs_mangroves_biotyp_cc_intersection <- PUs_mangroves_biotyp_cc_intersection %>%
  mutate(area_km2 = as.numeric(units::set_units(sf::st_area(.), "km2")) %>%
           as.numeric()) %>%
  pivot_wider(names_from = "Class", values_from = "area_km2")

# Obtain the area-weighted average of the climate change predictions and the area
# of each biophysical typology in each planning unit
PUs_mangroves_biotyp_cc_intersection <- PUs_mangroves_biotyp_cc_intersection %>%
  rowwise() %>%
  mutate(area_km2 = sum(c_across(Delta:OpenCoast), na.rm = T)) %>%
  group_by(ID) %>%
  summarise(Delta = sum(Delta, na.rm = T),
            Estuary = sum(Estuary , na.rm = T),
            Lagoon = sum(Lagoon , na.rm = T),
            OpenCoast = sum(OpenCoast, na.rm = T),
            Prob_gain_stability_landward = weighted.mean(Prob_gain_stability_landward,
                                                         as.numeric(area_km2),
                                                         na.rm = T), #mean of the probability weighted by the area
            Prob_gain_stability_seaward = weighted.mean(Prob_gain_stability_seaward,
                                                        as.numeric(area_km2),
                                                        na.rm = T),
            area_km2 = sum(area_km2, na.rm = T))

# Save the results
dir.create("Results/RDS/", recursive = TRUE)
saveRDS(PUs_mangroves_biotyp_cc_intersection,
        "Results/RDS/PUs_01_mangroves_biotyp_cc_intersection.rds")

# Join the results back to the planning units
PUs_mangroves_biotyp_cc <- PUs %>%
  left_join(
    (PUs_mangroves_biotyp_cc_intersection %>%
       st_drop_geometry() %>%
       as_tibble()
    ), by = "ID")

# Save the results
saveRDS(PUs_mangroves_biotyp_cc,
        "Results/RDS/PUs_01_mangroves_biotyp_cc.rds")

st_write(PUs_mangroves_biotyp_cc,
         "Results/gpkg/PUs_01_mangroves_biotyp_cc.gpkg",
         append = FALSE)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
