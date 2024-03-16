#Author: Alvise Dabalà
#Date: 21/02/2024

pacman::p_load(tidyverse, sf, prioritizr)

PUs_IUCN <- readRDS("Results/RDS/PUs_02_mangroves_biotyp_cc_IUCN.rds")

PUs <- readRDS("Results/RDS/PUs_04_mangroves_cc_IUCN_split_by_MEOW_and_biotyp.rds")

PUs_features <- PUs_IUCN %>%
  select(starts_with("Sp_")) %>%
  st_drop_geometry() %>%
  summarise(across(everything(.), sum)) %>%
  pivot_longer(names_to = "feature", values_to = "AOH", cols = everything(.))

#Calculate species targets following Rodrigues et al. 2014 and Butchart et al. 2015
spp_range_size_km2 <- seq(0.01, max(PUs_features$AOH), by = 100)

spp_target_percentage_rodrigues <-
  loglinear_interpolation(
    x = spp_range_size_km2,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 250000,
    coordinate_two_y = 0.1) * 100

plot(spp_target_percentage_rodrigues ~ spp_range_size_km2)

spp_target_percentage_butchart <- ifelse(
  spp_range_size_km2 >= 10000000,
  (1000000 / spp_range_size_km2) * 100,
  spp_target_percentage_rodrigues)

spp_target_percentage_butchart

# Select the target for each feature
targets_species <- map_dbl(PUs_features$AOH, function(species_AOH) {
  ID_range <- spp_range_size_km2 %>%
    as_tibble() %>%
    mutate(ID = rownames(.)) %>%
    slice(which.min(abs(value - species_AOH))) %>% #Select the value immediately smaller than the exact range of distribution
    dplyr::select(ID) %>%
    as.numeric()

  spp_target_percentage_butchart[[ID_range[[1]]]]/100
})

PUs_features_targets <- PUs_features %>%
  mutate(targets = targets_species)

PUs_features_split <- PUs %>%
  select(starts_with("Sp_")) %>%
  st_drop_geometry() %>%
  summarise(across(everything(.), sum)) %>%
  pivot_longer(names_to = "feature",
               values_to = "AOH",
               cols = everything(.))

#Function to assign the targets to subspecies
split_targets <- function(species) {
  species_target <- PUs_features_targets %>%
    filter(feature == species) %>%
    dplyr::select(targets) %>%
    as.numeric()

  PUs_features_split %>%
    filter(grepl(species, feature)) %>%
    mutate(targets = species_target)
}

PUs_features_split_targets <- map(PUs_features$feature, split_targets) %>%
  bind_rows()

saveRDS(PUs_features_targets, "Results/RDS/PUs_05_features_targets.rds")
saveRDS(PUs_features_split_targets, "Results/RDS/PUs_05_features_split_targets.rds")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()