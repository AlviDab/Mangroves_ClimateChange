#Author: Alvise Dabalà
#Date: 28/05/2024

pacman::p_load(tidyverse, sf, MetBrewer)

source("Code/Functions/f_addcols_WDPA.r")

split_group <- "biotyp"
CC_direction <- "mean"
prct <- 0.3

solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                           split_group,"/solution_prioritisation.rds")) %>%
  f_addcols_WDPA()

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                              split_group, "/",
                              CC_direction, "/solution_",
                              as.character(prct), "_", CC_direction, ".rds")) %>%
  f_addcols_WDPA()

#% of the solution covered by PAs
tot_area_mangroves <- sum(solution_cc$MangroveArea_km2)

#selection covered by WDPA
sel_cov_WDPA <- solution_cc %>%
  st_drop_geometry() %>%
  select(ends_with("WDPA_all_km2"),
         ID) %>%
  reframe(WDPA_area_km2 = rowSums(pick(ends_with("WDPA_all_km2"))),
          ID = ID)

solution_cc <- solution_cc %>%
  left_join(sel_cov_WDPA)

#Percentage covered by PA
(sum(sel_cov_WDPA$WDPA_area_km2)/tot_area_mangroves)*100 #43.1%

#Percentage solution covered by PA
solution_cc %>%
  st_drop_geometry() %>%
  filter(solution_1 == 1) %>%
  reframe(WDPA_area_km2 = sum(WDPA_area_km2),
          MangroveArea_km2 = sum(MangroveArea_km2),
          prct_protected = (WDPA_area_km2/MangroveArea_km2)*100) #41.5

#Calculate area of each feature covered by PAs
PUs_features_split_targets <- readRDS(paste0("Results/RDS/PUs_05_features_split_targets_by_biotyp.rds")) %>%
  left_join(solution_cc %>%
              st_drop_geometry() %>%
              select(ends_with(c("Delta", "Estuary", "Lagoon", "OpenCoast"))) %>%
              summarise(across(everything(.), ~sum(.))) %>%
              pivot_longer(cols = everything(),
                           names_to = "feature",
                           values_to = "tot_area_km2"),
            by = "feature")

sp_area_protected <- map(c("Delta", "Estuary", "Lagoon", "OpenCoast"),
                         function(typology) {
                           solution_cc %>%
                             st_drop_geometry() %>%
                             select(ends_with(typology),
                                    paste0(typology, "_WDPA_all_km2")) %>%
                             mutate(across(ends_with(typology),
                                           ~case_when(. > 0 ~ !!sym(paste0(typology, "_WDPA_all_km2")),
                                                      .default = 0))) %>%
                             select(ends_with(typology))
                         }) %>%
  bind_cols(.name_repair = "unique") %>%
  summarise(across(everything(.), ~sum(.))) %>%
  pivot_longer(cols = everything(),
               names_to = "feature",
               values_to = "protected_area_km2")

PUs_features_split_targets_WDPA <- PUs_features_split_targets %>%
  left_join(sp_area_protected, by = "feature") %>%
  mutate(reached_target = case_when(tot_area_km2 * targets >= protected_area_km2 ~ FALSE,
                                    .default = TRUE),
         reached_30_target = case_when(tot_area_km2 * 0.3 >= protected_area_km2 ~ FALSE,
                                       .default = TRUE),
         shortfall_target = targets - protected_area_km2/tot_area_km2)

#Reached targets and percentage reached
sum(PUs_features_split_targets_WDPA$reached_target) #56
sum(PUs_features_split_targets_WDPA$reached_target)/nrow(PUs_features_split_targets_WDPA)*100 #21.7%

sum(PUs_features_split_targets_WDPA$reached_30_target) #102

#Mean shortfall
PUs_features_split_targets_WDPA %>%
  filter(reached_target == FALSE) %>%
  summarise(shortfall_target = mean(shortfall_target))

#Resilience
protected_cc <- solution_cc %>%
  st_drop_geometry() %>%
  summarise(landward_cc_protected_areas = weighted.mean(Prob_gain_stability_landward, WDPA_area_km2), #54.4
            seaward_cc_protected_areas = weighted.mean(Prob_gain_stability_seaward, WDPA_area_km2), #18.3
            mean_cc_protected_areas = weighted.mean(Prob_gain_stability_mean, WDPA_area_km2)
  )

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.