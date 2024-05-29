#Author: Alvise Dabalà
#Date: 28/05/2024

pacman::p_load(tidyverse, sf)

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

#Repeat the value for each km² of mangroves selected (we included all the observation > 0.1 km²)
resilience_priorities <- solution_cc %>%
  st_drop_geometry() %>%
  filter(solution_1 == 1) %>%
  dplyr::select(ID, MangroveArea_km2, Prob_gain_stability_mean) %>%
  mutate(weighted_mean_exposure = weighted.mean(Prob_gain_stability_mean,
                                                MangroveArea_km2)) %>%
  mutate(category = "Selected_priorities")

resilience_WDPA_all <- solution_cc %>%
  st_drop_geometry() %>%
  dplyr::select(ID, area_mangroves_WDPA_all_km2, Prob_gain_stability_mean) %>%
  rename(MangroveArea_km2 = area_mangroves_WDPA_all_km2) %>%
  mutate(weighted_mean_exposure = weighted.mean(Prob_gain_stability_mean,
                                                MangroveArea_km2)) %>%
  mutate(category = "WDPA_all")

resilience_WDPA_I_VI <- solution_cc %>%
  st_drop_geometry() %>%
  dplyr::select(ID, area_mangroves_WDPA_I_VI_km2, Prob_gain_stability_mean) %>%
  rename(MangroveArea_km2 = area_mangroves_WDPA_I_VI_km2) %>%
  mutate(weighted_mean_exposure = weighted.mean(Prob_gain_stability_mean,
                                                MangroveArea_km2)) %>%
  mutate(category = "WDPA_I_VI")

resilience_WDPA_I_IV <- solution_cc %>%
  st_drop_geometry() %>%
  dplyr::select(ID, area_mangroves_WDPA_I_IV_km2, Prob_gain_stability_mean) %>%
  rename(MangroveArea_km2 = area_mangroves_WDPA_I_IV_km2) %>%
  mutate(weighted_mean_exposure = weighted.mean(Prob_gain_stability_mean,
                                                MangroveArea_km2)) %>%
  mutate(category = "WDPA_I_IV")

plot_data <- resilience_priorities %>%
  rbind(resilience_WDPA_all) %>%
  rbind(resilience_WDPA_I_VI) %>%
  rbind(resilience_WDPA_I_IV) %>%
  mutate(category = case_when(category == "WDPA_all" ~ "All PAs",
                              category == "WDPA_I_VI" ~ "Category I-VI",
                              category == "WDPA_I_IV" ~ "Category I-IV",
                              .default = "Selected priorities"))

plot_density <- ggplot(data = plot_data) +
  geom_density(aes(colour = category,
                   fill = category,
                   x = Prob_gain_stability_mean,
                   weight = MangroveArea_km2),
               alpha = 0.2) +
  geom_vline(aes(xintercept = weighted_mean_exposure,
                 colour = category)) +
  #scale_fill_manual(values = c('#ffba49', '#20a39e')) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.box = 'vertical') +
  ylab("Area weighted resilience") +
  xlab(expression(""))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.05)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100))

dir.create(paste0("Figures/Country/10_overlap_WDPA/",
                  split_group, "/RDS/"), recursive = TRUE)

saveRDS(plot_density, paste0("Figures/Country/10_overlap_WDPA/",
                             split_group, "/RDS/overlap_WDPA_resilience_",
                             CC_direction, "_", prct, ".rds"))


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()