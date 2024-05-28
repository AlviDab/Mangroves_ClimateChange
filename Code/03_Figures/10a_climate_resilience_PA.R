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

data <- solution_cc %>%
  st_drop_geometry() %>%
  group_by(solution_1) %>%
  summarise(w_mean_resilience_WDPA_all_protected = weighted.mean(Prob_gain_stability_mean,
                                                                 area_mangroves_WDPA_all_km2),
            w_mean_resilience_WDPA_all_unprotected = weighted.mean(Prob_gain_stability_mean,
                                                                   MangroveArea_km2 - area_mangroves_WDPA_all_km2),
            w_mean_resilience_WDPA_I_VI_protected = weighted.mean(Prob_gain_stability_mean,
                                                                  area_mangroves_WDPA_I_VI_km2),
            w_mean_resilience_WDPA_I_VI_unprotected = weighted.mean(Prob_gain_stability_mean,
                                                                    MangroveArea_km2 - area_mangroves_WDPA_I_VI_km2),
            w_mean_resilience_WDPA_I_IV_protected = weighted.mean(Prob_gain_stability_mean,
                                                                  area_mangroves_WDPA_I_IV_km2),
            w_mean_resilience_WDPA_I_IV_unprotected = weighted.mean(Prob_gain_stability_mean,
                                                                    MangroveArea_km2 - area_mangroves_WDPA_I_IV_km2))

plot_data <- data %>%
  rename_with(~ str_remove(., "w_mean_resilience_"), everything()) %>%
  pivot_longer(!solution_1, names_to = "protected_areas_category", values_to = "w_mean_resilience") %>%
  mutate(protection = str_extract(protected_areas_category, "[^_]*$"),
         solution_1 = case_when(solution_1 == 1 ~ "Selected",
                                .default = "Not selected"),
         protected_areas_category = str_remove(protected_areas_category, "_[^_]*$"),
         protected_areas_category = case_when(protected_areas_category == "WDPA_all" ~ "All PAs",
                                              protected_areas_category == "WDPA_I_VI" ~ "Category I-VI",
                                              .default = "Category I-IV"))

plot_overlap <- ggplot(data = plot_data) +
  geom_col(aes(x = protected_areas_category,
               y = w_mean_resilience,
               fill = as.factor(solution_1)),
           position = "dodge") +
  scale_fill_manual(values = c('#ffba49', '#20a39e')) +
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
  ylab("Weighted mean resilience") +
  xlab(expression("")) +
  facet_grid(vars(protection)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110))

dir.create(paste0("Figures/Country/10_overlap_WDPA/",
                  split_group, "/RDS/"), recursive = TRUE)

saveRDS(plot_overlap, paste0("Figures/Country/10_overlap_WDPA/",
                             split_group, "/RDS/overlap_WDPA_resilience_",
                             CC_direction, "_", prct, ".rds"))


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()