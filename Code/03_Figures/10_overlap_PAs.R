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

#% of the solution covered by PAs
tot_area_mangroves <- sum(solution_cc$MangroveArea_km2)

data <- solution_cc %>%
  st_drop_geometry() %>%
  group_by(solution_1) %>%
  summarise(tot_mangroveArea_km2 = sum(MangroveArea_km2),
            area_mangroves_WDPA_all_km2 = sum(area_mangroves_WDPA_all_km2),
            prct_area_mangroves_WDPA_all = sum(area_mangroves_WDPA_all_km2)/tot_mangroveArea_km2,
            area_mangroves_WDPA_I_VI_km2 = sum(area_mangroves_WDPA_I_VI_km2),
            prct_area_mangroves_WDPA_I_VI = sum(area_mangroves_WDPA_I_VI_km2)/tot_mangroveArea_km2,
            area_mangroves_WDPA_I_IV_km2 = sum(area_mangroves_WDPA_I_IV_km2),
            prct_area_mangroves_WDPA_I_IV = sum(area_mangroves_WDPA_I_IV_km2)/tot_mangroveArea_km2)

plot_data <- data %>%
  dplyr::select(contains(c("prct", "solution_1"))) %>%
  rename_with(~ str_remove(., "prct_area_mangroves_"), everything()) %>%
  pivot_longer(!solution_1, names_to = "protected_areas_category", values_to = "percentage_mangroves_protected") %>%
  mutate(solution_1 = case_when(solution_1 == 1 ~ "Selected",
                                .default = "Not selected"),
         protected_areas_category = case_when(protected_areas_category == "WDPA_all" ~ "All PAs",
                                              protected_areas_category == "WDPA_I_VI" ~ "Category I-VI",
                                              .default = "Category I-IV"))


plot_overlap <- ggplot() +
  geom_col(data = plot_data, aes(x = protected_areas_category,
                                 y = percentage_mangroves_protected*100,
                                 fill = as.factor(solution_1)),
           position = "dodge") +
  scale_fill_manual(values = c('#ffba49', '#20a39e')) +
  # geom_hline(aes(yintercept = sum(solution_cc$area_mangroves_WDPA_all_km2)/tot_area_mangroves*100), linetype = 2) +
  # geom_hline(aes(yintercept = sum(solution_cc$area_mangroves_WDPA_I_VI_km2)/tot_area_mangroves*100), linetype = 2) +
  # geom_hline(aes(yintercept = sum(solution_cc$area_mangroves_WDPA_I_IV_km2)/tot_area_mangroves*100), linetype = 2) +
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
  ylab("Protected areas coverage (%)") +
  xlab(expression("")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110))

dir.create(paste0("Figures/Country/10_overlap_WDPA/",
                  split_group, "/RDS/"), recursive = TRUE)

saveRDS(plot_overlap, paste0("Figures/Country/10_overlap_WDPA/",
                             split_group, "/RDS/overlap_WDPA_area_",
                             CC_direction, "_", prct, ".rds"))

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()