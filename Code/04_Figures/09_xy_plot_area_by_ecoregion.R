#Author: Alvise Dabalà
#Date: 08/04/2024

pacman::p_load(tidyverse, sf, MoMAColors)

split_group <- "biotyp"
CC_direction <- "mean"
prct <- 0.3

source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_MEOW.r")

solution <- readRDS(paste0("Results/RDS/prioritisation/01_prioritisation/",
                           split_group,"/solution_prioritisation.rds"))

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                              split_group, "/",
                              CC_direction, "/solution_",
                              as.character(prct), "_", CC_direction, ".rds"))

PUs_MEOW <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds")

plot_layer <- solution_cc %>%
  left_join(PUs_MEOW %>%
              as_tibble() %>%
              dplyr::select(MEOW, ID), by = "ID") %>%
  group_by(MEOW, solution_1) %>%
  summarise(tot_area = sum(area_km2),
            cc_exp = weighted.mean(Prob_gain_stability_mean, area_km2)) %>%
  pivot_wider(names_from = "solution_1", values_from = c("tot_area", "cc_exp")) %>%
  group_by(MEOW) %>%
  summarise(across(ends_with(c("0", "1")), ~sum(., na.rm = TRUE))) %>%
  mutate(perc_sel_area = tot_area_1/(tot_area_1 + tot_area_0),
         vul_ratio = cc_exp_1/cc_exp_0) %>%
  f_int_continents() %>%
  f_int_MEOW(type = "PROVINCE")

# ecoregion_highlight <- plot_layer %>%
#   sample_n(20) %>%
#   dplyr::select(MEOW) %>%
#   st_drop_geometry()

# plot_layer <- plot_layer %>%
#   mutate(
#     label = case_when(MEOW %in% ecoregion_highlight$MEOW ~ MEOW,
#                       .default = ""))

ggplot(data = plot_layer,
       aes(x = vul_ratio, y = perc_sel_area)) +
  geom_point(aes(fill = continent),
             size = 3, alpha = 0.8,
             shape = 21) +
  scale_fill_moma_d("Smith") +
  geom_vline(xintercept = 1) +
  # ggrepel::geom_text_repel(
  #   aes(label = label),
  #   color = "black",
  #   size = 9/.pt,
  #   point.padding = 0.1,
  #   box.padding = 0.6,
  #   min.segment.length = 0,
  #   max.overlaps = 1000,
  #   seed = 7654
  # ) +
  ylab("Percentage mangrove area selected (%)") +
  xlab("Vulnerability ratio") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  guides(
    fill = guide_legend(
      nrow = 1))