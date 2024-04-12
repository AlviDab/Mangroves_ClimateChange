#Author: Alvise Dabalà
#Date: 08/04/2024

pacman::p_load(tidyverse, sf, MoMAColors, purrr, furrr, parallel)

split_group <- "biotyp"
CC_direction <- "mean"
prct <- 0.3

source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_MEOW.r")

CC_direction <- "mean"
PUs_MEOW <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds")

map(c("MEOW_and_biotyp", "biotyp"), function(split_group) {

  solution <- readRDS(paste0("Results/RDS/prioritisation/01_prioritisation/",
                             split_group,"/solution_prioritisation.rds"))

  future_map(seq(0.05, 0.3, by = 0.05),
             .options = furrr_options(seed = TRUE),
             function(prct) {

               solution_cc <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                             split_group, "/",
                                             CC_direction, "/solution_",
                                             as.character(prct), "_", CC_direction, ".rds"))

               plot_layer <- solution_cc %>%
                 # f_int_MEOW(type = "PROVINCE") %>%
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
                        res_var = cc_exp_1 - cc_exp_0) %>%
                 f_int_continents()

               plot <- ggplot(data = plot_layer,
                      aes(x = res_var, y = perc_sel_area)) +
                 geom_point(aes(fill = continent),
                            size = 1, alpha = 0.8,
                            shape = 21) +
                 scale_fill_moma_d("Smith") +
                 geom_vline(xintercept = plot_layer$res_var, linetype = 2, linewidth = 0.5) + #SHOULD I HAVE A WEIGHTED MEAN HERE?
                 geom_vline(xintercept = 0, linewidth = 0.5) +
                 ylab("Percental area selected") +
                 xlab("Resilience variation") +
                 theme_bw() +
                 theme(legend.position = "top",
                       legend.title = element_blank(),
                       panel.grid.major = element_line(colour = "transparent"),
                       panel.background = element_blank(),
                       legend.key.size = unit(0.3, "cm"),
                       axis.text = element_text(size = 5),
                       axis.title = element_text(size = 6),
                       legend.text = element_text(size = 5)) +
                 guides(
                   fill = guide_legend(
                     nrow = 3)) +
                 xlim(-100, +100)

               dir.create(paste0("Figures/09_plot_percarea_CCratio/by_ecoregion/", split_group, "/RDS"), recursive = TRUE)

               ggsave(plot = plot, paste0("Figures/09_plot_percarea_CCratio/by_ecoregion/",
                                                  split_group,"/overlap_",
                                                  CC_direction, "_", prct, ".pdf"),
                      dpi = 300, width = 5, height = 5, units = "cm")

               saveRDS(plot, paste0("Figures/09_plot_percarea_CCratio/by_ecoregion/",
                                            split_group, "/RDS/overlap_",
                                            CC_direction, "_", prct, ".rds"))
             })
})

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()