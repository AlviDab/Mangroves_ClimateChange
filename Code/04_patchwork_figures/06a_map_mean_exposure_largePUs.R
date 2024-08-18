#Author: Alvise Dabalà
#Date: 26/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, patchwork)

# ncores <- detectCores() - 2
#
# plan(multisession, workers = ncores)

prct <- 0.3
# future_map(seq(0.05, 0.3, by = 0.05),
#            .options = furrr_options(seed = TRUE),
#            function(prct) {

  plot_legend <- readRDS(paste0("Figures/Country/04b_map_large_comparison_area_kappa/biotyp/RDS/legend.rds"))

  plot_map_large_PUs_mean_global <- readRDS(paste0("Figures/Country/04b_map_large_comparison_area_kappa/biotyp/RDS/map_mean_",
                                          prct, ".rds"))

  kd_plot_mean_global <- readRDS(paste0("Figures/Country/03_CC_exposure/biotyp/RDS/kdplot_exposure_mean_",
                               prct, ".rds"))

  plot_map_large_PUs_mean_country <- readRDS(paste0("Figures/Country/04b_map_large_comparison_area_kappa/country_and_biotyp/RDS/map_mean_",
                                                   prct, ".rds"))

  kd_plot_mean_country <- readRDS(paste0("Figures/Country/03_CC_exposure/country_and_biotyp/RDS/kdplot_exposure_mean_",
                                        prct, ".rds"))


  layout <- c(
    area(t = 85, l = 50, b = 115, r = 60),
    area(t = 1, l = 1, b = 90, r = 60),
    area(t = 110, l = 1, b = 190, r = 60),
    area(t = 200, l = 1, b = 250, r = 33),
    area(t = 200, l = 34, b = 250, r = 60)
  )

  theme_txt <- theme(title = element_text(size = 11,
                                          face = 'bold'),
                     text = element_text(size = 9),
                     axis.text = element_text(size = 10),
                     legend.text = element_text(size = 8))

  figure_01 <- plot_legend +
    (plot_map_large_PUs_mean_global +
       ggtitle("Global scale") +
       theme_txt) +
    (plot_map_large_PUs_mean_country +
       ggtitle("Country scale") +
       theme_txt) +
    (kd_plot_mean_global +
       ggtitle("Global-scale resilience") +
       theme_txt) +
    (kd_plot_mean_country +
       ggtitle("Country-scale resilience") +
       theme_txt) +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = list(c('', 'a', 'b', 'c', 'd'))) &
    theme(plot.tag = element_text(face = 'bold'))

  dir.create(paste0("Figures/Country/06a_large_map_exposure_mean"), recursive = TRUE)

  ggsave(plot = figure_01, paste0("Figures/Country/06a_large_map_exposure_mean/map_largePUs_exposure_", prct, ".pdf"),
         dpi = 300, width = 18, height = 27, units = "cm")

  ggsave(plot = figure_01, paste0("Figures/Country/06a_large_map_exposure_mean/map_largePUs_exposure_", prct, ".png"),
         dpi = 300, width = 18, height = 27, units = "cm")
#})

# plan(sequential)
#
# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
# gc() #free up memrory and report the memory usage.
# .rs.restartR()
