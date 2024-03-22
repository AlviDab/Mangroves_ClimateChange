#Author: Alvise Dabalà
#Date: 22/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, patchwork)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 0.3, by = 0.05),
           .options = furrr_options(seed = TRUE),
           function(prct) {

             CC_direction <- "mean"

             map(c("MEOW_and_biotyp", "biotyp"), function(split_group) {

               plot_overlap <- readRDS(paste0("Figures/01_map_differences/",
                                              split_group, "/RDS/overlap_",
                                              CC_direction, "_", prct, ".rds"))

               barplot_area <- readRDS(paste0("Figures/02_area/",
                                              split_group, "/RDS/barplot_area",
                                              CC_direction, "_", prct, ".rds"))

               kd_plot <- readRDS(paste0("Figures/03_CC_exposure/", split_group,
                                         "/RDS/kdplot_exposure",
                                         CC_direction, "_", prct, ".rds"))

               layout <- c(
                 area(t = 1, l = 1, b = 5, r = 6),
                 area(t = 6, l = 1, b = 7, r = 3),
                 area(t = 6, l = 4, b = 7, r = 6)
               )

               figure_01 <- (plot_overlap + kd_plot +
                               barplot_area) +
                 plot_layout(design = layout)

               dir.create(paste0("Figures/publication_figures/",
                                 split_group), recursive = TRUE)

               ggsave(plot = figure_01, paste0("Figures/publication_figures/",
                                               split_group,"/01_map_area_exposure_",
                                               CC_direction, "_", prct, ".pdf"),
                      dpi = 300, width = 18, height = 18, units = "cm")
             })
           })

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
