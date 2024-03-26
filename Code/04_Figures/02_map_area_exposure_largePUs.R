#Author: Alvise Dabalà
#Date: 26/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, patchwork)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 0.3, by = 0.05),
           .options = furrr_options(seed = TRUE),
           function(prct) {

             CC_direction <- "mean"

             map(c("MEOW_and_biotyp", "biotyp"), function(split_group) {

               plot_map_large_PUs <- readRDS(paste0("Figures/04_map_large/",
                                                    split_group, "/RDS/map_noCC_0.rds"))

               plot_map_large_PUs_CC <- readRDS(paste0("Figures/04_map_large/",
                                                       split_group,"/RDS/map_",
                                                       CC_direction, "_", prct, ".rds"))

               barplot_area <- readRDS(paste0("Figures/02_area/",
                                              split_group, "/RDS/barplot_area",
                                              CC_direction, "_", prct, ".rds"))

               kd_plot <- readRDS(paste0("Figures/03_CC_exposure/", split_group,
                                         "/RDS/kdplot_exposure",
                                         CC_direction, "_", prct, ".rds"))

               layout <- c(
                 area(t = 1, l = 1, b = 5, r = 6),
                 area(t = 6, l = 1, b = 10, r = 6),
                 area(t = 11, l = 1, b = 12, r = 3),
                 area(t = 11, l = 4, b = 12, r = 6)
               )

               figure_01 <- ((plot_map_large_PUs +
                                theme(legend.position = "top",
                                      title = element_text(size = 8,
                                                           face = 'bold')) +
                                ggtitle("Climate-naïve prioritisation") +
                                scale_fill_viridis_c(option = "B")) +
                               (plot_map_large_PUs_CC +
                                  theme(legend.position = "top",
                                        title = element_text(size = 8,
                                                             face = 'bold')) +
                                ggtitle("Climate-smart prioritisation")) +
                               kd_plot + barplot_area) +
                 plot_layout(design = layout) +
                 plot_annotation(tag_levels = 'a') &
                 theme(plot.tag = element_text(face = 'bold'))

               dir.create(paste0("Figures/publication_figures/",
                                 split_group), recursive = TRUE)

               ggsave(plot = figure_01, paste0("Figures/publication_figures/",
                                               split_group,"/02_map_largePUs_area_exposure_",
                                               CC_direction, "_", prct, ".pdf"),
                      dpi = 300, width = 18, height = 24, units = "cm")
             })
           })

plan(sequential)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()
