#Author: Alvise Dabalà
#Date: 28/05/2024

pacman::p_load(tidyverse, sf, patchwork)

split_group <- "biotyp"
CC_direction <- "mean"
prct <- 0.3

plot_overlap_area <- readRDS(paste0("Figures/Country/10_overlap_WDPA/",
                                    split_group, "/RDS/overlap_WDPA_area_",
                                    CC_direction, "_", prct, ".rds"))

plot_overlap_resilience <- readRDS(paste0("Figures/Country/10_overlap_WDPA/",
                               split_group, "/RDS/overlap_WDPA_resilience_",
                               CC_direction, "_", prct, ".rds"))

plot_overlap_area + plot_overlap_resilience
