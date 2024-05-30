#Author: Alvise Dabalà
#Date: 28/05/2024

pacman::p_load(tidyverse, sf, patchwork, MetBrewer)

split_group <- "biotyp"
CC_direction <- "mean"
prct <- 0.3

plot_overlap_area <- readRDS(paste0("Figures/Country/10_overlap_WDPA/",
                                    split_group, "/RDS/overlap_WDPA_area_",
                                    CC_direction, "_", prct, ".rds")) +
  theme(legend.position = "none") +
  scale_fill_met_d("Johnson", override.order = TRUE)

plot_overlap_resilience <- readRDS(paste0("Figures/Country/10_overlap_WDPA/",
                               split_group, "/RDS/overlap_WDPA_resilience_",
                               CC_direction, "_", prct, ".rds")) +
  scale_colour_met_d("Johnson", override.order = TRUE) +
  scale_fill_met_d("Johnson", override.order = TRUE) +
  theme(legend.position = "bottom")

patchwork_plot <- plot_overlap_area / plot_overlap_resilience

ggsave(plot = patchwork_plot, paste0("Figures/Country/10_overlap_WDPA/",
              split_group, "/comparison_WDPA_area_resilience_",
              CC_direction, "_", prct, ".pdf"),
       width = 18, height = 18, dpi = 300, units = "cm")
