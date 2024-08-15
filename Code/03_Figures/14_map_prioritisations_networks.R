#Author: Alvise Dabalà
#Date: 14/08/2024

pacman::p_load(tidyverse, sf, patchwork)

nocc_global <- readRDS(paste0("Figures/Country/04_map_large/biotyp/RDS/map_noCC_0.rds")) +
  labs(tag = "a")

cc_global <- readRDS(paste0("Figures/Country/04_map_large/biotyp/RDS/map_mean_0.3.rds")) +
  labs(tag = "b")

nocc_country <- readRDS(paste0("Figures/Country/04_map_large/country_and_biotyp/RDS/map_noCC_0.rds")) +
  labs(tag = "c")

cc_country <- readRDS(paste0("Figures/Country/04_map_large/country_and_biotyp/RDS/map_mean_0.3.rds")) +
  labs(tag = "d")

design <- c(
  patchwork::area(t = 1, l = 11, b = 10, r = 600),
  patchwork::area(t = 1, l = 601, b = 10, r = 1190),
  patchwork::area(t = 11, l = 1, b = 300, r = 10),
  patchwork::area(t = 301, l = 1, b = 600, r = 10),
  patchwork::area(t = 11, l = 11, b = 300, r = 600),
  patchwork::area(t = 11, l = 601, b = 300, r = 1190),
  patchwork::area(t = 301, l = 11, b = 600, r = 600),
  patchwork::area(t = 301, l = 601, b = 600, r = 1190)
)

plot_patchwork <- list(ggpubr::text_grob("Climate-naïve", face = "bold"),
     ggpubr::text_grob("Climate-smart", face = "bold"),
     ggpubr::text_grob("Global-scale", rot = 90, face = "bold"),
     ggpubr::text_grob("Country-scale", rot = 90, face = "bold"),
     nocc_global, cc_global,
     nocc_country, cc_country) %>%
  wrap_plots() +
  plot_layout(guides = 'collect', design = design) &
  theme(legend.position = 'bottom',
        plot.tag = element_text(face = 'bold'),
        title = element_text(size = 10))

dir.create("Figures/Country/14_map_prioritisations_networks/", recursive = TRUE)

ggsave(paste0("Figures/Country/14_map_prioritisations_networks/patchwork_maps_0.3_horizontal.pdf"),
       dpi = 300, width = 18, height = 13, units = "cm")

ggsave(paste0("Figures/Country/14_map_prioritisations_networks/patchwork_maps_0.3_horizontal.jpg"),
       dpi = 1000, width = 18, height = 13, units = "cm")
