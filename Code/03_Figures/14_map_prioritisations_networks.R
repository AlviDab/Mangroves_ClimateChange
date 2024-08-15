#Author: Alvise Dabalà
#Date: 14/08/2024

pacman::p_load(tidyverse, sf, patchwork)

nocc_global <- readRDS(paste0("Figures/Country/04_map_large/biotyp/RDS/map_noCC_0.rds")) +
  labs(tag = "a") +
  ggtitle("Global-scale climate-naïve")

cc_global <- readRDS(paste0("Figures/Country/04_map_large/biotyp/RDS/map_mean_0.3.rds")) +
  labs(tag = "b") +
  ggtitle("Global-scale climate-smart")

nocc_country <- readRDS(paste0("Figures/Country/04_map_large/country_and_biotyp/RDS/map_noCC_0.rds")) +
  labs(tag = "c") +
  ggtitle("Country-scale climate-naïve")

cc_country <- readRDS(paste0("Figures/Country/04_map_large/country_and_biotyp/RDS/map_mean_0.3.rds")) +
  labs(tag = "d") +
  ggtitle("Country-scale climate-smart")

design <- c(
"1
2
3
4"
)

plot_patchwork <- list(nocc_global, cc_global,
     nocc_country, cc_country) %>%
  wrap_plots() +
  plot_layout(guides = 'collect', design = design) &
  theme(legend.position = 'bottom',
        plot.tag = element_text(face = 'bold'),
        title = element_text(size = 10,
                             face = 'bold'),
        plot.title = element_text(hjust = 0.5))

dir.create("Figures/Country/14_map_prioritisations_networks/", recursive = TRUE)

ggsave(paste0("Figures/Country/14_map_prioritisations_networks/patchwork_maps_0.3_horizontal.pdf"),
       dpi = 300, width = 18, height = 13, units = "cm")

ggsave(paste0("Figures/Country/14_map_prioritisations_networks/patchwork_maps_0.3_horizontal.jpg"),
       dpi = 1000, width = 18, height = 13, units = "cm")
