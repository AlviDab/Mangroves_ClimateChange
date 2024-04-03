#Author: Alvise Dabalà
#Date: 03/04/2024

pacman::p_load(tidyverse, sf, furrr)

PUs <- readRDS("Results/RDS/prioritisation/02_prioritisation_CC/MEOW_and_biotyp/mean/solution_0.05_mean.rds")

dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

PUs_bin_feat <- PUs %>%
  mutate(across(starts_with("Sp_"),
                ~case_when(. > 0 ~ 1,
                           .default = 0))) %>%
  dplyr::select(starts_with("Sp_")) %>%
  mutate(n_features = rowSums(across(!geometry)))

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()

plot_map <- ggplot() +
  geom_sf(data = world_map, fill = "grey60",
          colour = "grey60",
          linewidth = 0.001) +
  geom_sf(data = PUs_bin_feat,
          aes(fill = n_features,
              colour = n_features),
          linewidth = 0.01) +
  scale_colour_viridis_c(option = "A", guide = "none") +
  scale_fill_viridis_c(option = "A") +
  guides(fill = guide_colourbar(barwidth = 10,
                                barheight = 0.5,
                                title.position = "top",
                                title = expression("Number of features"),
                                ticks.colour = "black")) +
  geom_sf(data = dat, fill = NA) +
  theme_minimal(base_size = 7) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_blank(),
        legend.position = "top",
        legend.box = "vertical",
        legend.key.size = unit(0.3, "cm")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(datum = NA)

dir.create(paste0("Figures/07_PUs_features/RDS"), recursive = TRUE)

ggsave(plot = plot_map, paste0("Figures/07_PUs_features/PUs_features.pdf"),
       dpi = 300, width = 18, height = 11, units = "cm")

saveRDS(plot_map, paste0("Figures/07_PUs_features/RDS/PUs_features.rds"))
