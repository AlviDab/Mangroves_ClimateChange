#Author: Alvise Dabalà
#Date: 24/06/2024

pacman::p_load(tidyverse, sf, purrr, furrr, parallel, openxlsx)

prct <- 0.3
split_group <- "biotyp"

source("Code/Functions/f_create_worldmap.r")
world_map <- f_worldmap()
source("Code/Functions/f_intersect_countries.R")

# map(seq(0.05, 0.3, by = 0.05),
#     function(prct) {

solution_cc_sw <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                 split_group, "/seaward/solution_",
                                 as.character(prct), "_seaward.rds")) %>%
  mutate(type = "seaward") %>%
  f_int_countries()

solution_cc_lw <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                 split_group, "/landward/solution_",
                                 as.character(prct), "_landward.rds")) %>%
  mutate(type = "landward") %>%
  f_int_countries()

#Resilience value
solution_cc_sw %>%
  st_drop_geometry() %>%
  filter(solution_1 == TRUE) %>%
  summarise(lw_w_mean_resilience = weighted.mean(Prob_gain_stability_landward, MangroveArea_km2),
            sw_w_mean_resilience = weighted.mean(Prob_gain_stability_seaward, MangroveArea_km2))

solution_cc_lw %>%
  st_drop_geometry() %>%
  filter(solution_1 == TRUE) %>%
  summarise(lw_w_mean_resilience = weighted.mean(Prob_gain_stability_landward, MangroveArea_km2),
            sw_w_mean_resilience = weighted.mean(Prob_gain_stability_seaward, MangroveArea_km2))

#Kappa by country
kappa_by_country <- map(unique(solution_cc_lw$country), function(sel_country) {
  kappa <- spatialplanr::splnr_get_kappaCorrData(list(solution_cc_lw %>%
                                                        filter(country == sel_country),
                                                      solution_cc_sw %>%
                                                        filter(country == sel_country)),
                                                 c("landward", "seaward"))[[2]]
})%>%
  unlist() %>%
  tibble(country = unique(solution_cc_lw$country), kappa = .)

solution_cc_separate <- solution_cc_sw %>%
  rbind(solution_cc_lw) %>%
  group_by(country, solution_1, type) %>%
  summarise(tot_area = sum(area_km2),
            w_mean_cc_exp_sw = weighted.mean(Prob_gain_stability_seaward, area_km2),
            w_mean_cc_exp_lw = weighted.mean(Prob_gain_stability_landward, area_km2)) %>%
  pivot_wider(names_from = "solution_1", values_from = c("tot_area",
                                                         "w_mean_cc_exp_sw",
                                                         "w_mean_cc_exp_lw")) %>%
  group_by(country, type) %>%
  summarise(across(ends_with(c("0", "1")), ~sum(., na.rm = TRUE))) %>%
  mutate(perc_sel_area = tot_area_1/(tot_area_1 + tot_area_0)) %>%
  ungroup()

solution_cc <- solution_cc_separate %>% #Maybe add also the exposure to climate change
  pivot_wider(names_from = type, values_from = perc_sel_area) %>%
  dplyr::select(country, landward, seaward) %>%
  left_join(kappa_by_country, by = "country") %>%
  group_by(country) %>%
  summarise(country = first(country),
            landward = sum(landward, na.rm = TRUE),
            seaward = sum(seaward, na.rm = TRUE),
            kappa = first(kappa)) %>%
  mutate(ratio_lw_sw = landward-seaward,
         size_dots = abs(ratio_lw_sw)) %>%
  st_centroid() %>%
  mutate(kappa_def = case_when(
    kappa == 1 ~ "Perfect",
    kappa == 0.2 ~ "None to slight", # to include 0.2
    dplyr::between(kappa, 0.8, 1) ~ "Almost perfect",
    dplyr::between(kappa, 0.6, 0.8) ~ "Substantial",
    dplyr::between(kappa, 0.4, 0.6) ~ "Moderate",
    dplyr::between(kappa, 0.2, 0.4) ~ "Fair",
    .default = "None to slight")) %>%
  mutate(size_dots = ifelse(is.na(kappa), NA, size_dots))

dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

plot_map <- ggplot() +
  geom_sf(data = world_map, fill = "grey60",
          colour = "grey60",
          linewidth = 0.001) +
  geom_sf(data = solution_cc, aes(colour = kappa_def, size = size_dots), alpha = 0.8) +
  scale_colour_viridis_d(option = "D", direction = -1,
                         name = "Agreement",
                         limits = c("None to slight",
                                    "Fair",
                                    "Moderate",
                                    "Substantial",
                                    "Almost perfect",
                                    "Perfect")) +
  geom_sf(data = dat, fill = NA) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_blank(),
        legend.position = "top",
        legend.box = "vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_alpha_continuous(range = c(0.8, 1)) +
  coord_sf(datum = NA)

dir.create(paste0("Figures/Country/09_map_kappa_ratio_lw_sw/", split_group, "/RDS"), recursive = TRUE)

ggsave(paste0("Figures/Country/09_map_kappa_ratio_lw_sw/",
              split_group,"/map_ratio_lw_sw_", prct, ".pdf"),
       dpi = 300, width = 18, height = 11, units = "cm")

write.xlsx(solution_cc %>%
             st_drop_geometry(), append = TRUE,
           file = paste0("Figures/Country/09_map_kappa_ratio_lw_sw/",
                         split_group,"/map_kappa_ratio_lw_sw_", prct, ".xlsx"))

saveRDS(plot_map, paste0("Figures/Country/09_map_kappa_ratio_lw_sw/",
                         split_group,"/RDS/map_ratio_lw_sw_", prct,  ".rds"))
# })

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
