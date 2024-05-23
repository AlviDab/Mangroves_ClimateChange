#Author: Alvise Dabalà
#Date: 08/04/2024

pacman::p_load(tidyverse, sf, MoMAColors, purrr, furrr, parallel)

split_group <- "biotyp"

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

solution_cc <- future_map(seq(0.05, 0.3, by = 0.05),
                          .options = furrr_options(seed = TRUE),
                          function(prct) {

                            source("Code/Functions/f_intersect_MEOW.r")
                            source("Code/Functions/f_intersect_continents.r")
                            source("Code/Functions/f_intersect_countries.r")

                            PUs_MEOW <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds")


                            solution_cc_sw <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                                             split_group, "/seaward/solution_",
                                                             as.character(prct), "_seaward.rds")) %>%
                              mutate(type = "seaward")

                            solution_cc_lw <- readRDS(paste0("Results/RDS/prioritisation/02_prioritisation_CC/",
                                                             split_group, "/landward/solution_",
                                                             as.character(prct), "_landward.rds")) %>%
                              mutate(type = "landward")

                            solution_cc <- solution_cc_sw %>%
                              rbind(solution_cc_lw) %>%
                              f_int_countries() %>%
                              f_int_MEOW(type = "PROVINCE") %>%
                              group_by(PROVINCE, solution_1, type) %>%
                              summarise(tot_area = sum(area_km2),
                                        cc_exp_sw = weighted.mean(Prob_gain_stability_seaward, area_km2),
                                        cc_exp_lw = weighted.mean(Prob_gain_stability_landward, area_km2)) %>%
                              pivot_wider(names_from = "solution_1", values_from = c("tot_area",
                                                                                     "cc_exp_sw",
                                                                                     "cc_exp_lw")) %>%
                              group_by(PROVINCE, type) %>%
                              summarise(across(ends_with(c("0", "1")), ~sum(., na.rm = TRUE))) %>%
                              mutate(perc_sel_area = tot_area_1/(tot_area_1 + tot_area_0)) %>%
                              ungroup() %>%
                              pivot_wider(names_from = "type", values_from = "perc_sel_area") %>%
                              group_by(PROVINCE) %>%
                              summarise(seaward = sum(seaward, na.rm = TRUE),
                                        landward = sum(landward, na.rm = TRUE),
                                        perc_difference = seaward-landward) %>%
                              f_int_continents() %>%
                              mutate(continent = case_when(grepl("America", continent) ~ "America",
                                                           .default = continent)) %>%
                              mutate(prct = prct)
                          }) %>%
  bind_rows()

plan(sequential)

#Solve the problem with French Guiana in Europe

## CHECK IF THERE ARE ANY OTHER FRENCH
solution_cc_long <- solution_cc %>%
  pivot_longer(c(seaward, landward)) #%>%
  # mutate(country = case_when(country == "France" ~ "French Guiana",
  #                            .default = country),
  #        continent = case_when(continent == "Europe" ~ "America",
  #                              .default = continent))

plot_sw_lw <- ggplot() +
  geom_col(data = solution_cc_long, aes(y = value,
                                        x = PROVINCE,
                                        fill = name),
           position = "dodge",
           width = 0.5) +
  facet_grid(prct ~ continent, scales = 'free', space = "free_x") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.box = 'vertical') +
  ylab("Percentage of mangrove area selected") +
  xlab("") +
  scale_fill_manual(values = c("#f18f01",
                               "#006e90"),
                    labels = c("landward",
                               "seaward")) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylim(c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1))

dir.create(paste0("Figures/10_plot_comparison_area_lw_sw/",
                  split_group, "/RDS"), recursive = TRUE)

ggsave(paste0("Figures/10_plot_comparison_area_lw_sw/",
              split_group, "/plot_comparison_area_lw_sw_",
              split_group, "_province.pdf"),
       dpi = 300, width = 18, height = 25, units = "cm")

saveRDS(plot_sw_lw, paste0("Figures/10_plot_comparison_area_lw_sw/",
                           split_group, "/RDS/plot_comparison_area_lw_sw_",
                     split_group, "_province.rds"))
