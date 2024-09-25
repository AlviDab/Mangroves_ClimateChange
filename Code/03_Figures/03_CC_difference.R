#Author: Alvise Dabalà
#Date: 18/03/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, openxlsx)

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 0.3, by = 0.05),
           function(prct) {

             map(c("landward", "seaward",
                   "mean"), function(CC_direction) {

                     map(c("country_and_biotyp", "biotyp"), function(split_group) {

                       solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                                                  split_group,"/solution_prioritisation.rds"))

                       solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                     split_group, "/",
                                                     CC_direction, "/solution_",
                                                     as.character(prct), "_", CC_direction, ".rds"))

                       col_name <- paste0("Prob_gain_stability_", CC_direction)

                       #mean climate risk climate-naïve
                       selected_cn <- solution %>%
                         st_drop_geometry() %>%
                         filter(solution_1 == 1) %>%
                         mutate(type = "Climate-naïve") %>%
                         mutate(weighted_mean_exposure = weighted.mean(!!sym(col_name),
                                                                       area_km2)) %>%
                         as_tibble()

                       #Add values from column Prob_gain_stability_mean to the dataframe that are missing it
                       if("Prob_gain_stability_mean" %in% names(solution_cc) == FALSE) {
                         solution_cc <- solution_cc %>%
                           left_join(solution %>%
                                       dplyr::select(ID, Prob_gain_stability_mean) %>%
                                       st_drop_geometry(), by = "ID")
                       }

                       #mean climate risk climate-smart
                       selected_cs <- solution_cc %>%
                         st_drop_geometry() %>%
                         filter(solution_1 == 1) %>%
                         mutate(type = "Climate-smart") %>%
                         mutate(weighted_mean_exposure = weighted.mean(!!sym(col_name),
                                                                       area_km2)) %>%
                         as_tibble()

                       #kernel density plot for comparison
                       PUs <- solution %>%
                         st_drop_geometry() %>%
                         mutate(type = "All PUs") %>%
                         mutate(weighted_mean_exposure = weighted.mean(!!sym(col_name),
                                                                       area_km2)) %>%
                         as_tibble()

                       selected <- rbind(selected_cn, selected_cs)

                       # WHY AM I USING TOTAL AREA ONLY FOR PUs?
                       total_area <- PUs %>%
                         summarise(sum(area_km2)) %>%
                         as.numeric()

                       d <- density(PUs[, col_name] %>%
                                      unlist(),
                                    weights = ((PUs$area_km2)/total_area))

                       kd_plot <- ggplot(data = selected) +
                         geom_density(aes(x = !!sym(col_name),
                                          weight = area_km2/total_area,
                                          colour = type, fill = type),
                                      alpha = 0.2) +
                         geom_vline(aes(xintercept = weighted_mean_exposure,
                                        colour = type)#,
                                    #linetype = "dashed",
                                    #linewidth = 0.3
                         ) +
                         scale_fill_manual(values = c("#0F0247", "#26AFD1"),
                                           name = "") +
                         scale_colour_manual(values = c("#0F0247", "#26AFD1"),
                                             name = "") +
                         scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
                         scale_y_continuous(limits = c(0, d$y[which.max(d$y)]*1.2), expand = c(0, 0)) +
                         xlab("Area-weighted resilience") +
                         ylab("Proportion of mangroves") +
                         theme_bw(base_size = 7) +
                         theme(panel.background = element_blank(),
                               text = element_text(size = 8),
                               axis.text = element_text(size = 7),
                               legend.position = "top",
                               legend.key.size = unit(0.3, "cm"),
                               plot.margin = margin(r = 0.5, unit = "cm"))

                       dir.create(paste0("Figures/Country/03_CC_exposure/", split_group, "/RDS"), recursive = TRUE)

                       ggsave(plot = kd_plot, paste0("Figures/Country/03_CC_exposure/",
                                                     split_group, "/kdplot_exposure_",
                                                     CC_direction, "_", prct, ".pdf"),
                              dpi = 300, width = 12, height = 12, units = "cm")

                       saveRDS(kd_plot, paste0("Figures/Country/03_CC_exposure/", split_group,
                                               "/RDS/kdplot_exposure_",
                                               CC_direction, "_", prct, ".rds"))

                       selected <- selected %>%
                         group_by(type) %>%
                         summarise(weighted_mean_exposure = first(weighted_mean_exposure)) %>%
                         mutate(prct_increase_comparison_all_PUs = (weighted_mean_exposure - weighted_mean_exposure[1])/weighted_mean_exposure[1]*100) %>%
                         mutate(prct_increase_comparison_climate_naïve = (weighted_mean_exposure - weighted_mean_exposure[2])/weighted_mean_exposure[2]*100)

                       write.xlsx(selected %>%
                                    st_drop_geometry(), paste0("Figures/Country/03_CC_exposure/",
                                                               split_group, "/kdplot_exposure_",
                                                               CC_direction, "_", prct, ".xlsx"))
                     })
                   })
           })

plan(sequential)

#Description of the figures
writeLines("The figures show a kernel density plot of the weighted exposure of the selected planning units (and the whole planning units)
to climate change

'mean' means that I am using a mean value of landward and seaward change in the prioritisation.

The value reported is the percentage used as a tradeoff to select climate-priority areas for each conservation feature (more on the method 'climate-priority areas' in Buenafe et al. 2023 - https://doi.org/10.1002/eap.2852).",
           "Figures/Country/03_CC_exposure/info.txt")

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
.rs.restartR()