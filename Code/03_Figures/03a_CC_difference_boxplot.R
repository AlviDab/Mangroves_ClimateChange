#Author: Alvise Dabalà
#Date: 02/10/2024

pacman::p_load(tidyverse, sf, parallel, furrr, purrr, openxlsx, ggeffects, ggstats, ggrepel)

source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_countries.R")

CC_direction <- "mean"

PUs <- readRDS("Results/RDS/PUs_03_mangroves_biotyp_cc_IUCN_MEOW.rds") %>%
  f_int_countries() %>%
  mutate(country = case_when(country == "France" ~ "French Guiana",
                             .default = country) %>%
           gsub(" ", "_", .))

ncores <- detectCores() - 2

plan(multisession, workers = ncores)

future_map(seq(0.05, 1, by = 0.05),
           function(prct) {

             map(c("landward", "seaward",
                   "mean"), function(CC_direction) {

                     col_name <- paste0("Prob_gain_stability_", CC_direction)

                     selected <- map(c("country_and_biotyp", "biotyp"), function(split_group) {

                       solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                                                  split_group,"/solution_prioritisation.rds"))

                       solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                     split_group, "/",
                                                     CC_direction, "/solution_",
                                                     as.character(prct), "_", CC_direction, ".rds"))

                       type_indicator <- ifelse(split_group == "country_and_biotyp",
                                                "country-scale",
                                                "global-scale")

                       #mean climate risk climate-naïve
                       selected_cn <- solution %>%
                         st_drop_geometry() %>%
                         filter(solution_1 == 1) %>%
                         mutate(type = paste("Climate-naïve", type_indicator)) %>%
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
                         mutate(type = paste("Climate-smart", type_indicator)) %>%
                         as_tibble()

                       selected <- rbind(selected_cn, selected_cs) %>%
                         mutate(split_group = split_group)
                     }) %>%
                       bind_rows()

                     total_area <- PUs %>%
                       summarise(sum(area_km2)) %>%
                       as.numeric()

                     selected_summary <- selected %>%
                       group_by(type) %>%
                       summarise(weighted_mean_exposure = weighted.mean(!!sym(col_name),
                                                                     area_km2),
                              std_dev = sqrt(Hmisc::wtd.var(!!sym(col_name), weights = area_km2)[1]),
                              n = n(),
                              std_error = std_dev/sqrt(n)) #How to calculate std error in weighted mean?

                     selected_country <- selected %>%
                       left_join(PUs %>%
                                   dplyr::select(c("country", "cellID"))) %>%
                       group_by(country, type) %>%
                       summarise(weighted_mean_exposure = weighted.mean(!!sym(col_name),
                                                                     area_km2)) %>%
                       mutate(country = str_replace_all(country, "_", " "))

                     countries_ggrepel <- c("Ecuador", "New Caledonia", "Sierra Leone", "Cambodia", "Fiji",
                                            "Equatorial Guinea", "Japan", "Malaysia")

                     pos <- position_jitter(seed = 1, width = 0.2) #position jitter

                     ggplot() +
                       geom_point(data = selected_summary, aes(x = type, y = weighted_mean_exposure),
                                  size = 4) +
                       geom_errorbar(data = selected_summary, aes(x = type, y = weighted_mean_exposure,
                                                                  ymin = weighted_mean_exposure - std_dev,
                                                                  ymax = weighted_mean_exposure + std_dev),
                                     width = 0.2,
                                     linewidth = 1.2) +
                       geom_point(data = selected_country, aes(y = weighted_mean_exposure, x = type,
                                                               colour = type),
                                  position = pos,
                                  linewidth = 0.1, alpha = 0.6) +
                       geom_text_repel(data = selected_country,
                                       aes(x = type, y = weighted_mean_exposure,
                                           label = ifelse(country == "United_Arab_Emirates",
                                                          country,
                                                          "")),
                                       hjust = 0, min.segment.length = 0, max.overlaps = 1.5,
                                       force = 30,
                                       max.iter = 5000, size = 3,
                                       position = pos) +
                       scale_fill_manual(values = c("#B80000", "#F2AC6B", "#003049", "#4B86AA"),
                                         name = "") +
                       scale_colour_manual(values = c("#B80000", "#F2AC6B", "#003049", "#4B86AA"),
                                           name = "") +
                       scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
                       ylab("area-weighted climate-resilience") +
                       xlab("") +
                       theme_bw(base_size = 10) +
                       theme(panel.background = element_blank(),
                             text = element_text(size = 9),
                             axis.text = element_text(size = 10),
                             axis.title = element_text(size = 12, face = 'bold'),
                             legend.position = "top",
                             legend.key.size = unit(0.3, "cm"),
                             plot.margin = margin(r = 0.5, unit = "cm"))
                   })
           })
