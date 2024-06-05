#Author: Alvise Dabalà
#Date: 28/05/2024

pacman::p_load(tidyverse, sf, MetBrewer)

source("Code/Functions/f_addcols_WDPA.r")

split_group <- "biotyp"
CC_direction <- "mean"
prct <- 0.3

solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                           split_group,"/solution_prioritisation.rds")) %>%
  f_addcols_WDPA()

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                              split_group, "/",
                              CC_direction, "/solution_",
                              as.character(prct), "_", CC_direction, ".rds")) %>%
  f_addcols_WDPA()

#% of the solution covered by PAs
tot_area_mangroves <- sum(solution_cc$MangroveArea_km2)

data <- solution_cc %>%
  st_drop_geometry() %>%
  group_by(solution_1) %>%
  summarise(area_mangroves_WDPA_all_km2 = sum(area_mangroves_WDPA_all_km2),
            area_mangroves_WDPA_I_VI_km2 = sum(area_mangroves_WDPA_I_VI_km2),
            area_mangroves_WDPA_I_IV_km2 = sum(area_mangroves_WDPA_I_IV_km2))

plot_data <- data %>%
  rename_with(~ str_remove(., c("area_mangroves_")), everything()) %>%
  rename_with(~ str_remove(., c("_km2")), everything()) %>%
  pivot_longer(!solution_1,
               names_to = "protected_areas_category",
               values_to = "area_mangroves_protected") %>%
  mutate(solution_1 = case_when(solution_1 == 1 ~ "Selected",
                                .default = "Not selected"),
         protected_areas_category = case_when(protected_areas_category == "WDPA_all" ~ "All PAs",
                                              protected_areas_category == "WDPA_I_VI" ~ "Category I-VI",
                                              .default = "Category I-IV"))

aggregate_data <- plot_data %>%
  group_by(protected_areas_category) %>%
  summarise(area_mangroves_protected = sum(area_mangroves_protected))

plot_overlap <- ggplot() +
  geom_col(data = aggregate_data, aes(x = fct_relevel(protected_areas_category,
                                                 c("All PAs", "Category I-VI", "Category I-IV")),
                                 y = (area_mangroves_protected/tot_area_mangroves)*100,
                                 fill = fct_relevel(protected_areas_category,
                                                    c("All PAs", "Category I-VI", "Category I-IV"))),
           linewidth = 0.5,
           position = "dodge") +
  scale_fill_met_d(name = "Egypt", override.order = TRUE) +
  geom_text(data = aggregate_data, aes(label = scales::percent(area_mangroves_protected/tot_area_mangroves,
                                        accuracy = 0.01),
                y = (area_mangroves_protected/tot_area_mangroves)*100,
                x = fct_relevel(protected_areas_category,
                                c("All PAs", "Category I-VI", "Category I-IV"))),
                size = 6 * (5/14),
                colour = "black",
                vjust = -1, hjust = 0.5) +
              theme_bw() +
              theme(legend.position = "top",
                    legend.title = element_blank(),
                    panel.grid.major = element_line(colour = "transparent"),
                    panel.background = element_blank(),
                    legend.key.size = unit(0.5, "cm"),
                    axis.text = element_text(size = 7),
                    axis.title = element_text(size = 9),
                    legend.text = element_text(size = 9),
                    legend.box = 'vertical') +
              ylab("Protected areas coverage (%)") +
              xlab("") +
              scale_y_continuous(expand = c(0, 0), limits = c(0, 100))

            dir.create(paste0("Figures/Country/10_overlap_WDPA/",
                              split_group, "/RDS/"), recursive = TRUE)

            saveRDS(plot_overlap, paste0("Figures/Country/10_overlap_WDPA/",
                                         split_group, "/RDS/overlap_WDPA_area_",
                                         CC_direction, "_", prct, ".rds"))

            xlsx::write.xlsx(data, paste0("Figures/Country/10_overlap_WDPA/",
                                          split_group, "/overlap_WDPA_area_",
                                          CC_direction, "_", prct, ".xlsx"))

            rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
            gc() #free up memrory and report the memory usage.