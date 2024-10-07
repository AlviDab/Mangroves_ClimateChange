#Author: Alvise Dabalà
#Date: 14/06/2024

pacman::p_load(tidyverse, sf, parallel,
               furrr, purrr, spatstat,
               collapse, modelr, openxlsx)

ncores <- detectCores() - 2

# plan(multisession, workers = ncores)

area_cr <- map(c("landward", "seaward"), function(CC_direction) {
  map(c("country_and_biotyp",
        "biotyp"), function(split_group) {

          future_map(seq(0, 1, by = 0.05),
                     function(prct) {
                       if(prct == 0) {
                         solution <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/",
                                                    split_group,"/solution_prioritisation.rds"))
                       } else {

                         solution <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/",
                                                    split_group, "/",
                                                    CC_direction, "/solution_",
                                                    as.character(prct), "_", CC_direction, ".rds"))

                       }

                       solution <- solution %>%
                         rename_with(~"resilience", !!sym(paste0("Prob_gain_stability_", CC_direction)))

                       area <- solution %>%
                         st_drop_geometry() %>%
                         filter(solution_1 == 1) %>%
                         summarise(Area_mangrove_km2 = sum(MangroveArea_km2))

                       #Median resilience
                       climate_resilient_median_mean <- solution %>%
                         st_drop_geometry() %>%
                         filter(solution_1 == 1) %>%
                         summarise(median_resilience =
                                     spatstat.univar::weighted.median(resilience,
                                                                      MangroveArea_km2),
                                   mean_resilience =
                                     weighted.mean(resilience,
                                                   MangroveArea_km2))
                       # mode_resilience = collapse::fmode(resilience,
                       #                                   MangroveArea_km2))

                       #Filter mangroves with resilience >= 75
                       climate_resilient_75 <- solution %>%
                         st_drop_geometry() %>%
                         filter(solution_1 == 1,
                                resilience >= 75) %>%
                         summarise(area_climate_resilient_75 = sum(MangroveArea_km2))

                       area_cr <- area %>%
                         cbind(climate_resilient_median_mean) %>%
                         cbind(climate_resilient_75) %>%
                         add_column(threshold = prct)
                     }) %>%
            bind_rows() %>%
            mutate(scale = split_group,
                   edge = CC_direction) %>%
            mutate(prct_increase_area = (Area_mangrove_km2 -
                                           Area_mangrove_km2[1])/Area_mangrove_km2[1],
                   prct_increase_mean_resilience = (mean_resilience -
                                                      mean_resilience[1])/mean_resilience[1],
                   ratio_prct_increase = prct_increase_mean_resilience/prct_increase_area*100)
        })%>%
    bind_rows()
}) %>%
  bind_rows()

area_cr_global_lw <- area_cr %>%
  filter(scale == "biotyp",
         edge == "landward") %>%
  filter(prct_increase_area != 0)

area_cr_global_sw <- area_cr %>%
  filter(scale == "biotyp",
         edge == "seaward") %>%
  filter(prct_increase_area != 0)

area_cr_country_lw <- area_cr %>%
  filter(scale == "country_and_biotyp",
         edge == "landward") %>%
  filter(prct_increase_area != 0)

area_cr_country_sw <- area_cr %>%
  filter(scale == "country_and_biotyp",
         edge == "seaward") %>%
  filter(prct_increase_area != 0)

fit_global_lw <- lm(prct_increase_mean_resilience*100 ~ log(prct_increase_area*100),
                    data = area_cr_global_lw)
fit_global_sw <- lm(prct_increase_mean_resilience*100 ~ log(prct_increase_area*100),
                    data = area_cr_global_sw)
fit_country_lw <- lm(prct_increase_mean_resilience*100 ~ log(prct_increase_area*100),
                     data = area_cr_country_lw)
fit_country_sw <- lm(prct_increase_mean_resilience*100 ~ log(prct_increase_area*100),
                     data = area_cr_country_sw)

scatterplot <- ggplot(data = area_cr, aes(x = prct_increase_area*100,
                                          y = prct_increase_mean_resilience*100)) +
  geom_point(aes(colour = scale, shape = edge), alpha = 0.7, size = 1) +
  geom_function(fun = function(x) fit_global_lw$coefficients[1] +
                  fit_global_lw$coefficients[2]*log(x),
                xlim = c(min(area_cr_global_lw$prct_increase_area*100), max(area_cr_global_lw$prct_increase_area*100)),
                colour = "#003049") +
  geom_function(fun = function(x) fit_global_sw$coefficients[1] +
                  fit_global_sw$coefficients[2]*log(x),
                xlim = c(min(area_cr_global_sw$prct_increase_area*100), max(area_cr_global_sw$prct_increase_area*100)),
                colour = "#003049",
                linetype = "dashed") +
  geom_function(fun = function(x) fit_country_lw$coefficients[1] +
                  fit_country_lw$coefficients[2]*log(x),
                xlim = c(min(area_cr_country_lw$prct_increase_area*100), max(area_cr_country_lw$prct_increase_area*100)),
                colour = "#4B86AA") +
  geom_function(fun = function(x) fit_country_sw$coefficients[1] +
                  fit_country_sw$coefficients[2]*log(x),
                xlim = c(min(area_cr_country_sw$prct_increase_area*100), max(area_cr_country_sw$prct_increase_area*100)),
                colour = "#4B86AA",
                linetype = "dashed") +
  scale_colour_manual(values = c("#003049", "#4B86AA"),
                      labels = c('Global scale',
                                 'Country scale'),
                      guide = guide_legend()) +
  scale_shape_manual(values = c(16, 17),
                     labels = c('Landward',
                                'Seaward'),
                     guide = guide_legend()) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.1),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.05),
    legend.position = "top"
  ) +
  ylab("Percentage increase in resilience (%)") +
  xlab("Percentage increase in area (%)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(colour = "grey20"
                                    #face = "bold"
        ),
        axis.title.y = element_text(colour = "grey20"
                                    #face = "bold"
        )) +
  scale_x_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1)))

dir.create(paste0("Figures/Country/12_linear_investment/RDS"), recursive = TRUE)

write.xlsx(area_cr %>%
             st_drop_geometry(), "Figures/Country/12_linear_investment/linear_investment.xlsx")

ggsave(plot = scatterplot, "Figures/Country/12_linear_investment/linear_investment.pdf",
       dpi = 300, width = 18, height = 11, units = "cm")

saveRDS(scatterplot, "Figures/Country/12_linear_investment/RDS/linear_investment.rds")
