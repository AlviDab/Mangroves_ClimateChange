# Author: Alvise Dabalà
# Date: 01/07/2025
# Description: This script compares the area and the climate resilience of the
#              areas selected by the different prioritisations used for the sensitivity analysis

################################################################################

# Load packages
pacman::p_load(tidyverse, sf, parallel, furrr, purrr, openxlsx, ggeffects, ggstats, ggrepel)

# Load functions
source("Code/Functions/f_intersect_continents.r")
source("Code/Functions/f_intersect_countries.r")

# Load data
PUs <- readRDS("Results/RDS/PUs_02_mangroves_biotyp_cc_IUCN.rds") %>%
  f_int_countries()

# Set parameters
prct <- 0.3
CC_direction <- "mean"

# List all the prioritisations available
prioritisations_list <- list.files("Results/RDS/prioritisation/Country/01_prioritisation/")

# Keep only results to use for the sensitivity analysis
prioritisations_list <- prioritisations_list[str_detect(prioritisations_list, "30|1000PU")]

# Column name for the climate resilience
col_name <- paste0("Prob_gain_stability_", CC_direction)

# Extract the areas selected by each prioritisation
selected <- map(prioritisations_list, function(prior_name) {
  solution <- readRDS(paste0(
    "Results/RDS/prioritisation/Country/01_prioritisation/",
    prior_name, "/solution_prioritisation.rds"
  ))

  # climate-smart
  solution_cc <- readRDS(paste0(
    "Results/RDS/prioritisation/Country/02_prioritisation_CC/",
    prior_name, "/",
    CC_direction, "/solution_",
    as.character(prct), "_", CC_direction, ".rds"
  ))

  # Define indicators for the plot
  scale_indicator <- ifelse("country_and_biotyp" %>%
    grepl(prior_name),
  "Country-scale",
  "Global-scale"
  )

  # Define sensitivity indicator
  sensitivity_indicator <- ifelse("30" %>%
    grepl(prior_name),
  "Uniform targets (30%)",
  paste0("1000 ", "km²", " planning units")
  )

  # climate-naïve
  selected_cn <- solution %>%
    st_drop_geometry() %>%
    filter(solution_1 == 1) %>%
    mutate(
      type = paste(
        "Climate-naïve",
        scale_indicator,
        sensitivity_indicator
      ),
      climate = "Climate-naïve",
      scale = scale_indicator,
      sensitivity = sensitivity_indicator
    ) %>%
    as_tibble()

  # climate-smart
  selected_cs <- solution_cc %>%
    st_drop_geometry() %>%
    filter(solution_1 == 1) %>%
    mutate(
      type = paste(
        "Climate-smart",
        scale_indicator,
        sensitivity_indicator
      ),
      climate = "Climate-smart",
      scale = scale_indicator,
      sensitivity = sensitivity_indicator
    ) %>%
    as_tibble()

  selected <- rbind(selected_cn, selected_cs) %>%
    mutate(prior_name = prior_name)
}) %>%
  bind_rows()

# Calculate the total mangrove area
total_mangrove_area <- PUs %>%
  st_drop_geometry() %>%
  summarise(area = sum(area_km2))

# Calculate the area and percentage of area respect to the total selected by each prioritisation type
selected_area <- selected %>%
  group_by(type) %>%
  summarise(
    resilience = weighted.mean(Prob_gain_stability_mean, area_km2, na.rm = TRUE),
    area_km2 = sum(area_km2),
    prct_area = (area_km2 / total_mangrove_area$area) * 100,
    sensitivity = first(sensitivity),
    climate = first(climate),
    scale = first(scale)
  ) %>%
  ungroup()

# Define a theme for the plots
theme_layer <- list(theme(
  axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 12),
  strip.text.x = element_text(size = 12, face = "bold"),
  axis.title.y = element_text(size = 14, face = "bold"),
  legend.title = element_blank(),
  panel.grid.major = element_line(colour = "grey90", linewidth = 0.1),
  panel.grid.minor = element_line(colour = "grey90", linewidth = 0.05),
  legend.position = "bottom",
  plot.tag = element_text(face = "bold"),
  # title = element_blank(), #element_text(size = 12, face = 'bold'),
  legend.text = element_text(size = 12),
  plot.title = element_text(hjust = 0.5)
))

# Now we plot the area
(ggplot_area <- ggplot(selected_area, aes(x = scale, y = prct_area, fill = climate)) +
  geom_bar(
    stat = "identity", position = "dodge",
    color = "black"
  ) +
  labs(
    x = "",
    y = "Percentage of total mangrove area selected"
  ) +
  theme_bw() +
  theme_layer +
  scale_fill_manual(values = c("Climate-naïve" = "#F2AC6B", "Climate-smart" = "#4B86AA")) +
  # ggtitle("Percentage mangrove area selected by the prioritisations used for sensitivity analysis") +
  facet_wrap(~sensitivity) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))))

# And the resilience with a similar graph but not using bar, col or boxplot, just showing the horizontal line
(ggplot_resilience <- ggplot(selected_area, aes(x = scale, y = resilience, color = climate)) +
  geom_point(size = 3) +
  labs(
    x = "",
    y = "Area-weighted climate resilience"
  ) +
  theme_bw() +
  theme_layer +
  scale_color_manual(values = c("Climate-naïve" = "#F2AC6B", "Climate-smart" = "#4B86AA")) +
  # ggtitle("Area-weighted climate resilience of the areas selected by the prioritisations used for sensitivity analysis") +
  facet_wrap(~sensitivity))

# Create output directory if it does not exist


# Save the both ggplot results in a pdf
ggsave(
  plot = ggplot_area, "Figures/Country/08_comparison_area_resilience_sensitivity/08_comparison_area.jpg",
  dpi = 300, width = 18, height = 15, units = "cm"
)

# Resilience
ggsave(
  plot = ggplot_resilience, "Figures/Country/08_comparison_area_resilience_08_comparison_resilience.jpg",
  dpi = 300, width = 18, height = 15, units = "cm"
)

# Clean environment
rm(list = ls())
gc()
.rs.restartR()
