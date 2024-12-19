pacman::p_load(tidyverse, sf)

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/biotyp/mean/solution_0.3_mean.rds")) %>%
  mutate(solution_cc = solution_1) %>%
  select(c("ID", "solution_cc"))

solution_nocc <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/biotyp/solution_prioritisation.rds")) %>%
  mutate(solution_nocc = solution_1)

solution_cc_country <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/country_and_biotyp/mean/solution_0.3_mean.rds")) %>%
  mutate(solution_cc_country = solution_1) %>%
  select(c("ID", "solution_cc_country"))

source("Code/Functions/f_intersect_countries.R")

#Intersect countries
PUs_country <- solution_nocc %>%
  left_join(solution_cc %>%
              st_drop_geometry(), by = "ID") %>%
  left_join(solution_cc_country %>%
              st_drop_geometry(), by = "ID") %>%
  f_int_countries()

area_selected_cc <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_cc == TRUE) %>%
  group_by(country) %>%
  summarise(area_selected_cc = sum(area_km2)) %>%
  arrange(desc(area_selected_cc))

area_selected_nocc <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_nocc == TRUE) %>%
  group_by(country) %>%
  summarise(area_selected_nocc = sum(area_km2)) %>%
  arrange(desc(area_selected_nocc))

area_selected_cc_country <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_cc_country == TRUE) %>%
  group_by(country) %>%
  summarise(area_selected_cc_country = sum(area_km2)) %>%
  arrange(desc(area_selected_cc_country))

area_not_selected_cc <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_cc == FALSE) %>%
  group_by(country) %>%
  summarise(area_not_selected_cc = sum(area_km2)) %>%
  arrange(desc(area_not_selected_cc))

area_not_selected_nocc <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_nocc == FALSE) %>%
  group_by(country) %>%
  summarise(area_not_selected_nocc = sum(area_km2)) %>%
  arrange(desc(area_not_selected_nocc))

area_not_selected_cc_country <- PUs_country %>%
  st_drop_geometry() %>%
  filter(solution_cc_country == FALSE) %>%
  group_by(country) %>%
  summarise(area_not_selected_cc_country = sum(area_km2)) %>%
  arrange(desc(area_not_selected_cc_country))

resilience_country <- PUs_country %>%
  st_drop_geometry() %>%
  group_by(country) %>%
  summarise(resilience = weighted.mean(Prob_gain_stability_mean, area_km2))

prct_area_selected <- area_selected_cc %>%
  full_join(area_selected_nocc, by = "country") %>%
  full_join(area_selected_cc_country, by = "country") %>%
  full_join(area_not_selected_cc, by = "country") %>%
  full_join(area_not_selected_nocc, by = "country") %>%
  full_join(area_not_selected_cc_country, by = "country") %>%
  left_join(resilience_country, by = "country") %>%
  mutate(
    across(everything(), replace_na, 0)
  ) %>%
  mutate(prct_area_selected_cc = area_selected_cc/(area_not_selected_cc + area_selected_cc),
         prct_area_selected_nocc = area_selected_nocc/(area_not_selected_nocc + area_selected_nocc),
         prct_area_selected_cc_country = area_selected_cc_country/(area_not_selected_cc_country + area_selected_cc_country)) %>%
  mutate(change_prct_area_selected_nocc_to_cc = prct_area_selected_cc - prct_area_selected_nocc) %>%
  arrange(desc(change_prct_area_selected_nocc_to_cc))

n_countries <- prct_area_selected %>%
  summarise(prct_area_selected_cc_0 = sum(ifelse(prct_area_selected_cc == 0, 1, 0)),
            prct_area_selected_cc_less_than_5 = sum(ifelse(prct_area_selected_cc <= 0.05, 1, 0)) - prct_area_selected_cc_0,
            prct_area_selected_nocc_0 = sum(ifelse(prct_area_selected_nocc == 0, 1, 0)),
            prct_area_selected_nocc_less_than_5 = sum(ifelse(prct_area_selected_nocc <= 0.05, 1, 0)) - prct_area_selected_nocc_0)

std_area_selected <- prct_area_selected %>%
  summarise(sd_cc = sd(prct_area_selected_cc),
            sd_cc_country = sd(prct_area_selected_cc_country))