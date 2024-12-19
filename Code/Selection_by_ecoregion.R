pacman::p_load(tidyverse, sf)

solution_cc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/biotyp/mean/solution_0.3_mean.rds")) %>%
  mutate(solution_cc = solution_1) %>%
  select(c("ID", "solution_cc"))
solution_nocc <- readRDS(paste0("Results/RDS/prioritisation/Country/01_prioritisation/biotyp/solution_prioritisation.rds")) %>%
  mutate(solution_nocc = solution_1)

moll_proj <- "ESRI:54009"

MEOW <- read_sf("Data/MEOW/Marine_Ecoregions_Of_the_World__MEOW_.shp") %>%
  st_transform(moll_proj) %>%
  st_make_valid()

PUs_MEOW_index <- solution_nocc %>%
  st_centroid() %>% #so that it intersect only one ecoregion
  st_intersects(MEOW) %>%
  as_vector()

MEOW_ecoregions <- MEOW %>%
  st_drop_geometry() %>%
  dplyr::select(ECOREGION) %>%
  mutate(ECOREGION = paste0("MEOW_",
                            str_replace_all(MEOW$ECOREGION, " ", "_") %>% #To not have any column with ``
                              str_replace_all("/", "_and_")))

PUs_MEOW <- solution_nocc %>%
  mutate(MEOW_ecoregions[PUs_MEOW_index, ]) %>%
  rename(MEOW = ECOREGION) %>%
  relocate(geometry, .before = "ID") %>%
  relocate(MEOW, .before = "area_km2") %>%
  st_as_sf() %>%
  left_join(solution_cc %>%
              st_drop_geometry(), by = "ID")

area_selected_cc <- PUs_MEOW %>%
  st_drop_geometry() %>%
  filter(solution_cc == TRUE) %>%
  group_by(MEOW) %>%
  summarise(area_selected_cc = sum(area_km2)) %>%
  arrange(desc(area_selected_cc))

area_selected_nocc <- PUs_MEOW %>%
  st_drop_geometry() %>%
  filter(solution_nocc == TRUE) %>%
  group_by(MEOW) %>%
  summarise(area_selected_nocc = sum(area_km2)) %>%
  arrange(desc(area_selected_nocc))

area_not_selected_cc <- PUs_MEOW %>%
  st_drop_geometry() %>%
  filter(solution_cc == FALSE) %>%
  group_by(MEOW) %>%
  summarise(area_not_selected_cc = sum(area_km2)) %>%
  arrange(desc(area_not_selected_cc))

area_not_selected_nocc <- PUs_MEOW %>%
  st_drop_geometry() %>%
  filter(solution_nocc == FALSE) %>%
  group_by(MEOW) %>%
  summarise(area_not_selected_nocc = sum(area_km2)) %>%
  arrange(desc(area_not_selected_nocc))

resilience_MEOW <- PUs_MEOW %>%
  st_drop_geometry() %>%
  group_by(MEOW) %>%
  summarise(resilience = weighted.mean(Prob_gain_stability_mean, area_km2))

prct_area_selected <- area_selected_cc %>%
  full_join(area_not_selected_cc, by = "MEOW") %>%
  full_join(area_selected_nocc, by = "MEOW") %>%
  full_join(area_not_selected_nocc, by = "MEOW") %>%
  left_join(resilience_MEOW, by = "MEOW") %>%
  mutate(
    across(everything(), replace_na, 0)
  ) %>%
  mutate(prct_area_selected_cc = area_selected_cc/(area_not_selected_cc + area_selected_cc),
         prct_area_selected_nocc = area_selected_nocc/(area_not_selected_nocc + area_selected_nocc)) %>%
  mutate(change_prct_area_selected_nocc_to_cc = prct_area_selected_cc - prct_area_selected_nocc) %>%
  arrange(desc(change_prct_area_selected_nocc_to_cc))