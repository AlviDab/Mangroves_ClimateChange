pacman::p_load(tidyverse, sf)

solution_nocc <- readRDS(paste0("Results/RDS/prioritisation/Country/02_prioritisation_CC/biotyp/mean/solution_0.3_mean.rds"))

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
  st_as_sf()

area_selected <- PUs_MEOW %>%
  st_drop_geometry() %>%
  filter(solution_1 == TRUE) %>%
  group_by(MEOW) %>%
  summarise(area_selected = sum(area_km2)) %>%
  arrange(desc(area_selected))

area_not_selected <- PUs_MEOW %>%
  st_drop_geometry() %>%
  filter(solution_1 == FALSE) %>%
  group_by(MEOW) %>%
  summarise(area_not_selected = sum(area_km2)) %>%
  arrange(desc(area_not_selected))

prct_area_selected <- area_selected %>%
  left_join(area_not_selected, by = "MEOW") %>%
  mutate(prct_area_selected = area_selected/(area_not_selected + area_selected)) %>%
  arrange(desc(prct_area_selected))

