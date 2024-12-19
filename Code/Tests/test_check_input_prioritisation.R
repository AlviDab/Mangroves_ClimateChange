gsub("^[^_]*_[^_]*_[^_]*_", "", PUs_prioritisation_input %>%
       select(starts_with("Sp")) %>%
       names()) %>%
  gsub("_[^_]*$", "", .) %>%
  unique()

  gsub("^[^_]*_[^_]*_[^_]*_", "", targets_by_country_and_biotyp %>%
         select(feature) %>%
         unlist()) %>%
  gsub("_[^_]*$", "", .) %>%
  unique()
