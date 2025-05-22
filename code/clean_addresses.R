#Load packages
pacman::p_load(tidyverse)

#Load data

addresses <- readRDS("data/raw/0_address.RDS")

#Create unique ID

addresses_clean <- addresses %>%
  mutate(geocode_id = row_number()) %>%
  mutate(
    geocode_accuracy = case_when(
      # Highest quality: full rooftop/street match
      geocode_match_type == "full_match" |
        geocode_location_type == "ROOFTOP" |
        geocode_resulttype == "intersection" ~ "full_match",
      
      # Match to a specific street or interpolated point
      geocode_location_type %in% c("RANGE_INTERPOLATED", "GEOMETRIC_CENTER") |
        geocode_resulttype == "street" |
        geocode_match_type %in% c("match_by_street", "match_by_postcode") ~ "street",
      
      # Place or neighborhood
      geocode_resulttype %in% c("place") |
        geocode_match_type == "match_by_building" ~ "place",
      
      # Broader locality or district
      geocode_resulttype %in% c("locality") |
        geocode_match_type == "match_by_city_or_disrict" ~ "city",
      
      # National or regional match
      geocode_match_type == "match_by_country_or_state" ~ "country",
      
      # Missing or unclassified
      TRUE ~ "unknown"
    )
  ) %>%
  select(geocode_id, sn, mun, direccion_full, geocode_accuracy, geocode_match_address) %>%
  mutate(geocode_accuracy = factor(geocode_accuracy, levels=c("full_match", "street", "place", "city", "country", "unknown")))
  
saveRDS(addresses_clean, "data/clean/addresses_geocode_pts.Rds")

addresses_single <- addresses_clean %>%
  select(sn, mun, direccion_full) %>%
  sf::st_drop_geometry() %>%
  unique()

saveRDS(addresses_single, "data/clean/addresses_list.Rds")

