#Load packages
pacman::p_load(tidyverse,
               sf,
               leaflet,
               shiny)

#Load data

el_salvador_municipios <- st_read("data/raw/El Salvador_Municipios/WGS84_Municipios.shp")
sa_area_base2 <- st_read("data/raw/sa_area_base2.geojson")
sss_area_base <- st_read("data/raw/sss_area_base.geojson")
ch_area_base <- st_read("data/raw/ch_area_base_4326.geojson")


# Collapse each into a single boundary
sf_use_s2(FALSE)
sa_boundary <- sa_area_base2 %>%
  st_buffer(0) %>%             # Fix self-intersections
  st_union() %>%
  st_as_sf() %>%
  mutate(area = "SA")

sss_union <- sss_area_base %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(area = "SSS")

ch_union <- ch_area_base %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf() %>%
  mutate(area = "CH")

# Combine into a single sf object
intervention_areas <- bind_rows(sa_boundary, sss_union, ch_union) %>%
  mutate(area = case_when(area == "CH" ~ "Chalcuapa",
                          area == "SA" ~ "SantaAna",
                          area == "SSS" ~ "SanSebastian"))

# Ensure geometry is valid and still an sf object
intervention_areas <- st_make_valid(intervention_areas)

# Transform to UTM safely
utm_crs <- 32616
intervention_utm <- st_transform(intervention_areas, crs = utm_crs)

# Apply 100m buffer and union to remove holes/pockets
intervention_outer <- intervention_utm %>%
  group_by(area) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  st_buffer(0) %>%
  st_union() %>%                   # Collapse any fragments
  st_cast("MULTIPOLYGON") %>%
  st_as_sf()

# Back to WGS84
intervention_outer <- st_transform(intervention_outer, crs = 4326)

saveRDS(intervention_areas, "data/Shapefiles/intervention_areas.Rds")

# Ensure everything is valid and in WGS84
el_salvador_municipios <- st_make_valid(el_salvador_municipios)
intervention_outer <- st_make_valid(intervention_outer)

# Transform both to projected CRS for buffering (UTM Zone 16N)
utm_crs <- 32616
el_salvador_utm <- st_transform(el_salvador_municipios, crs = utm_crs)
intervention_outer <- st_transform(intervention_outer, crs = utm_crs)

# Create 50 km buffer
intervention_buffer <- st_buffer(intervention_outer, dist = 200)
intervention_buffer <- st_transform(intervention_buffer, 4326)
saveRDS(intervention_buffer, "data/Shapefiles/intervention_buffer.Rds")

# Create inverse mask
country_outline <- el_salvador_utm %>%
  st_union() %>%
  st_make_valid()

intervention_union <- st_union(intervention_buffer) %>%
  st_make_valid() %>%
  st_transform(st_crs(country_outline))

intervention_mask <- st_difference(country_outline, intervention_union) %>%
  st_make_valid() %>%
  st_as_sf()

intervention_mask <- st_transform(intervention_mask, 4326)
saveRDS(intervention_mask, "data/Shapefiles/intervention_mask.Rds")

sf_use_s2(TRUE)
