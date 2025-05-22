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
stopifnot(inherits(intervention_areas, "sf"))

# Check structure before transforming
print(st_geometry_type(intervention_areas))
print(st_crs(intervention_areas))

# Transform to UTM safely
utm_crs <- 32616
intervention_utm <- st_transform(intervention_areas, crs = utm_crs)
stopifnot(inherits(intervention_utm, "sf"))

# Confirm geometry remains
print(st_geometry_type(intervention_utm))

# Apply 100m buffer and union to remove holes/pockets
intervention_outer <- intervention_utm %>%
  group_by(area) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  st_buffer(500) %>%
  st_union() %>%                   # Collapse any fragments
  st_cast("MULTIPOLYGON") %>%
  st_as_sf()

# Back to WGS84
intervention_outer <- st_transform(intervention_outer, crs = 4326)


sf_use_s2(TRUE)

saveRDS(intervention_outer, "data/Shapefiles/intervention_areas.Rds")

# Ensure everything is valid and in WGS84
el_salvador_municipios <- st_make_valid(el_salvador_municipios)
intervention_areas <- st_make_valid(intervention_areas)

# Transform both to projected CRS for buffering (UTM Zone 16N)
utm_crs <- 32616
el_salvador_utm <- st_transform(el_salvador_municipios, crs = utm_crs)
intervention_utm <- st_transform(intervention_areas, crs = utm_crs)

# Create 50 km buffer
intervention_buffer <- st_buffer(intervention_utm, dist = 10000)

# Combine original and buffer
intervention_extended <- st_union(st_combine(rbind(intervention_utm, intervention_buffer)))

# Filter municipios that intersect extended intervention area
el_salvador_filtered <- el_salvador_utm %>%
  filter(st_intersects(., intervention_extended, sparse = FALSE) %>% apply(1, any))

# Optionally transform back to WGS84 before saving
el_salvador_filtered <- st_transform(el_salvador_filtered, crs = 4326)

# Save result
saveRDS(el_salvador_filtered, "data/Shapefiles/municipos.Rds")
