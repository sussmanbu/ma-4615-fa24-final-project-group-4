
library(tidyverse)
library(sf)
library(tidycensus)
library(here)
library(viridis)

# Load crime data and convert to spatial format
data <- readRDS(here("filtered_data_2.rds"))
crime_sf <- st_as_sf(data, coords = c("LON", "LAT"), crs = 4326)

# Load neighborhood boundaries
neighborhoods <- st_read(here("LA_Times_Neighborhood_Boundaries-shp/8494cd42-db48-4af1-a215-a2c8f61e96a22020328-1-621do0.x5yiu.shp")) %>%
  st_transform(st_crs(crime_sf))

# Join crime incidents with neighborhoods
crime_with_neighborhoods <- st_join(crime_sf, neighborhoods, join = st_within)
census_api_key("c2aebe6041f0c99e41a3458ed8d0b95ee3650fa4")

# Load and transform median income data
economic_data <- get_acs(
  geography = "block group",
  variables = c(median_income = "B19013_001"),
  state = "CA",
  county = "Los Angeles",
  year = 2020,
  geometry = TRUE
) %>% 
  st_transform(st_crs(crime_with_neighborhoods))

# Join income with neighborhood-level crime data
crime_with_income <- st_join(crime_with_neighborhoods, economic_data, join = st_within)

# Summarize crime data by neighborhood
crime_summary <- crime_with_income %>%
  group_by(GEOID) %>%
  summarize(
    total_crimes = n(),
    avg_income = mean(estimate, na.rm = TRUE),
    geometry = st_union(geometry)
  ) %>%
  filter(!is.na(avg_income)) %>%  # Remove rows with missing income
  st_as_sf()



laCityBounday <- st_read(here("CityBoundaryofLosAngeles/geo_export_416eaeda-447a-4473-b003-37e2cad181ac.shp"))
