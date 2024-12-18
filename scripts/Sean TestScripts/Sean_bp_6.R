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
  group_by(AREA.NAME) %>%
  summarize(
    total_crimes = n(),
    avg_income = mean(estimate, na.rm = TRUE),
    geometry = st_union(geometry)
  ) %>%
  filter(!is.na(avg_income)) %>%  # Remove rows with missing income
  st_as_sf()

# Plot: Crime vs Median Income Scatter Plot
ggplot(crime_summary, aes(x = avg_income, y = total_crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Crime vs Median Income", x = "Median Income", y = "Total Crimes")

ggplot() +
  geom_sf(data = economic_data, aes(fill = estimate), color = NA) +  # Median income layer
  scale_fill_viridis_c(option = "magma", na.value = "grey50") +
  scale_color_viridis_c(option = "inferno", na.value = "grey50") +
  labs(
    title = "Median Income and Crime Data by Neighborhood in Los Angeles",
    fill = "Median Income",
    color = "Average Income"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  coord_sf(
    xlim = c(-119, -117),  # Adjust these values to focus on the Los Angeles area
    ylim = c(33, 35)       # Adjust these values to focus on the Los Angeles area
  )
# Plot: Combined Map with Income and Crime Data
# Plot: Black Dots (Crime Locations) on Economic Data Heatmap
ggplot() +
  # Economic data as a heatmap
  geom_sf(data = economic_data, aes(fill = estimate), color = NA, alpha = 0.8) +
  # Black dots for crime data
  geom_sf(data = crime_sf, color = "black", size = 0.5, alpha = 0.7) +
  scale_fill_viridis_c(option = "magma", na.value = "grey50") +
  labs(
    title = "Crime Incidents on Median Income Heatmap",
    fill = "Median Income"
  ) +
  coord_sf(
    xlim = c(-119, -117),  # Focus on Los Angeles area
    ylim = c(33, 35)       # Focus on Los Angeles area
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# Linear Model: Relationship Between Crimes and Income
model <- lm(total_crimes ~ avg_income, data = crime_summary)
summary(model)
