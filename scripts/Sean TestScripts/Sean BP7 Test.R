
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



lapd_boundary <- st_read(here("CityBoundaryofLosAngeles/geo_export_416eaeda-447a-4473-b003-37e2cad181ac.shp")) %>%
  st_transform(st_crs(crime_sf))  # Align CRS
plot(st_geometry(lapd_boundary))  # Visualize the boundary


crime_within_lapd <- crime_sf %>%
  filter(rowSums(st_within(geometry, st_geometry(lapd_boundary), sparse = FALSE)) > 0)

crime_with_income_lapd <- st_join(crime_within_lapd, economic_data, join = st_within)

economic_data_lapd <- st_intersection(economic_data, lapd_boundary)


# Update the crime summary
crime_summary_lapd <- crime_with_income_lapd %>%
  group_by(GEOID) %>%
  summarize(
    total_crimes = n(),
    avg_income = mean(estimate, na.rm = TRUE),
    geometry = st_union(geometry)
  ) %>%
  filter(!is.na(avg_income)) %>%  # Exclude rows with missing income
  st_as_sf()

# Update visualizations
ggplot(crime_summary_lapd, aes(x = avg_income, y = total_crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Crime vs Median Income (LAPD Jurisdiction)", x = "Median Income", y = "Total Crimes")

ggplot() +
  geom_sf(data = economic_data_lapd, aes(fill = estimate), color = NA) +  # Median income layer
  scale_fill_viridis_c(option = "magma", na.value = "grey50") +
  scale_color_viridis_c(option = "inferno", na.value = "grey50") +
  labs(
    title = "Median Income within LAPD Jurisdiction",
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
    xlim = c(-119, -117),  # Focus on the Los Angeles area
    ylim = c(33, 35)       # Focus on the Los Angeles area
  )


crime_within_lapd <- st_transform(crime_within_lapd, st_crs(economic_data_lapd))

ggplot() +
  # Median income heatmap
  geom_sf(data = economic_data_lapd, aes(fill = estimate), color = NA) +  
  scale_fill_viridis_c(option = "magma", na.value = "grey50") +
  scale_color_viridis_c(option = "inferno", na.value = "grey50") +
  # Overlay crime points
  geom_sf(data = crime_within_lapd, aes(), color = "black", shape = 4, size = 0.5, alpha = 0.7) +
  labs(
    title = "Median Income and Crime Data within LAPD Jurisdiction",
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
    xlim = c(-119, -117),  # Focus on the Los Angeles area
    ylim = c(33, 35)       # Focus on the Los Angeles area
  )
#Good place to use shinylive here. Can filter by year. 

# crime_filtered <- crime_within_lapd %>%
#   filter(year == 2021)
# ggplot() +
#   geom_sf(data = economic_data_lapd, aes(fill = estimate), color = NA) +  
#   scale_fill_viridis_c(option = "magma", na.value = "grey50") +
#   geom_sf(data = crime_filtered, aes(), color = "black", shape = 4, size = 0.5, alpha = 0.7) +
#   labs(
#     title = "Median Income and 2020 Crime Data within LAPD Jurisdiction",
#     fill = "Median Income"
#   ) +
#   theme_minimal() +
#   coord_sf(xlim = c(-119, -117), ylim = c(33, 35))

#Look at data utilizing a poisson distribution model. 
# Fit a Poisson regression model
poisson_model <- glm(total_crimes ~ avg_income, 
                     family = poisson(link = "log"), 
                     data = crime_summary_lapd)

# Summary of the model
summary(poisson_model)
exp(coef(poisson_model))

residual_deviance <- poisson_model$deviance
degrees_of_freedom <- poisson_model$df.residual
dispersion <- residual_deviance / degrees_of_freedom
dispersion

quasi_poisson_model <- glm(total_crimes ~ avg_income, 
                           family = quasipoisson(link = "log"), 
                           data = crime_summary_lapd)
summary(quasi_poisson_model)

library(MASS)
nb_model <- glm.nb(total_crimes ~ avg_income, data = crime_summary_lapd)
summary(nb_model)

crime_summary_lapd <- crime_summary_lapd %>%
  mutate(fitted_crimes = predict(poisson_model, type = "response"))

# Plot observed vs. predicted
ggplot(crime_summary_lapd, aes(x = avg_income, y = total_crimes)) +
  geom_point() +
  geom_line(aes(y = fitted_crimes), color = "red") +
  labs(
    title = "Poisson Regression: Crime vs. Median Income",
    x = "Median Income",
    y = "Total Crimes"
  )

crime_summary_lapd <- crime_summary_lapd %>%
  mutate(fitted_crimes_nb = predict(nb_model, type = "response"))






