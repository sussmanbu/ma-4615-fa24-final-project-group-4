library(dplyr)
library(purrr)
library(tidyr)
library(here)

# Load the data
data <- readRDS(here("filtered_data_copy.rds"))

# Convert the date column to year format and extract year
data$DATE.OCC <- as.POSIXct(data$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")
data$year <- as.numeric(format(data$DATE.OCC, "%Y"))

# Filter for the years 2020–2024
filtered_years <- data %>%
  filter(year %in% 2020:2024)

# Create a tibble with the mapping of descent codes
descent_codes <- tribble(
  ~Descent_Code, ~Descent_Description,
  "A", "Other Asian",
  "B", "Black",
  "C", "Chinese",
  "D", "Cambodian",
  "F", "Filipino",
  "G", "Guamanian",
  "H", "Hispanic/Latin/Mexican",
  "I", "American Indian/Alaskan Native",
  "J", "Japanese",
  "K", "Korean",
  "L", "Laotian",
  "O", "Other",
  "P", "Pacific Islander",
  "S", "Samoan",
  "U", "Hawaiian",
  "V", "Vietnamese",
  "W", "White",
  "X", "Unknown",
  "Z", "Asian Indian"
)

# Join the tibble with the main dataset by Vict.Descent
data_clean <- filtered_years %>%
  left_join(descent_codes, by = c("Vict.Descent" = "Descent_Code"))

# Replace unmatched or missing descriptions with "Unknown"
data_clean$Descent_Description[is.na(data_clean$Descent_Description)] <- "Unknown"

# Drop irrelevant columns with many NAs
data_clean <- data_clean %>%
  select(-c(Crm.Cd.2, Crm.Cd.3, Crm.Cd.4, Weapon.Used.Cd, Weapon.Desc, Cross.Street))

# Extract the 'month' column in "YYYY-MM" format
data_clean$month <- format(data_clean$DATE.OCC, "%Y-%m")

# Split by year and sample up to 2,500 rows per year
sampled_data <- data_clean %>%
  group_split(year) %>%
  map_df(~ slice_sample(.x, n = min(2500, nrow(.x))))

# Save the filtered data as an RDS file
saveRDS(sampled_data, "filtered_data_2.rds")

# Check the cleaned and sampled data
nrow(sampled_data)
str(sampled_data)
