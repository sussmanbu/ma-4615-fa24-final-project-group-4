# Load required libraries
library(tibble)
library(dplyr)
library(ggplot2)


data <- readRDS("~/Documents/BU Fall 2024/MA 415/MA 415 G4 Final Project/filtered_data_copy.rds")

str(data)
colSums(is.na(data))

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
data_clean <- data %>%
  left_join(descent_codes, by = c("Vict.Descent" = "Descent_Code"))

# Replace unmatched or missing descriptions with "Unknown"
data_clean$Descent_Description[is.na(data_clean$Descent_Description)] <- "Unknown"

# Drop irrelevant columns with many NAs
data_clean <- data_clean[, !(names(data_clean) %in% c("Crm.Cd.2", "Crm.Cd.3", 
                                                      "Crm.Cd.4", "Weapon.Used.Cd", 
                                                      "Weapon.Desc", "Cross.Street"))]
data_clean$DATE.OCC <- as.POSIXct(data_clean$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")

# Check if the conversion worked
str(data_clean$DATE.OCC)
head(data_clean$DATE.OCC)

# Extract the 'month' column in "YYYY-MM" format
data_clean$month <- format(data_clean$DATE.OCC, "%Y-%m")

# Check the output
head(data_clean$month)
# Check the cleaned data
nrow(data_clean)
str(data_clean)

# Plot 1: Top Locations with Highest Crime
ggplot(data_clean, aes(x = reorder(AREA.NAME, AREA.NAME, function(x) -length(x)))) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top Locations with Highest Crime", x = "Area", y = "Count")



# Plot 2: Crime Frequency Over Time (by Month)
ggplot(data_clean, aes(x = month)) +
  geom_bar(fill = "darkred") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Crime Frequency Over Time", x = "Month", y = "Count")

#Plot 3: Crimes by Descent Category
ggplot(data_clean, aes(x = reorder(Descent_Description, Descent_Description, function(x) -length(x)))) +
  geom_bar(fill = "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Crime Counts by Descent Description", x = "Descent Category", y = "Count")

#Plot 4: Frequency of Crime Type
# Calculate the frequency of each crime type
top_crime_types <- data_clean %>%
  count(Crm.Cd.Desc, sort = TRUE) %>%
  top_n(20, n)


ggplot(top_crime_types, aes(x = reorder(Crm.Cd.Desc, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 20 Most Frequent Crime Types", x = "Crime Type", y = "Count")

#Plot 5: Crime by descent by month
ggplot(data_clean, aes(x = month, fill = Descent_Description)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Monthly Crime Frequency by Descent", x = "Month", y = "Count")



