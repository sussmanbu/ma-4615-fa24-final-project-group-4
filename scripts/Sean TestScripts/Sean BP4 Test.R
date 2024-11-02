
library(ggplot2)

data <- readRDS(here("filtered_data_2.rds"))

filtered_data <- subset(data, Descent_Description != "Unknown" & Descent_Description != "Other")

# Create a contingency table for the filtered data
crime_table_filtered <- table(filtered_data$Descent_Description, filtered_data$Crm.Cd.Desc)

chi_test_filtered <- chisq.test(crime_table_filtered)
print(chi_test_filtered)
# Extract standardized residuals
standardized_residuals_filtered <- chi_test_filtered$stdres

top_crimes <- names(sort(colSums(crime_table_filtered), decreasing = TRUE))[1:20]

# Filter the contingency table to only include top crime types
crime_table_top <- crime_table_filtered[, top_crimes]

chi_test_top <- chisq.test(crime_table_top)
standardized_residuals_top <- chi_test_top$stdres


residuals_top_df <- as.data.frame(as.table(standardized_residuals_top))
colnames(residuals_top_df) <- c("Descent", "Crime_Type", "Residual")



ggplot(residuals_top_df, aes(x = Crime_Type, y = Descent, fill = Residual)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Standardized Residuals of Descent and Top Crime Types",
       x = "Crime Type", y = "Descent Category", fill = "Residual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


