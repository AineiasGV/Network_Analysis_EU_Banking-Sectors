# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Step 1: Read the data from the Excel file
data <- EU

# Step 2: Data Preparation
# Assuming the data has columns: Year, Reporting_Country, Counterparty_Country, total_claims
# Filter the data for the years 2006 to 2023
data_filtered <- data %>%
  filter(year >= 2006 & year <= 2023)

# Remove negative values
data_filtered <- data_filtered %>%
  filter(total_claims >= 0)

# Step 3: Summary Statistics
# Calculate summary statistics for each country
summary_stats <- data_filtered %>%
  group_by(reporting_country) %>%
  summarize(
    Total = sum(total_claims, na.rm = TRUE),
    Mean = mean(total_claims, na.rm = TRUE),
    Median = median(total_claims, na.rm = TRUE),
    Std_Dev = sd(total_claims, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert the summary statistics to a data frame
summary_stats_df <- as.data.frame(summary_stats)

# Step 4: Print the table as an image
# Create a table plot using ggplot2 and gridExtra
table_plot <- tableGrob(summary_stats_df)

# Save the plot as an image
png("EU_Banking_Summary_Statistics_2006_2023__.png", width = 1200, height = 600)
grid.draw(table_plot)
dev.off()
