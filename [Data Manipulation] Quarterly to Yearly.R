# Step 1: Load necessary libraries
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(lubridate)  # For date manipulation
library(openxlsx)   # For writing Excel files

# Step 2: Define the file path for the input Excel file
file_path <- "C:/Users/user/OneDrive/Υπολογιστής/Repap - Network Analysis/Final Analysis/Data/Complete Datasets"

# Step 3: Read the Excel file into a dataframe
data <- read_excel(file.path(file_path, "EU.xlsx"))

# Step 4: Convert the 'period' column to Date type
data$period <- ymd(data$period)

# Step 5: Extract the year from the 'period' column
data$year <- year(data$period)

# Step 6: Group data by 'reporting_country', 'counterparty_country', and 'year', then sum the 'total_claims'
yearly_data <- data %>%
  group_by(reporting_country, counterparty_country, year) %>%
  summarise(total_claims = sum(total_claims, na.rm = TRUE))

# Step 7: Display the first few rows of the yearly aggregated data
print(head(yearly_data))

# Step 8: Define the file path for the output Excel file
output_path <- file.path(file_path, "Yearly_Aggregated_Data.xlsx")

# Step 9: Save the yearly aggregated data to a new Excel file
write.xlsx(yearly_data, output_path)
