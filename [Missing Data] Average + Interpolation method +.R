# Step 1: Install and load necessary packages
# Install required packages for data manipulation and handling
install.packages("readxl")
install.packages("writexl")
install.packages("dplyr")
install.packages("zoo")

# Load the installed libraries into the R session
library(readxl)
library(writexl)
library(dplyr)
library(zoo)

# Step 2: Define a function to handle missing data with different strategies
# This function takes a dataframe and a strategy as input and applies the chosen method to fill missing values
handle_missing_data <- function(df, strategy) {
  if (strategy == "ffill") {
    # Forward fill: Replace NA values by propagating the last non-NA value forward
    df$total_claims <- zoo::na.locf(df$total_claims, na.rm = FALSE)
  } else if (strategy == "bfill") {
    # Backward fill: Replace NA values by propagating the next non-NA value backward
    df$total_claims <- zoo::na.locf(df$total_claims, na.rm = FALSE, fromLast = TRUE)
  } else if (strategy == "linear") {
    # Linear interpolation: Estimate missing values by linearly interpolating between non-NA values
    df$total_claims <- zoo::na.approx(df$total_claims, na.rm = FALSE)
  } else if (strategy == "mean") {
    # Mean imputation: Replace NA values with the mean of non-NA values in the column
    mean_value <- mean(df$total_claims, na.rm = TRUE)
    df$total_claims[is.na(df$total_claims)] <- mean_value
  }
  return(df)
}

# Step 3: Specify the path to the Excel file containing data with missing values
file_path <- "C:/Users/user/OneDrive/Υπολογιστής/Repap - Network Analysis/Final Analysis/Data/missing/aggregate.xlsx"

# Step 4: Retrieve all sheet names from the Excel file
sheet_names <- excel_sheets(file_path)

# Step 5: Initialize an empty list to store processed data for each sheet
processed_data <- list()

# Step 6: Iterate through each sheet in the Excel file
for (sheet in sheet_names) {
  # Read the data from the current sheet
  data <- read_excel(file_path, sheet = sheet)
  
  # Step 7: Calculate the percentage of missing values in the 'total_claims' column
  missing_percentage <- sum(is.na(data$total_claims)) / nrow(data) * 100
  
  # Step 8: Apply an appropriate strategy based on the percentage of missing data
  if (missing_percentage <= 10) {
    # If less than or equal to 10% of data is missing, use forward fill
    data <- handle_missing_data(data, "ffill")
  } else if (missing_percentage <= 50) {
    # If 10-50% of data is missing, use linear interpolation
    data <- handle_missing_data(data, "linear")
  } else {
    # If more than 50% of data is missing, use mean imputation
    data <- handle_missing_data(data, "mean")
  }
  
  # Step 9: Additionally, apply backward fill to address any initial missing values
  data <- handle_missing_data(data, "bfill")
  
  # Step 10: Store the processed data in the list
  processed_data[[sheet]] <- data
}

# Step 11: Write the processed data from all sheets into a new Excel file
write_xlsx(processed_data, "processed_aggregate.xlsx")

# Step 12: Print the path to the output file
output_file <- "processed_aggregate.xlsx"
print(paste("Processed file saved at:", output_file))
