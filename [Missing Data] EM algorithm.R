# Step 1: Install and load necessary packages
install.packages("readxl")    # For reading Excel files
install.packages("writexl")   # For writing Excel files
install.packages("dplyr")     # For data manipulation
install.packages("mice")      # For handling missing data using multiple imputation

library(readxl)    # Load readxl package
library(writexl)   # Load writexl package
library(dplyr)     # Load dplyr package
library(mice)      # Load mice package

# Step 2: Define a function to handle missing data using the Expectation Maximization (EM) algorithm
handle_missing_data_em <- function(df) {
  # Step 2.1: Impute missing data using the 'mice' function with the method 'norm' (normal distribution)
  imputed_data <- mice(df, method = 'norm', m = 1, maxit = 50, seed = 123)
  
  # Step 2.2: Extract the complete data from the imputed dataset
  complete_data <- complete(imputed_data, 1)
  
  # Step 2.3: Ensure all 'total_claims' values are non-negative
  complete_data$total_claims <- ifelse(complete_data$total_claims < 0, 0, complete_data$total_claims)
  
  # Step 2.4: Return the processed data
  return(complete_data)
}

# Step 3: Define the path to the Excel file containing the data
file_path <- "C:/Users/user/OneDrive/Υπολογιστής/Repap - Network Analysis/Final Analysis/Data/missing/aggregate.xlsx"

# Step 4: Retrieve all sheet names from the Excel file
sheet_names <- excel_sheets(file_path)

# Step 5: Initialize a list to store processed data for each sheet
processed_data <- list()

# Step 6: Loop through each sheet in the Excel file
for (sheet in sheet_names) {
  # Step 6.1: Read the data from the current sheet
  data <- read_excel(file_path, sheet = sheet)
  
  # Step 6.2: Check if the 'total_claims' column exists in the data
  if ("total_claims" %in% colnames(data)) {
    # Step 6.3: Apply the EM algorithm to handle missing data
    data <- handle_missing_data_em(data)
  }
  
  # Step 6.4: Store the processed data in the list
  processed_data[[sheet]] <- data
}

# Step 7: Write the processed data to a new Excel file
output_file <- "processed_aggregate_em2.xlsx"
write_xlsx(processed_data, output_file)

# Step 8: Print the path to the output file
print(paste("Processed file saved at:", output_file))
