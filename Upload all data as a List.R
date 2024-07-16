# Load necessary libraries
library(readxl)

# Define the directory containing the Excel files
directory <- "C:/Users/user/OneDrive/Υπολογιστής/Repap - Network Analysis/Final Analysis/Data/Complete Datasets/Years [em]"

# List all Excel files in the directory
files <- list.files(directory, pattern = "*.xlsx", full.names = TRUE)

# Initialize an empty list to hold the data frames
data_list <- list()

# Loop through each file and read the data
for (file in files) {
  # Read the data from the current Excel file
  data <- read_excel(file)
  
  # Extract the base name of the file (without extension)
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Add the data frame to the list, using the file name as the list element name
  data_list[[file_name]] <- data
}

# Print the names of the loaded data frames
print(names(data_list))
