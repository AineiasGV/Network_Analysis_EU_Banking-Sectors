# Load necessary libraries
library(readxl)
library(igraph)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Initialize a list to store summary statistics for each year
summary_stats <- list()

# Loop through each data frame in the list
for (file_name in names(data_list)) {
  # Load the data for the current file
  data <- data_list[[file_name]]
  
  # Extract the year from the file name
  year <- sub("EU_", "", file_name)
  
  # Create a graph from the data frame
  g <- graph_from_data_frame(d = data, directed = TRUE)
  
  # Set edge weights
  E(g)$weight <- data$total_claims
  
  # Ensure all weights are positive
  if (any(E(g)$weight <= 0)) {
    E(g)$weight <- E(g)$weight + abs(min(E(g)$weight)) + 1
  }
  
  # Calculate summary statistics
  avg_weighted_degree <- mean(strength(g, mode = "all", weights = E(g)$weight))
  
  avg_betweenness <- mean(betweenness(g, weights = E(g)$weight))
  
  # Calculate average path length and diameter for the largest connected component
  components <- clusters(g)
  largest_component <- which.max(components$csize)
  subgraph <- induced_subgraph(g, which(components$membership == largest_component))
  
  avg_path_length <- mean_distance(subgraph, directed = TRUE, weights = E(subgraph)$weight)
  diameter <- diameter(subgraph, directed = TRUE, weights = E(subgraph)$weight)
  
  # Store the summary statistics in a data frame
  summary_stats[[year]] <- data.frame(
    Year = year,
    Avg_Weighted_Degree = avg_weighted_degree,
    Avg_Betweenness = avg_betweenness,
    Avg_Path_Length = avg_path_length,
    Diameter = diameter
  )
}

# Combine all summary statistics into a single data frame
summary_stats_df <- bind_rows(summary_stats)

# Print the summary statistics
print(summary_stats_df)

# Create a ggplot table
p <- tableGrob(summary_stats_df)

# Save the table as an image
ggsave("Network_Summary_Statistics.png", plot = p, width = 12, height = 8, dpi = 300)
