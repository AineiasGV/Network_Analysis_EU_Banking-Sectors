# Step 1: Load necessary libraries
library(readxl)
library(igraph)
library(dplyr)
library(ggraph)
library(ggplot2)
library(viridis)

# Step 2: Loop through each data frame in the list
for (file_name in names(data_list)) {
  
  # Step 3: Load the data for the current file
  data <- data_list[[file_name]]
  
  # Step 4: Extract the year from the file name
  year <- sub("EU_", "", file_name)
  
  # Step 5: Create a graph from the data frame
  g <- graph_from_data_frame(d = data, directed = TRUE)
  
  # Step 6: Set edge weights based on total claims
  E(g)$weight <- data$total_claims
  
  # Step 7: Calculate weighted degree centrality
  in_degree_centrality <- strength(g, mode = "in", weights = E(g)$weight)
  out_degree_centrality <- strength(g, mode = "out", weights = E(g)$weight)
  total_degree_centrality <- in_degree_centrality + out_degree_centrality
  V(g)$centrality <- total_degree_centrality
  
  # Step 8: Print country names from the graph to ensure we have all coordinates
  print(V(g)$name)
  
  # Step 9: Define geographical coordinates for each EU country
  coords <- data.frame(
    name = c("AT:Austria", "BE:Belgium", "CY:Cyprus", "DE:Germany", "DK:Denmark", "EE:Estonia", "ES:Spain", "FI:Finland", "FR:France", 
             "GR:Greece", "HR:Croatia", "IE:Ireland", "IT:Italy", "LT:Lithuania", "LU:Luxembourg", "LV:Latvia", "MT:Malta", 
             "NL:Netherlands", "PT:Portugal", "SE:Sweden", "SI:Slovenia", "SK:Slovakia", "GB:United Kingdom"),
    latitude = c(47.5162, 50.8503, 35.1264, 51.1657, 56.2639, 58.5953, 40.4637, 61.9241, 46.6034, 39.0742, 45.1, 53.1424, 41.8719, 
                 55.1694, 49.8153, 56.8796, 35.9375, 52.1326, 39.3999, 60.1282, 46.1512, 48.669, 55.3781),
    longitude = c(14.5501, 4.3517, 33.4299, 10.4515, 9.5018, 25.0136, -3.7492, 25.7482, 2.2137, 21.8243, 15.2, -7.6921, 12.5674, 
                  23.8813, 6.1296, 24.6032, 14.3754, 5.2913, -8.2245, 18.6435, 14.9955, 19.699, -3.436)
  )
  
  # Step 10: Map coordinates to the nodes in the graph
  V(g)$latitude <- coords$latitude[match(V(g)$name, coords$name)]
  V(g)$longitude <- coords$longitude[match(V(g)$name, coords$name)]
  
  # Step 11: Check for any missing coordinates and stop if any are missing
  if (any(is.na(V(g)$latitude)) || any(is.na(V(g)$longitude))) {
    missing_countries <- V(g)$name[is.na(V(g)$latitude) | is.na(V(g)$longitude)]
    stop(paste("Some countries' coordinates are missing or mismatched:", paste(missing_countries, collapse = ", ")))
  }
  
  # Step 12: Create a layout matrix based on geographical coordinates
  layout <- as.matrix(data.frame(x = V(g)$longitude, y = V(g)$latitude))
  
  # Step 13: Plot the graph with geographical layout using ggraph
  p <- ggraph(g, layout = layout) + 
    geom_edge_link(aes(width = weight), edge_colour = 'grey') +
    geom_node_point(aes(color = centrality), size = 6) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3, color = 'black') +
    scale_color_viridis(option = "D", direction = -1) + # Use viridis color scale
    theme_void() +
    scale_edge_width(range = c(0.2, 2)) +
    labs(title = paste("Banking Network of EU+ Countries (", year, ")")) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, hjust = 0.5, color = "black", face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_fixed(ratio = 1)  # Adjust aspect ratio to match map proportions
  
  # Step 14: Save the plot with higher resolution and correct file name
  ggsave(paste0("Geographical_EU_Banking_Network_", year, ".png"), plot = p, width = 12, height = 8, dpi = 300)
}
