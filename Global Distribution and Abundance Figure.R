# Load libraries
library(ggplot2)     # For plotting
library(sf)          # For spatial data handling
library(ggspatial)   # For better map visuals
library(scatterpie)  # For adding pie charts to a map
library(maps)        # For world map data
library(readxl)      # For reading Excel files (if needed)

# Step 1: Prepare the base map
# Load world shapefile data
world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Step 2: Prepare data for pie charts
# Example data with regions, coordinates, and pie chart values
pie_data <- data.frame(
  region = c(
    "ARIK", "BIGC", "BLUE", "CARI", "COMO", "CUPE", "GUIL", "KING", "LECO", "LEWI",
    "MART", "MAYF", "MCDI", "MCRA", "POSE", "STCU", "SYCA", "WALK", "WLOU", "ABBY",
    "BARR", "BART", "BLAN", "BONA", "CLBJ", "CPER", "DCFS", "DEJU", "DELA", "DSNY",
    "GRSM", "GUAN", "HARV", "HEAL", "JERC", "JORN", "KONA", "KONZ", "LAJA", "LENO",
    "MLBS", "MOAB", "NIWO", "NOGP", "OAES", "ORNL", "OSBS", "RMNP", "SCBI", "SERC",
    "SJER", "SOAP", "STEI", "STER", "TALL", "TOOL", "TREE", "UKFS", "UNDE", "WOOD",
    "WREF", "YELL", "SRER", "BARC", "BLDE", "BLWA", "CRAM", "FLNT", "HOPB", "LIRO",
    "OKSR", "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "TOMB", "TOOK", "ONAQ"),
  Longitude = c(
    -102.44715, -119.25755, -96.6242, -147.50397, -105.54416, -66.98676, -66.79868, -96.60383, 
    -83.50379, -77.98322, -121.93379, -87.40769, -96.44302, -122.16555, -78.14726, -89.58637, 
    -111.50809, -84.27925, -105.9154, -122.33032, -156.61936, -71.28737, -78.04179, -147.50258, 
    -97.57, -104.74559, -99.10656, -145.75136, -87.80388, -81.43619, -83.50195, -66.8687, 
    -72.17265, -149.21335, -84.46862, -106.84254, -96.61293, -96.56307, -67.07689, -88.16118, 
    -80.52485, -109.38827, -105.58237, -100.91535, -99.05878, -84.28259, -81.99343, -105.54596, 
    -78.13949, -76.56001, -119.73228, -119.26219, -89.58637, -103.02929, -87.39326, -149.37047, 
    -89.58571, -95.19215, -89.53725, -99.24133, -121.95191, -110.53914, -110.83549, -82.00841, 
    -110.58715, -87.79815, -89.47369, -84.4374, -72.32953, -89.70477, -149.14302, -97.78231, 
    -99.11388, -99.25315, -111.79789, -82.01775, -88.15887, -149.61064, -112.45245
  ),  # Longitude
  Latitude = c(
    39.75821, 37.05972, 34.44422, 65.15322, 40.03496, 18.11352, 18.17406, 39.10506, 
    35.69043, 39.09564, 45.79084, 32.96037, 38.94586, 44.2596, 38.89431, 45.50894, 
    33.75099, 35.95738, 39.89137, 45.76244, 71.28241, 44.06389, 39.0337, 65.15401, 
    33.40123, 40.81554, 47.16165, 63.88112, 32.54173, 28.12505, 35.68896, 17.96955, 
    42.53691, 63.8758, 31.19484, 32.59069, 39.11045, 39.10077, 18.02126, 31.85386, 
    37.37831, 38.24828, 40.05425, 46.76972, 35.4106, 35.96413, 29.68928, 40.2759, 
    38.89292, 38.89013, 37.10878, 37.03337, 45.50894, 40.46189, 32.95047, 68.66109, 
    45.49369, 39.04043, 46.23391, 47.1282, 45.82049, 44.95348, 31.91068, 29.67598, 
    44.95011, 32.54153, 46.20967, 31.18542, 42.47194, 45.99827, 68.66975, 33.37852, 
    47.15909, 47.12984, 40.78393, 29.68778, 31.85343, 68.63069, 40.177599
  ),   # Latitude
  Archaea = c(
    1, 0.5, 0, 1.5, 1.5, 1, 1.5, 0.5, 0, 0.5, 0, 0.5, 0, 1, 0.5, 0, 0, 1, 0.5, 1, 
    5, 1, 0, 0, 0, 0, 0, 0, 1, 2, 1, 0, 1, 0, 0, 0, 1, 1, 1, 2, 1, 0, 0, 1, 1, 0, 
    1, 0, 0, 0, 0, 2, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0.5, 0, 1.5, 
    0, 0, 0, 0.5, 0, 0, 0, 0
  ),      
  Bacteria = c(
    50.5, 64, 57.5, 75.5, 49.5, 82, 70, 48.5, 77, 40.5, 55, 66.5, 48, 71, 54, 82, 
    28, 71, 52.5, 64, 85, 63, 87, 68, 79, 81, 73, 57, 72, 62,     44, 61, 63, 78, 73, 88, 74, 78, 76, 57, 44, 70, 65, 80, 81, 62, 59, 56, 82, 82, 
    66, 51, 77, 84, 69, 84, 70, 72, 76, 82, 67, 72, 67, 61, 41, 74, 66, 83, 52.5, 66, 
    81, 45, 75, 77, 59, 59, 76, 67, 79
  ),
  Eukarya = c(
    12.5, 7, 19.5, 2.5, 33.5, 2, 2, 7, 12.5, 18.5, 15, 20.5, 5, 9, 17, 0, 15, 16.5, 
    32, 32, 8, 31, 9, 30, 16, 16, 21, 41, 18, 21, 46, 25, 33, 19, 23, 11, 20, 15, 
    10, 32, 48, 27, 32, 15, 14, 33, 26, 41, 13, 14, 31, 44, 19, 14, 26, 14, 26, 21, 
    20, 13, 29, 25, 29, 3, 16, 0, 1, 7, 22, 1, 3.5, 7, 0, 23, 11.5, 2, 24, 3, 18
  ),
  Unclassified = c(
    36, 28.5, 23, 20.5, 15.5, 15, 26.5, 44, 10.5, 40.5, 30, 12.5, 47, 19, 28.5, 18, 
    57, 11.5, 15, 3, 2, 5, 4, 2, 5, 3, 6, 2, 9, 15, 9, 14, 3, 3, 4, 1, 5, 6, 13, 9, 
    7, 3, 3, 4, 4, 5, 14, 3, 5, 4, 3, 3, 3, 2, 4, 2, 3, 6, 3, 5, 3, 2, 4, 36, 43, 
    26, 33, 10, 24.5, 33, 14, 48, 25, 0, 29, 39, 0, 30, 3
  )
)

# Step 3: Plot the map with pie charts
ggplot(data = world) +
  # Plot the world map
  geom_sf(fill = "lightblue", color = "white") +
  
  # Overlay pie charts
  geom_scatterpie(data = pie_data, 
                  aes(x = Longitude, y = Latitude, group = region), 
                  cols = c("Archaea", "Bacteria", "Eukarya", "Unclassified"), 
                  pie_scale = 0.5) +  # Adjust pie chart size
  
  # Optional: Add labels for each region
  geom_text(data = pie_data, 
            aes(x = Longitude, y = Latitude, label = region),
            color = "Black",
            size = 3, vjust = -3, family="serif") +
  
  # Add a title and minimal theme
  labs(title = "Abundance Percentage on Earth",
       subtitle = "Demonstrating Regional Abundance Distributions",
       fill = "Domain") + # Changed the legend title
  coord_sf() + 
  coord_sf(
    xlim = c(-160, -60),  # Longitude range (Western Hemisphere)
    ylim = c(20, 72)       # Latitude range (Northern Hemisphere only)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),   # Apply font globally
    plot.title = element_text(size = 50, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 20)
  )

#-----------------------------------------------------------------------------------------------------------------------------

# Perform Independent T-Tests
# Specify domains to compare
domain_pairs <- combn(c("Archaea", "Bacteria", "Eukarya", "Unclassified"), 2, simplify = FALSE)

# Initialize a results dataframe
t_test_results <- data.frame(
  Comparison = character(),
  Mean_Difference = numeric(),
  t_Statistic = numeric(),
  df = numeric(),
  p_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each pair of domains
for (pair in domain_pairs) {
  # Extract the domain names
  domain1 <- pair[1]
  domain2 <- pair[2]
  
  # Perform the independent t-test
  test_result <- t.test(pie_data[[domain1]], pie_data[[domain2]])
  
  # Store the results
  t_test_results <- rbind(
    t_test_results,
    data.frame(
      Comparison = paste(domain1, "vs", domain2),
      Mean_Difference = mean(pie_data[[domain1]]) - mean(pie_data[[domain2]]),
      t_Statistic = test_result$statistic,
      df = test_result$parameter,
      p_Value = test_result$p.value
    )
  )
}

# Display the results
print(t_test_results)

# Optionally: Save results to CSV
write.csv(t_test_results, "independent_t_test_results.csv", row.names = FALSE)

    