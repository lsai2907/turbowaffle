# Set working directory
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(lavaan)
library(stringr)
library(qgraph)
library(semPlot)

# Load the data from Excel sheets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

# Identify a common key column to merge datasets
common_keys <- intersect(intersect(colnames(micr_cell_count), colnames(biomass_data)), colnames(soil_prop_data))
if (length(common_keys) == 0) {
  stop("No common key column found in all datasets. Please check your data.")
} else {
  common_key <- common_keys[1]  # Use the first matching key
}

# Handle duplicates: Keep the first unique entry
micr_cell_count <- micr_cell_count %>% distinct(!!sym(common_key), .keep_all = TRUE)
biomass_data <- biomass_data %>% distinct(!!sym(common_key), .keep_all = TRUE)
soil_prop_data <- soil_prop_data %>% distinct(!!sym(common_key), .keep_all = TRUE)

# Merge datasets using the common key
combined_data <- micr_cell_count %>%
  inner_join(biomass_data, by = common_key) %>%
  inner_join(soil_prop_data, by = common_key)

# Remove unwanted columns safely
cols_to_remove <- c(1, 9, 13, ncol(combined_data) - 1, ncol(combined_data))
cols_to_remove <- cols_to_remove[cols_to_remove <= ncol(combined_data)]
combined_data <- combined_data[, -cols_to_remove]

# Clean column names
colnames(combined_data) <- gsub("\\[.*?\\]", "", colnames(combined_data))
colnames(combined_data) <- str_trim(colnames(combined_data))
colnames(combined_data) <- gsub(" ", "_", colnames(combined_data))

# Standardize numeric variables
combined_data <- combined_data %>%
  mutate(across(where(is.numeric), scale))

measurement_model <- '
  OxygenInWater =~ Average_of_dissolvedOxygen + Average_of_dissolvedOxygenSaturation
  WaterProperties =~ Average_of_specificConductance + Average_of_waterTemp
  MicrobialDensity =~ Average_Cell_Density
  LipidMetrics =~ Average_of_totalLipidScaledConcentration + Average_of_lipidInternalStandardResponse
  SoilProperties =~ Average_of_soilTemp + Average_of_soilInWaterpH + Average_of_nitrogenPercent
'

structural_model <- '
  MicrobialDensity ~ OxygenInWater + WaterProperties
  LipidMetrics ~ MicrobialDensity
  SoilProperties ~ LipidMetrics + MicrobialDensity
'

sem_model <- paste(measurement_model, structural_model, sep = "\n")

# Fit SEM model without incorrect standardization argument
fit <- sem(sem_model, data = combined_data, missing = "ML")

# Print summary with proper standardization
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Load the semPlot library
library(semPlot)

# Adjust plot margins to make room for the legend at the bottom
par(mar = c(5, 5, 4, 8))  # Keep the top margin as it is, reduce the bottom margin for the legend

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(lavaan)
library(stringr)
library(qgraph)

# Your data and processing code here...

# Fit SEM model (assumed you've already done this step)
fit <- sem(sem_model, data = combined_data, missing = "ML")

# Plot the SEM model with standardized coefficients
semPaths(fit, 
         whatLabels = "std.all",  # Ensures full standardization of paths
         layout = "tree", 
         style = "ram", 
         label.cex = 1.2,  # Reduced label size
         title.cex = 1.2,  # Reduced title size
         edge.label.cex = 0.55)  # Reduced edge label size

# Set the global font family to serif
par(family = "serif")

# Adjust plot margins to make room for the legend at the bottom
par(mar = c(5, 5, 4, 8))  # Keep the top margin as it is, reduce the bottom margin for the legend

# Add a title with serif font
title(main = "Structural Equation Model of Relationships between Aquatic and Terrestrial Ecosystems", cex.main = 1.5, font.main = 4)

# Define legend descriptions
variable_legend <- c(
  "OxygenInWater" = "OIW = Dissolved Oxygen in Water (mg/L) and Saturation (%)",
  "WaterProperties" = "WtP = Water Conductivity (uS/cm) and Temperature (°C)",
  "MicrobialDensity" = "McD = Microbial Cell Density",
  "LipidMetrics" = "LpM = Lipid Concentration and Response",
  "SoilProperties" = "SlP = Soil Temperature (°C), pH, and Nitrogen (%)",
  "Av_O" = "Av_O = Average of Dissolved Oxygen (mg/L)",
  "Av_OS" = "Av_OS = Average of Dissolved Oxygen Saturation (%)",
  "A_C" = "A_C = Average of Conductivity (uS/cm)",
  "Avrg_f_wt" = "Avrg_f_wt = Average of Water Temperature (°C)",
  "A_L" = "A_L = Average of Total Lipid Scaled Concentration",
  "A_IS" = "A_IS = Average of Lipid Internal Standard Response",
  "Avrg_f_sT" = "Avrg_f_sT = Average of Soil Temperature (°C)",
  "A_IW" = "A_IW = Average of Soil in Water pH",
  "A_P" = "A_P = Average of Nitrogen Percentage (%)"
)

# Adjust the legend to fit in 2 columns
legend("bottom", 
       legend = variable_legend, 
       title = "Variable Descriptions", 
       col = "black", 
       pch = 10, 
       pt.cex = 1, 
       cex = 0.7, 
       box.lwd = 2,       # Line width of the box
       box.col = "white", # Box color
       bg = "white",      # Background color for the legend box
       text.width = 1,  # Adjust the width for text fitting (customize as needed)
       ncol = 2,          # Set number of columns in the legend
       xpd = TRUE)        # Allow moving the legend outside the plot area if needed

