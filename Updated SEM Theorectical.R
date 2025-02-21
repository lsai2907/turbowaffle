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
library(openxlsx)

# Load datasets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

# Rename site columns for consistency
colnames(micr_cell_count)[1] <- "Row Labels"
colnames(biomass_data)[1] <- "Row Labels"
colnames(soil_prop_data)[1] <- "Row Labels"

# Drop unnecessary rows (if applicable)
biomass_data <- biomass_data %>% filter(!is.na(`Row Labels`)) %>% slice(-1)
soil_prop_data <- soil_prop_data %>% filter(!is.na(`Row Labels`))

# Convert site names to uppercase and trim spaces
micr_cell_count$Site <- trimws(toupper(micr_cell_count$Site))
biomass_data$Site <- trimws(toupper(biomass_data$Site))
soil_prop_data$Site <- trimws(toupper(soil_prop_data$Site))

# Create a full list of unique site names
all_sites <- unique(c(micr_cell_count$Site, biomass_data$Site, soil_prop_data$Site))

# Create a data frame with all sites
aligned_data <- data.frame(Site = all_sites)

# Merge all datasets on "Site"
aligned_data <- aligned_data %>%
  left_join(micr_cell_count, by = "Site") %>%
  left_join(biomass_data, by = "Site") %>%
  left_join(soil_prop_data, by = "Site")

# Check column names after merge
colnames(aligned_data)

# Clean column names by removing spaces and special characters
colnames(aligned_data) <- gsub("[[:space:][:punct:]]", "", colnames(aligned_data))

# Rename columns if necessary
colnames(aligned_data)[colnames(aligned_data) == "Average of dissolvedOxygen [mg]"] <- "AverageofdissolvedOxygen"
colnames(aligned_data)[colnames(aligned_data) == "Average of dissolvedOxygenSaturation [%]"] <- "AverageofdissolvedOxygenSaturation"
colnames(aligned_data)[colnames(aligned_data) == "Average of specificConductance [microsiemens/cm]"] <- "AverageofspecificConductance"
colnames(aligned_data)[colnames(aligned_data) == "Average of waterTemp [C]"] <- "AverageofwaterTemp"
colnames(aligned_data)[colnames(aligned_data) == "Average Cell Density [number/mL]"] <- "AverageCellDensity"
colnames(aligned_data)[colnames(aligned_data) == "Average of lipidInternalStandardResponse"] <- "AverageoflipidInternalStandardResponse"
colnames(aligned_data)[colnames(aligned_data) == "Average of soilTemp [Degree]"] <- "AverageofsoilTemp"
colnames(aligned_data)[colnames(aligned_data) == "Average of soilInWaterpH [pH]"] <- "AverageofsoilInWaterpH"
colnames(aligned_data)[colnames(aligned_data) == "Average of nitrogenPercent [%]"] <- "AverageofnitrogenPercent"

# Verify if the necessary columns are present in the combined data
required_columns <- c(
  "AverageofdissolvedOxygen", 
  "AverageofdissolvedOxygenSaturation", 
  "AverageofspecificConductance", 
  "AverageofwaterTemp", 
  "AverageCellDensity", 
  "AverageoflipidInternalStandardResponse", 
  "AverageofsoilTemp", 
  "AverageofsoilInWaterpH", 
  "AverageofnitrogenPercent"
)

# Check if the required columns are available
missing_columns <- setdiff(required_columns, colnames(aligned_data))
if (length(missing_columns) > 0) {
  print("Missing columns in the dataset:")
  print(missing_columns)
} else {
  print("All required columns are present.")
}

# Standardize numeric variables (scale all numeric columns)
aligned_data <- aligned_data %>%
  mutate(across(where(is.numeric), scale))

# Define your measurement model for SEM with the exact column names
structural_model <- '
  OxygenInWater =~ `AverageofdissolvedOxygen` + `AverageofdissolvedOxygenSaturation`
  WaterProperties =~ `AverageofspecificConductance` + `AverageofwaterTemp`
  MicrobialDensity =~ `AverageCellDensity`
  LipidMetrics =~ `AverageoflipidInternalStandardResponse`
  SoilProperties =~ `AverageofsoilTemp` + `AverageofsoilInWaterpH` + `AverageofnitrogenPercent`
  MicrobialDensity ~ OxygenInWater + WaterProperties
  LipidMetrics ~ MicrobialDensity
  SoilProperties ~ LipidMetrics + MicrobialDensity
'

# Combine measurement and structural models
sem_model <- paste(structural_model, sep = "\n")

# Fit SEM model
fit <- sem(sem_model, data = aligned_data, missing = "ML")

# Print SEM model summary with fit measures and standardized values
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Adjust plot margins to make room for the legend at the bottom
par(mar = c(5, 5, 4, 8))  # Keep the top margin as it is, reduce the bottom margin for the legend

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

# Add a title with serif font
title(main = "Structural Equation Model of Relationships between Aquatic and Terrestrial Ecosystems", cex.main = 1.5, font.main = 4)

variable_legend <- c(
  "AvO" = "AvO = Average of Dissolved Oxygen (mg/L)",
  "AOS" = "AOS = Average of Dissolved Oxygen Saturation (%)",
  "AvC" = "AvC = Average of Conductivity (uS/cm)",
  "ATC" = "ATC = Average of Water Temperature (°C)",
  "OIW" = "OIW = Oxygen in Water (mg/L and Saturation %)",
  "WtP" = "WtP = Water Conductivity (uS/cm) and Temperature (°C)",
  "McD" = "McD = Microbial Cell Density",
  "LpM" = "LpM = Lipid Concentration and Response",
  "S1P" = "S1P = Soil Temperature (°C), pH, and Nitrogen (%)",
  "ACD" = "ACD = Average Cell Density (number/mL)",
  "AIS" = "AIS = Average of Lipid Internal Standard Response",
  "ATD" = "ATD = Average of Soil Temperature (°C)",
  "AIW" = "AIW = Average of Soil in Water pH",
  "AvP" = "AvP = Average of Nitrogen Percentage (%)"
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
