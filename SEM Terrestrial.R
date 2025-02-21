# Set working directory
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load necessary libraries
library(readxl)
library(dplyr)
library(readxl)
library(ggplot2)
library(dplyr)
library(lavaan)
library(stringr)
library(qgraph)
library(semPlot)

# Read in data
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)

# Check column names in both datasets
colnames(soil_prop_data)
colnames(biomass_data)

# Merge datasets based on a common column (e.g., "SampleID" or "Site")
combined_data <- left_join(soil_prop_data, biomass_data, by = "Row Labels")

# Check the first few rows
head(combined_data)

colnames(combined_data)

# Rename columns if necessary
colnames(combined_data)[colnames(combined_data) == "Average of soilTemp [Degree]"] <- "soilTemp"
colnames(combined_data)[colnames(combined_data) == "Average of soilInWaterpH [pH]"] <- "soilInWaterpH"
colnames(combined_data)[colnames(combined_data) == "Average of nitrogenPercent [%]"] <- "nitrogenPercent"
colnames(combined_data)[colnames(combined_data) == "Average of organicCPercent [%]"] <- "carbonPercent"
colnames(combined_data)[colnames(combined_data) == "Average of soilMoisture [Gram/Gram]"] <- "soilMoisture"
colnames(combined_data)[colnames(combined_data) == "Average of soilFreshMass [Gram]"] <- "soilFreshMass"
colnames(combined_data)[colnames(combined_data) == "Average of freezeDryMass [Gram]"] <- "freezeDryMass"
colnames(combined_data)[colnames(combined_data) == "Average of totalLipidScaledConcentration [nanomolesPerGram]"] <- "LipidScaledConcentration"
colnames(combined_data)[colnames(combined_data) == "Average of lipidInternalStandardResponse [2000picoAmpSecond]"] <- "lipidInternalStandardResponse"

structural_model <- '
  soilMoisture ~ soilTemp + soilInWaterpH + nitrogenPercent + carbonPercent
  soilFreshMass ~ soilMoisture + soilTemp + nitrogenPercent
  freezeDryMass ~ soilFreshMass + soilMoisture
  LipidScaledConcentration ~ freezeDryMass + carbonPercent
  lipidInternalStandardResponse ~ LipidScaledConcentration + nitrogenPercent
'

# Fit the model
fit <- sem(structural_model, data = combined_data)

# Summarize results
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Set the global font family to serif
par(family = "serif")

# Plot the SEM model with standardized coefficients
semPaths(fit, 
         whatLabels = "std.all",  # Ensures full standardization of paths
         layout = "circle2", 
         style = "ram", 
         label.cex = 1.5,  # Reduced label size
         title.cex = 2.3,  # Reduced title size
         edge.label.cex = 1.1)  # Reduced edge label size

# Add a title with serif font
title(main = "Structural Equation Model of Relationships within Terrestrial Ecosystems", cex.main = 2.5, font.main = 6)

variable_legend <- c(
  "sIT" = "sIT = Soil Temperature (°C)",
  "sIW" = "sIW = Soil In-Water pH",
  "ntP" = "ntP = Nitrogen Percent (%)",
  "crP" = "crP = Carbon Percent (%)",
  "sIM" = "sIM = Soil Moisture (Gram/Gram)",
  "sFM" = "sFM = Soil Fresh Mass (Gram)",
  "fDM" = "fDM = Freeze Dry Mass (Gram)",
  "LSC" = "LSC = Lipid Scaled Concentration (nanomolesPerGram)",
  "IIS" = "IIS = Lipid Internal Standard Response (2000 picoAmpSecond)"
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

