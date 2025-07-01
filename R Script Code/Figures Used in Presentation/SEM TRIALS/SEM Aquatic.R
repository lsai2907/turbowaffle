rm(list = ls())

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

micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)

colnames(micr_cell_count)

colnames(micr_cell_count)[colnames(micr_cell_count) == "Row Labels"] <- "RowLabels"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average of totalCellCount [number]"] <- "totalCellCount"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average of cellCountSampleVolume [mL]"] <- "cellCountSampleVolume"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average Cell Density [number/mL]"] <- "cellDensity"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average of dissolvedOxygen [mg]"] <- "dissolvedOxygen"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average of dissolvedOxygenSaturation [%]"] <- "dissolvedOxygenSaturation"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average of specificConductance [microsiemens/cm]"] <- "specificConductance"
colnames(micr_cell_count)[colnames(micr_cell_count) == "Average of waterTemp [C]"] <- "waterTemp"

structural_model <- '
  cellDensity ~ totalCellCount + cellCountSampleVolume + dissolvedOxygen + specificConductance + waterTemp
  dissolvedOxygen ~ specificConductance + waterTemp
  dissolvedOxygenSaturation ~ dissolvedOxygen + specificConductance
'
# Fit the model
fit <- sem(structural_model, data = micr_cell_count)

# Summarize results
summary(fit, fit.measures = TRUE, standardized = TRUE)

library(dplyr)

micr_cell_count_scaled <- micr_cell_count %>%
  mutate(across(c(totalCellCount, cellCountSampleVolume, dissolvedOxygen, specificConductance, waterTemp, dissolvedOxygenSaturation, cellDensity), scale))

# Fit model on scaled data
fit_scaled <- sem(structural_model, data = micr_cell_count_scaled)

# Check summary and fit indices
summary(fit_scaled, fit.measures = TRUE, standardized = TRUE)

# Plot the SEM model with standardized coefficients
semPaths(fit_scaled, 
         whatLabels = "std.all",  # Ensures full standardization of paths
         layout = "circle3", 
         style = "ram", 
         label.cex = 1.7,  # Reduced label size
         title.cex = 3,  # Reduced title size
         edge.label.cex = 1.2)  # Reduced edge label size

# Set the global font family to serif
par(family = "serif")

# Add a title with serif font
title(main = "Structural Equation Model of Relationships within Aquatic Ecosystems", cex.main = 2.9, font.main = 6)

variable_legend <- c(
  "spC" = "spC = Specific Conductance (microsiemens/cm)",
  "dsO" = "dsO = Dissolved Oxygen (mg/L)",
  "dOS" = "dOS = Dissolved Oxygen Saturation (%)",
  "CID" = "CID = Cell Density (number/mL)",
  "wtT" = "wtT = Water Temperature (°C)",
  "cCS" = "cCS = Cell Count Sample Volume (mL)",
  "tCC" = "tCC = Total Cell Count (number)"
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
       text.width = 1,    # Adjust the width for text fitting (customize as needed)
       ncol = 2,          # Set number of columns in the legend
       xpd = TRUE)        # Allow moving the legend outside the plot area if needed

# Fit the SEM model
fit <- sem(structural_model, data = micr_cell_count_scaled)


# Summary with fit measures and standardized solution
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Then get the fit indices and R2
fit_indices <- fitMeasures(fit_scaled, c("cfi", "nfi", "df", "pvalue"))
r_squared <- inspect(fit_scaled, "r2")

print(fit_indices)
print(r_squared)

chi_square <- fitMeasures(fit_scaled, "chisq")
print(chi_square)

fitMeasures(fit_scaled, "df")
fitMeasures(fit_scaled, "pvalue")

N <- nrow(micr_cell_count_scaled)
print(N)

r_squared <- inspect(fit, "r2")
print(r_squared)
mean_r2 <- mean(r_squared)
print(mean_r2)

