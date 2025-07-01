# Clear environment and set working directory
rm(list = ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lavaan)
library(stringr)
library(qgraph)
library(semPlot)
library(tidyr)

# Load soil and biomass datasets
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)

# 🔻 Load and extract biomass from columns 19 and 20 of the actual biomass file
biomass_actual <- read_excel("Soil Microbe Biomass Calc.xlsx", sheet = 1)

biomass_actual_subset <- biomass_actual %>%
  select(`Row Labels...19`, `BIOMASS [mmol C /kg]`) %>%
  rename(
    `Row Labels` = `Row Labels...19`,
    Biomass = `BIOMASS [mmol C /kg]`
  )

# Summarize soil properties by site
soil_prop_summary <- soil_prop_data %>%
  group_by(`Row Labels`) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Summarize original biomass data by site
biomass_summary <- biomass_data %>%
  group_by(`Row Labels`) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Merge all datasets using "Row Labels"
combined_data <- soil_prop_summary %>%
  left_join(biomass_summary, by = "Row Labels") %>%
  left_join(biomass_actual_subset, by = "Row Labels")

# Drop rows with missing values
combined_data_clean <- combined_data %>% drop_na()

# Rename variables for clarity and drop lipid variables
combined_data_clean <- combined_data_clean %>%
  rename(
    soilTemp = `Average of soilTemp [Degree]`,
    soilMoisture = `Average of soilMoisture [Gram/Gram]`,
    soilInWaterpH = `Average of soilInWaterpH [pH]`,
    nitrogenPercent = `Average of nitrogenPercent [%]`,
    carbonPercent = `Average of organicCPercent [%]`,
    soilFreshMass = `Average of soilFreshMass [Gram]`,
    freezeDryMass = `Average of freezeDryMass [Gram]`
  ) %>%
  select(-starts_with("Average of totalLipidScaledConcentration"),
         -starts_with("Average of lipidInternalStandardResponse"))

# 🔁 SEM model: Biomass as central mediator
structural_model <- '
   Biomass ~ soilMoisture + carbonPercent + soilTemp + nitrogenPercent
  soilFreshMass ~ Biomass + soilInWaterpH
  
'

# Fit SEM
fit <- sem(structural_model, data = combined_data_clean, estimator = "ML", se = "standard")

# Print SEM summary
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# SEM path diagram
par(family = "serif")
semPaths(fit,
         whatLabels = "std.all",
         layout = "tree",
         style = "ram",
         label.cex = 1.5,
         title.cex = 2.3,
         edge.label.cex = 1.1)
title(main = "SEM with Biomass as Central Variable (No Lipid Variables)", cex.main = 2.5, font.main = 6)

# Legend for SEM variables
variable_legend <- c(
  "soilTemp" = "Soil Temperature (°C)",
  "soilInWaterpH" = "Soil In-Water pH",
  "nitrogenPercent" = "Nitrogen (%)",
  "carbonPercent" = "Organic Carbon (%)",
  "soilMoisture" = "Soil Moisture (g/g)",
  "soilFreshMass" = "Soil Fresh Mass (g)",
  "freezeDryMass" = "Freeze-Dry Mass (g)",
  "Biomass" = "Measured Biomass (mmol C/kg)"
)

legend("bottom",
       legend = variable_legend,
       title = "Variable Descriptions",
       col = "black",
       pch = 10,
       pt.cex = 1,
       cex = 0.7,
       box.lwd = 2,
       box.col = "white",
       bg = "white",
       text.width = 1,
       ncol = 2,
       xpd = TRUE)

# Print diagnostics
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(fit, c("cfi", "nfi", "chisq", "df", "pvalue"))

# Sample size
n_obs <- nobs(fit)
cat("Sample size (N):", n_obs, "\n")

# R-squared values
inspect(fit, "r2")
cat("Average R-squared:", mean(unlist(inspect(fit, "r2"))), "\n")


