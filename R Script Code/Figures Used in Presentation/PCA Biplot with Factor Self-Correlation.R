# Setting the working directory
rm(list = ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")
#==============================PCA

# Load necessary libraries
library(tidyverse)
library(factoextra)
library(ggplot2)
library(readxl)
library(dplyr)

# Load datasets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

# Get common row names to align datasets
common_rows <- Reduce(intersect, list(
  rownames(micr_cell_count),
  rownames(biomass_data),
  rownames(soil_prop_data)
))

micr_cell_count <- micr_cell_count[common_rows, ]
biomass_data <- biomass_data[common_rows, ]
soil_prop_data <- soil_prop_data[common_rows, ]

# Combine datasets
combined_data_terrestrial <- cbind(biomass_data, soil_prop_data)
combined_data_terrestrial_r <- combined_data_terrestrial %>%
  select(-c(1, 5))


combined_data_aquatic <-cbind(micr_cell_count)
combined_data_aquatic_r <- combined_data_aquatic %>%
  select(-c(1))


# Remove rows with missing values or infinite values
combined_data_terrestrial_cut <- combined_data_terrestrial_r[complete.cases(combined_data_terrestrial_r) & !apply(combined_data_terrestrial_r, 1, function(row) any(row == Inf | row == -Inf)), ]
combined_data_aquatic_cut <- combined_data_aquatic_r[complete.cases(combined_data_aquatic_r) & !apply(combined_data_aquatic_r, 1, function(row) any(row == Inf | row == -Inf)), ]

# Perform PCA
pca_result_terrestrial_cut <- prcomp(combined_data_terrestrial_cut, scale. = TRUE)
pca_result_aquatic_cut <- prcomp(combined_data_aquatic_cut, scale. = TRUE)

# Create PCA data for plotting
pca_data_terrestrial_cut <- as.data.frame(pca_result_terrestrial_cut$x)
pca_vars_terrestrial_cut <- as.data.frame(pca_result_terrestrial_cut$rotation)

pca_data_aquatic_cut <- as.data.frame(pca_result_aquatic_cut$x)
pca_vars_aquatic_cut <- as.data.frame(pca_result_aquatic_cut$rotation)

# Add row names to pca_data for labels
pca_data_terrestrial_cut$Label <- rownames(combined_data_terrestrial_cut)

pca_data_aquatic_cut$Label <- rownames(combined_data_aquatic_cut)

# Assign factor groups (Example: Customize this for your dataset)
pca_data_terrestrial_cut$Group <- apply(combined_data_terrestrial_cut, 1, function(row) {
  colnames(combined_data_terrestrial_cut)[which.max(row)]
})

pca_data_aquatic_cut$Group <- apply(combined_data_aquatic_cut, 1, function(row) {
  colnames(combined_data_aquatic_cut)[which.max(row)]
})

#--------------------------------------------------------------------------------------------------------------------------------------
# Remove specific factors from the variable loadings
exclude_factors_terrestrial_cut <- c("Average of freezeDryMass [Gram]", "Average of totalLipidScaledConcentration [nanomolesPerGram]", "Average of lipidInternalStandardResponse [2000picoAmpSecond]")
pca_vars_terrestrial_cut <- pca_vars_terrestrial_cut[!rownames(pca_vars_terrestrial_cut) %in% exclude_factors_terrestrial_cut, ]
pca_vars_terrestrial_cut$Group <- rownames(pca_vars_terrestrial_cut) # Each variable is its own group

exclude_factors_aquatic_cut <- c()
pca_vars_factors_aquatic_cut <- pca_vars_aquatic_cut[!rownames(pca_vars_aquatic_cut) %in% exclude_factors_aquatic_cut, ]
pca_vars_factors_aquatic_cut$Group <- rownames(pca_vars_factors_aquatic_cut) # Each variable is its own group

# Custom name mappings (Replace this with your own mapping)
custom_labels_terrestrial <- c(
  "Average of freezeDryMass [Gram]" = "Freeze Dry Mass",
  "Average of totalLipidScaledConcentration [nanomolesPerGram]" = "Lipid Scaled Concentration",
  "Average of lipidInternalStandardResponse [2000picoAmpSecond]" = "Lipid Internal Standard Response",
  "Average of soilTemp [Degree]" = "Soil Temp",
  "Average of soilMoisture [Gram/Gram]" = "Soil Moisture",
  "Average of soilInWaterpH [pH]" = "Soil pH",
  "Average of nitrogenPercent [%]" = "Nitrogen %",
  "Average of organicCPercent [%]" = "Organic C %",
  "Average of soilFreshMass [Gram]" = "Soil Fresh Mass"
)

custom_labels_aquatic <- c(
  "Average of totalCellCount [number]" = "Total Cell Count",
  "Average of cellCountSampleVolume [mL]" = "Sample Volume (mL)",
  "Average Cell Density [number/mL]" = "Cell Density (#/mL)",
  "Average of dissolvedOxygen [mg]" = "Dissolved O2 (mg/L)",
  "Average of dissolvedOxygenSaturation [%]" = "O2 Saturation (%)",
  "Average of specificConductance [microsiemens/cm]" = "Conductivity (µS/cm)",
  "Average of waterTemp [C]" = "Water Temperature (°C)"
)


# Apply the custom names to the pca_data$Label
pca_data_terrestrial_cut$Label <- recode(pca_data_terrestrial_cut$Label, !!!custom_labels_terrestrial)

pca_data_aquatic_cut$Label <- recode(pca_data_aquatic_cut$Label, !!!custom_labels_aquatic)

# Apply the custom names to the pca_vars rownames
# Only recode if the rownames are found in the custom_labels mapping
rownames(pca_vars_terrestrial_cut) <- recode(rownames(pca_vars_terrestrial_cut), !!!custom_labels_terrestrial)

rownames(pca_vars_factors_aquatic_cut) <- recode(rownames(pca_vars_factors_aquatic_cut), !!!custom_labels_aquatic)

# Summary of PCA - Variance Explained and Cumulative Variance
pca_variance_t <- pca_result_terrestrial_cut$sdev^2  # Variance explained by each PC
pca_variance_ratio_t <- pca_variance_t / sum(pca_variance_t)  # Proportion of variance explained by each PC
cumulative_variance_t <- cumsum(pca_variance_ratio_t)  # Cumulative variance

pca_variance_a <- pca_result_aquatic_cut$sdev^2  # Variance explained by each PC
pca_variance_ratio_a <- pca_variance_a / sum(pca_variance_a)  # Proportion of variance explained by each PC
cumulative_variance_a <- cumsum(pca_variance_ratio_a)  # Cumulative variance

# Print PCA summary
print(pca_variance_ratio_t)
print(cumulative_variance_t)

print(pca_variance_ratio_a)
print(cumulative_variance_a)

# Load necessary libraries
library(ggplot2)
library(ggpubr)

# Compute variance explained for axis labels terrestrial
explained_variance_t <- pca_result_terrestrial_cut$sdev^2 / sum(pca_result_terrestrial_cut$sdev^2) * 100
x_label_t <- paste0("PC1 (", round(explained_variance_t[1], 1), "% Variance)")
y_label_t <- paste0("PC2 (", round(explained_variance_t[2], 1), "% Variance)")

ggplot(data = pca_data_terrestrial_cut, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Group), color = "red", alpha = 0.8, size = 4, show.legend = FALSE) + 
  
  # Vectors for variable loadings
  geom_segment(data = pca_vars_terrestrial_cut, aes(x = 0, y = 0, xend = PC1 * 3.5, yend = PC2 * 4), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1, color = "black") +
  
  # 👉 ADD THIS TO SHOW VECTOR LABELS
  geom_text(data = pca_vars_terrestrial_cut, 
            aes(x = PC1 * 3.5, y = PC2 * 4, label = Group), 
            size = 5, hjust = 0.5, vjust = -0.7, family = "serif", fontface = "bold") +
  
  scale_color_manual(values = "red", guide = "none") +  
  scale_shape_manual(values = c(16, 17, 18, 15, 8), guide = "none") +  
  
  theme_pubr() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.title = element_text(size = 25, face = "bold"),
    axis.text = element_text(size = 25),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  labs(
    x = x_label_t,
    y = y_label_t
  )

# Compute variance explained for axis labels aquatic
explained_variance_a <- pca_result_aquatic_cut$sdev^2 / sum(pca_result_aquatic_cut$sdev^2) * 100
x_label_a <- paste0("PC1 (", round(explained_variance_a[1], 1), "% Variance)")
y_label_a <- paste0("PC2 (", round(explained_variance_a[2], 1), "% Variance)")

library(ggrepel)

# Apply custom labels to aquatic variable names
pca_vars_factors_aquatic_cut$Group <- recode(pca_vars_factors_aquatic_cut$Group, !!!custom_labels_aquatic)

# Create the PCA biplot with non-overlapping labels
ggplot(data = pca_data_aquatic_cut, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Group), color = "red", alpha = 0.8, size = 4, show.legend = FALSE) + 
  
  geom_segment(data = pca_vars_factors_aquatic_cut, 
               aes(x = 0, y = 0, xend = PC1 * 3.5, yend = PC2 * 4), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1, color = "black") +
  
  # ➤ Repelled text labels
  geom_text_repel(data = pca_vars_factors_aquatic_cut, 
                  aes(x = PC1 * 3.5, y = PC2 * 4, label = Group),
                  size = 5, family = "serif", fontface = "bold",
                  min.segment.length = 0.1,
                  segment.size = 0.5,
                  segment.color = "grey30") +
  
  theme_pubr() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.title = element_text(size = 25, face = "bold"),
    axis.text = element_text(size = 25),
    legend.position = "none"
  ) +
  labs(
    x = x_label_a,
    y = y_label_a
  )
#-------------------------------------------------------------------------------------------------------------------------------------

# Setting the working directory
rm(list = ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

#============================== PCA (TERRESTRIAL ONLY) ==============================

# Load necessary libraries
library(tidyverse)
library(factoextra)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)
library(ggrepel)

# Load datasets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

# 🔻 Load additional biomass file (only column T = biomass values)
actual_biomass <- read_excel("Soil Microbe Biomass Calc.xlsx", sheet = 1)
actual_biomass_subset <- actual_biomass[, 20, drop = FALSE]  # Column T only
rownames(actual_biomass_subset) <- actual_biomass$`Row Labels`  # Replace with actual ID column if needed
colnames(actual_biomass_subset) <- "Biomass"

# Get common row names across datasets
common_rows <- Reduce(intersect, list(
  rownames(micr_cell_count),
  rownames(biomass_data),
  rownames(soil_prop_data),
  rownames(actual_biomass_subset)
))

# Subset to common rows
micr_cell_count <- micr_cell_count[common_rows, ]
biomass_data <- biomass_data[common_rows, ]
soil_prop_data <- soil_prop_data[common_rows, ]
actual_biomass_subset <- actual_biomass_subset[common_rows, ]

# Combine terrestrial datasets (including Biomass column)
combined_data_terrestrial <- cbind(biomass_data, soil_prop_data, actual_biomass_subset)

# Select relevant columns for PCA
combined_data_terrestrial_r <- combined_data_terrestrial %>%
  select(-c(1, 5))

# Remove incomplete or infinite rows
combined_data_terrestrial_cut <- combined_data_terrestrial_r[
  complete.cases(combined_data_terrestrial_r) &
    !apply(combined_data_terrestrial_r, 1, function(row) any(row == Inf | row == -Inf)),
]

# Run PCA
pca_result_terrestrial_cut <- prcomp(combined_data_terrestrial_cut, scale. = TRUE)

# Prepare PCA outputs
pca_data_terrestrial_cut <- as.data.frame(pca_result_terrestrial_cut$x)
pca_vars_terrestrial_cut <- as.data.frame(pca_result_terrestrial_cut$rotation)
pca_data_terrestrial_cut$Label <- rownames(combined_data_terrestrial_cut)

# Group by dominant variable (optional)
pca_data_terrestrial_cut$Group <- apply(combined_data_terrestrial_cut, 1, function(row) {
  colnames(combined_data_terrestrial_cut)[which.max(row)]
})

# Clean loadings: remove irrelevant variables if needed
exclude_factors_terrestrial_cut <- c("Average of freezeDryMass [Gram]", 
                                     "Average of totalLipidScaledConcentration [nanomolesPerGram]", 
                                     "Average of lipidInternalStandardResponse [2000picoAmpSecond]")
pca_vars_terrestrial_cut <- pca_vars_terrestrial_cut[!rownames(pca_vars_terrestrial_cut) %in% exclude_factors_terrestrial_cut, ]
pca_vars_terrestrial_cut$Group <- rownames(pca_vars_terrestrial_cut)

# Custom labels (including biomass)
custom_labels_terrestrial <- c(
  "Average of soilTemp [Degree]" = "Soil Temp",
  "Average of soilMoisture [Gram/Gram]" = "Soil Moisture",
  "Average of soilInWaterpH [pH]" = "Soil pH",
  "Average of nitrogenPercent [%]" = "Nitrogen %",
  "Average of organicCPercent [%]" = "Organic C %",
  "Average of soilFreshMass [Gram]" = "Soil Fresh Mass",
  "Biomass" = "Measured Biomass"
)

# Rename PCA data labels
pca_data_terrestrial_cut$Label <- recode(pca_data_terrestrial_cut$Label, !!!custom_labels_terrestrial)
rownames(pca_vars_terrestrial_cut) <- recode(rownames(pca_vars_terrestrial_cut), !!!custom_labels_terrestrial)

# PCA variance summary
explained_variance_t <- pca_result_terrestrial_cut$sdev^2 / sum(pca_result_terrestrial_cut$sdev^2) * 100
x_label_t <- paste0("PC1 (", round(explained_variance_t[1], 1), "% Variance)")
y_label_t <- paste0("PC2 (", round(explained_variance_t[2], 1), "% Variance)")

# Plot PCA for terrestrial
ggplot(data = pca_data_terrestrial_cut, aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = Group), color = "red", alpha = 0.8, size = 4, show.legend = FALSE) + 
  geom_segment(data = pca_vars_terrestrial_cut, 
               aes(x = 0, y = 0, xend = PC1 * 3.5, yend = PC2 * 4), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1, color = "black") +
  geom_text(data = pca_vars_terrestrial_cut, 
            aes(x = PC1 * 3.5, y = PC2 * 4, label = Group), 
            size = 5, hjust = 0.5, vjust = -0.7, family = "serif", fontface = "bold") +
  scale_color_manual(values = "red", guide = "none") +  
  scale_shape_manual(values = c(16, 17, 18, 15, 8), guide = "none") +  
  theme_pubr() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.title = element_text(size = 25, face = "bold"),
    axis.text = element_text(size = 25),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  labs(
    x = x_label_t,
    y = y_label_t
  )


