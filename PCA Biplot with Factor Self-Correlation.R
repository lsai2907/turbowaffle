# Setting the working directory
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)  # For rename_with() and other data manipulation functions

# Load the data from Excel sheets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 5)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

# Combine all datasets into one dataframe
common_rows <- Reduce(intersect, list(
  rownames(micr_cell_count),
  rownames(biomass_data),
  rownames(soil_prop_data)
))

micr_cell_count <- micr_cell_count[common_rows, ]
biomass_data <- biomass_data[common_rows, ]
soil_prop_data <- soil_prop_data[common_rows, ]

combined_data <- cbind(micr_cell_count, biomass_data, soil_prop_data)

combined_data <- combined_data %>%
  select(-c(1, 9, 13, ncol(combined_data)-1, ncol(combined_data)))  # Remove last two columns

# Ensure column names are unique
combined_data <- combined_data %>%
  rename_with(~make.unique(.x), everything())

# Check the duplicated column names
duplicate_columns <- combined_data %>%
  select(contains("Row Labels")) %>%
  names()

# Rename duplicated "Row Labels" columns
combined_data <- combined_data %>%
  rename_at(duplicate_columns, ~ paste0(.x, "_duplicate"))

# Define custom labels for the variables
custom_labels <- c(
  expression(bold("O2 in H2O [mg]")), 
  expression(bold("O2 in H2O Sat [%]")), 
  expression(bold("Conductivity [µS/cm]")),
  expression(bold("H2O Temp [°C]")),
  expression(bold("Microbe Den [#/mL]")),
  expression(bold("Dry Mass [Gram]")),
  expression(bold("Lipid Conc [nmol/g]")),
  expression(bold("Lipid Resp [2 nA/s]")),
  expression(bold("Soil Temp [°C]")),
  expression(bold("Soil Moisture [g/g]")),
  expression(bold("Soil pH [pH]")),
  expression(bold("Nitrogen [%]")),
  expression(bold("Carbon [%]")),
  expression(bold("Soil Mass [Gram]"))
)

# Custom panel functions for correlation plot
panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")  # Use only complete cases
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt, cex = 1.2)
  
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 1.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if (p < 0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2, cex = 1.2)
}

panel.smooth <- function (x, y, col = "blue", bg = NA, pch = 18, cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
}

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan")
}

# Plotting the combined correlation matrix
pdf("HOLY_combined_correlation_matrix.pdf", width=14, height=12)
par(family="serif", font = 1, cex = 1.5)
pairs(combined_data, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist,
      labels = custom_labels, 
      cex=2, lwd=1, las=3, cex.lab=1.5, cex.axis=1.25, font.lab=2, family="serif",
      main="Combined Correlation Matrix of Different Variables",
      cex.main=1.5)
dev.off()

#==============================PCA

# Load necessary libraries
library(tidyverse)
library(factoextra)
library(ggplot2)

# Ensure PCA data is clean
combined_data %>% 
  names() %>% 
  duplicated() %>% 
  any()

# Identify and remove columns with zero variance
zero_variance_cols <- combined_data %>%
  summarise(across(everything(), sd)) %>%
  summarise(across(everything(), ~ .x == 0)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Zero_Variance") %>%
  filter(Zero_Variance == TRUE) %>%
  pull(Column)

# Remove rows with missing values or infinite values
combined_data <- combined_data[complete.cases(combined_data) & !apply(combined_data, 1, function(row) any(row == Inf | row == -Inf)), ]

# Perform PCA
pca_result <- prcomp(combined_data, scale. = TRUE)

# Create PCA data for plotting
pca_data <- as.data.frame(pca_result$x)
pca_vars <- as.data.frame(pca_result$rotation)

# Add row names to pca_data for labels
pca_data$Label <- rownames(combined_data)

# Assign factor groups (Example: Customize this for your dataset)
# Replace with your logic for assigning groups
pca_data$Group <- apply(combined_data, 1, function(row) {
  # Grouping logic (example: max contributing variable)
  colnames(combined_data)[which.max(row)]
})

# Remove specific factors from the variable loadings
exclude_factors <- c("Average of cellCountSampleVolume [mL]", "Average of totalCellCount [number]")
pca_vars <- pca_vars[!rownames(pca_vars) %in% exclude_factors, ]
pca_vars$Group <- rownames(pca_vars) # Each variable is its own group

# Dynamically generate unique colors for groups
unique_groups <- unique(c(pca_data$Group, pca_vars$Group))
group_colors <- scales::hue_pal()(length(unique_groups))

# Custom name mappings (Replace this with your own mapping)
custom_labels <- c(
  "Average Cell Density [number/mL]" = "Cell Density",
  "Average of dissolvedOxygen [mg]" = "Dissolved O2",
  "Average of dissolvedOxygenSaturation [%]" = "Dissolved O2 Sat.",
  "Average of freezeDryMass [Gram]" = "Freeze Dry Mass",
  "Average of lipidInternalStandardResponse [2000picoAmpSecond]" = "Lipid Int. Standard Resp.",
  "Average of nitrogenPercent [%]" = "Nitrogen %",
  "Average of soilInWaterpH [pH]" = "pH of Soil",
  "Average of soilMoisture [Gram/Gram]" = "Soil Moisture",
  "Average of soilTemp [Degree]" = "Soil Temp",
  "Average of specificConductance [microsiemens/cm]" = "Spec. Conductance",
  "Average of totalLipidScaledConcentration [nanomolesPerGram]" = "Lipid Scaled Conc.",
  "Average of waterTemp [C]" = "H2O Temp"
)

# Apply the custom names to the pca_data$Label
pca_data$Label <- recode(pca_data$Label, !!!custom_labels)

# Apply the custom names to the pca_vars rownames
# Only recode if the rownames are found in the custom_labels mapping
rownames(pca_vars) <- recode(rownames(pca_vars), !!!custom_labels)

# Plot
# Plot with adjusted text positions to reduce overlap
ggplot(data = pca_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Group), alpha = 0.6, size = 3) +
  # Use position_nudge to adjust text positions slightly
  geom_text(aes(label = Label, color = Group), family = "serif", 
            vjust = -0.5, size = 3, show.legend = FALSE, 
            position = position_nudge(x = 0.1, y = 0.1)) +  # Adjust values as needed
  geom_segment(data = pca_vars, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5, color = Group), 
               arrow = arrow(length = unit(0.2, "cm")), size = 1) +
  geom_text(data = pca_vars, aes(x = PC1 * 5, y = PC2 * 5, label = rownames(pca_vars), color = Group), 
            family = "serif", vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(values = setNames(group_colors, unique_groups)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"), # Set all text to serif font
    legend.position = "right", # Move legend to the bottom
    legend.title = element_text(hjust = 0.5), # Center-align the legend title
    legend.text = element_text(size = 10), # Adjust legend text size
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold") # Center-align the plot title
  ) +
  labs(
    title = "PCA Biplot with Factor Self-Correlation", 
    x = "Principal Component 1", 
    y = "Principal Component 2",
    color = "Legend" # Rename legend title
  )

#------------------------
#==============================PCA

# Load necessary libraries
library(tidyverse)
library(factoextra)
library(ggplot2)

# Ensure PCA data is clean
combined_data %>% 
  names() %>% 
  duplicated() %>% 
  any()

# Identify and remove columns with zero variance
zero_variance_cols <- combined_data %>%
  summarise(across(everything(), sd)) %>%
  summarise(across(everything(), ~ .x == 0)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Zero_Variance") %>%
  filter(Zero_Variance == TRUE) %>%
  pull(Column)

# Remove rows with missing values or infinite values
combined_data <- combined_data[complete.cases(combined_data) & !apply(combined_data, 1, function(row) any(row == Inf | row == -Inf)), ]

# Perform PCA
pca_result <- prcomp(combined_data, scale. = TRUE)

# Create PCA data for plotting
pca_data <- as.data.frame(pca_result$x)
pca_vars <- as.data.frame(pca_result$rotation)

# Add row names to pca_data for labels
pca_data$Label <- rownames(combined_data)

# Assign factor groups (Example: Customize this for your dataset)
pca_data$Group <- apply(combined_data, 1, function(row) {
  colnames(combined_data)[which.max(row)]
})

# Remove specific factors from the variable loadings
exclude_factors <- c("Average of cellCountSampleVolume [mL]", "Average of totalCellCount [number]")
pca_vars <- pca_vars[!rownames(pca_vars) %in% exclude_factors, ]
pca_vars$Group <- rownames(pca_vars) # Each variable is its own group

# Dynamically generate unique colors for groups
unique_groups <- unique(c(pca_data$Group, pca_vars$Group))
group_colors <- scales::hue_pal()(length(unique_groups))

# Custom name mappings (Replace this with your own mapping)
custom_labels <- c(
  "Average Cell Density [number/mL]" = "Cell Density",
  "Average of dissolvedOxygen [mg]" = "Dissolved O2",
  "Average of dissolvedOxygenSaturation [%]" = "Dissolved O2 Sat.",
  "Average of freezeDryMass [Gram]" = "Freeze Dry Mass",
  "Average of lipidInternalStandardResponse [2000picoAmpSecond]" = "Lipid Int. Standard Resp.",
  "Average of nitrogenPercent [%]" = "Nitrogen %",
  "Average of soilInWaterpH [pH]" = "pH of Soil",
  "Average of soilMoisture [Gram/Gram]" = "Soil Moisture",
  "Average of soilTemp [Degree]" = "Soil Temp",
  "Average of specificConductance [microsiemens/cm]" = "Spec. Conductance",
  "Average of totalLipidScaledConcentration [nanomolesPerGram]" = "Lipid Scaled Conc.",
  "Average of waterTemp [C]" = "H2O Temp"
)

# Apply the custom names to the pca_data$Label
pca_data$Label <- recode(pca_data$Label, !!!custom_labels)

# Apply the custom names to the pca_vars rownames
# Only recode if the rownames are found in the custom_labels mapping
rownames(pca_vars) <- recode(rownames(pca_vars), !!!custom_labels)

# Summary of PCA - Variance Explained and Cumulative Variance
pca_variance <- pca_result$sdev^2  # Variance explained by each PC
pca_variance_ratio <- pca_variance / sum(pca_variance)  # Proportion of variance explained by each PC
cumulative_variance <- cumsum(pca_variance_ratio)  # Cumulative variance

# Print PCA summary
cat("PCA Summary:\n")
cat("=====================================\n")
cat("Variance Explained by Each Principal Component:\n")
print(pca_variance_ratio)
cat("\nCumulative Variance Explained by PCs:\n")
print(cumulative_variance)

# Plot
ggplot(data = pca_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Group), alpha = 0.6, size = 3) +
  # Use position_nudge to adjust text positions slightly
  geom_text(aes(label = Label, color = Group), family = "serif", 
            vjust = -0.5, size = 5, show.legend = FALSE, 
            position = position_nudge(x = 0.1, y = 0.1)) +  # Adjust values as needed
  geom_segment(data = pca_vars, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5, color = Group), 
               arrow = arrow(length = unit(0.2, "cm")), size = 1) +
  geom_text(data = pca_vars, aes(x = PC1 * 5, y = PC2 * 5, label = rownames(pca_vars), color = Group), 
            family = "serif", vjust = -0.5, size = 7, show.legend = FALSE) +
  scale_color_manual(values = setNames(group_colors, unique_groups)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"), # Set all text to serif font
    legend.position = "bottom", # Move legend to the bottom
    legend.title = element_text(hjust = 0.5), # Center-align the legend title
    legend.text = element_text(size = 10), # Adjust legend text size
    legend.box = "horizontal", # Arrange legend horizontally
    legend.box.spacing = unit(0.5, "cm"), # Space between legend items
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold") # Center-align the plot title
  ) +
  labs(
    title = "PCA Biplot with Factor Self-Correlation", 
    x = "Principal Component 1", 
    y = "Principal Component 2",
    color = "" # Rename legend title
  ) +
  guides(color = guide_legend(ncol = 2)) # Set the number of columns in the legend
