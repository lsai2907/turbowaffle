# Set working directory
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Load datasets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)
biomass_data <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)
soil_prop_data <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

# 🔻 Load actual biomass file (only columns S and T from Sheet 1)
actual_biomass <- read_excel("Soil Microbe Biomass Calc.xlsx", sheet = 1)  # ⬅️ Replace with actual filename
actual_biomass_subset <- actual_biomass[, c(19, 20)]  # Columns S and T
rownames(actual_biomass_subset) <- actual_biomass$`Row Labels`  # ⬅️ Replace if different

# Get common row names across all 4 datasets
common_rows <- Reduce(intersect, list(
  rownames(micr_cell_count),
  rownames(biomass_data),
  rownames(soil_prop_data),
  rownames(actual_biomass_subset)
))

# Subset all datasets to common rows
micr_cell_count <- micr_cell_count[common_rows, ]
biomass_data <- biomass_data[common_rows, ]
soil_prop_data <- soil_prop_data[common_rows, ]
actual_biomass_subset <- actual_biomass_subset[common_rows, ]

# Combine all datasets
combined_data <- cbind(micr_cell_count, biomass_data, soil_prop_data, actual_biomass_subset)

# Remove irrelevant columns by index
combined_data <- combined_data %>%
  select(-c(1, 9, 13))

# Ensure unique column names
combined_data <- combined_data %>%
  rename_with(~make.unique(.x), everything())

# Remove duplicated "Row Labels" columns if they exist
duplicate_columns <- combined_data %>%
  select(contains("Row Labels")) %>%
  names()

combined_data <- combined_data %>%
  rename_at(duplicate_columns, ~ paste0(.x, "_duplicate"))

# Remove specific columns by name
combined_data <- combined_data %>%
  select(
    -`Average of totalLipidScaledConcentration [nanomolesPerGram]`,
    -`Average of lipidInternalStandardResponse [2000picoAmpSecond]`
  )

# Drop non-numeric columns
combined_data <- combined_data %>%
  select(where(is.numeric))

# Custom labels for variables
custom_labels <- c(
  expression(bold("Total Cell Count [#]")),
  expression(bold("Sample Volume [mL]")),
  expression(bold("Cell Density [#/mL]")),
  expression(bold("Dissolved O₂ [mg/L]")),
  expression(bold("O₂ Saturation [%]")),
  expression(bold("Conductivity [µS/cm]")),
  expression(bold("Water Temp [°C]")),
  expression(bold("Freeze-Dry Mass [g]")),
  expression(bold("Soil Temp [°C]")),
  expression(bold("Soil Moisture [g/g]")),
  expression(bold("Soil pH [pH]")),
  expression(bold("Nitrogen [%]")),
  expression(bold("Organic C [%]")),
  expression(bold("Soil Fresh Mass [g]")),
  expression(bold("Biomass [mmol C/kg]"))
)

# Custom correlation panel
panel.cor <- function(x, y, digits = 2, cex.cor = 2, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(na.omit(x)) > 1 & length(na.omit(y)) > 1) {
    r <- cor(x, y, use = "complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste("r= ", txt, sep = "")
    text(0.5, 0.7, txt, cex = 1.7, font = 2, family = "serif")
    
    p <- cor.test(x, y)$p.value
    txt2 <- ifelse(p < 0.01, "p<0.01", paste("p= ", format(p, digits = digits), sep = ""))
    text(0.5, 0.3, txt2, cex = 1.7, font = 2, family = "serif")
  }
}

# Custom scatter panel
panel.smooth <- function (x, y, col = "black", bg = NA, pch = 18, cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
}

# Histogram panel
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey")
}

# Pairs plot with bold serif text
pairs(combined_data, 
      lower.panel = panel.smooth, 
      upper.panel = panel.cor, 
      diag.panel = panel.hist,
      labels = custom_labels, 
      cex = 1.5,
      lwd = 2, 
      las = 3, 
      cex.lab = 3.5,
      cex.axis = 1.6,
      font.labels = 2,
      font.lab = 1.5,
      family = "serif")

# Function to calculate p-values
cor.test.pvalues <- function(df) {
  n <- ncol(df)
  pvalues <- matrix(NA, nrow = n, ncol = n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      test <- cor.test(df[[i]], df[[j]])
      pvalues[i,j] <- test$p.value
      pvalues[j,i] <- test$p.value
    }
  }
  return(pvalues)
}

# Correlation matrix and p-values
cor_matrix <- cor(combined_data, use = "complete.obs")
pvalues_matrix <- cor.test.pvalues(combined_data)

# Create summary of strong correlations
summary_table <- data.frame(
  Variable1 = character(0),
  Variable2 = character(0),
  Correlation = numeric(0),
  p_value = numeric(0),
  stringsAsFactors = FALSE
)

for(i in 1:(ncol(cor_matrix)-1)){
  for(j in (i+1):ncol(cor_matrix)){
    if(abs(cor_matrix[i,j]) > 0.5) {
      summary_table <- rbind(summary_table, data.frame(
        Variable1 = colnames(cor_matrix)[i],
        Variable2 = colnames(cor_matrix)[j],
        Correlation = cor_matrix[i,j],
        p_value = pvalues_matrix[i,j]
      ))
    }
  }
}

# Sort and display strongest results
summary_table_sorted <- summary_table[order(summary_table$p_value), ]
best_summary <- summary_table_sorted[summary_table_sorted$p_value < 0.01, ]
print(best_summary)
