# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)  # For rename_with() and other data manipulation functions

# Load the data from Excel sheets
micr_cell_count <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 6)
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
  expression(bold("O2_H2O[mg]")), 
  expression(bold("O2_H2O Sat[%]")), 
  expression(bold("Conduct.[µS/cm]")),
  expression(bold("H2O Temp.[°C]")),
  expression(bold("Microb Den.[#/mL]")),
  expression(bold("Dry Mass[Gram]")),
  expression(bold("Lipid Conc.[nmol/g]")),
  expression(bold("Lipid Res.[2 nA/s]")),
  expression(bold("Soil Temp.[°C]")),
  expression(bold("Soil Moist.[g/g]")),
  expression(bold("Soil pH[pH]")),
  expression(bold("Nitrogen[%]")),
  expression(bold("Carbon[%]")),
  expression(bold("Soil Mass[Gram]"))
)

panel.cor <- function(x, y, digits = 2, cex.cor = 2, ...) {  
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # Ensure x and y are treated as numeric vectors
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # Compute correlation and p-value safely
  if (length(na.omit(x)) > 1 & length(na.omit(y)) > 1) {
    r <- cor(x, y, use = "complete.obs")  # Use only complete cases
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste("r= ", txt, sep = "")
    text(0.5, 0.7, txt, cex = 1.5, font = 2)  # Increased font size and bold
    
    p <- cor.test(x, y)$p.value
    txt2 <- ifelse(p < 0.01, "p<0.01", paste("p= ", format(p, digits = digits), sep = ""))
    text(0.5, 0.3, txt2, cex = 1.7, font = 2)  # Increased font size and bold
  }
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

# Plotting the combined correlation matrix to screen (rather than saving as PDF)
par(family="serif", font = 1, cex = 1.5)
pairs(combined_data, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist,
      labels = custom_labels, 
      cex=2, lwd=1, las=3, cex.lab=1.5, cex.axis=1.25, font.lab=2, family="serif",
      main="Combined Correlation Matrix of Different Variables",
      cex.main=1.5)

pairs(combined_data, 
      lower.panel = panel.smooth, 
      upper.panel = panel.cor, 
      diag.panel = panel.hist,
      labels = custom_labels, 
      cex = 1,  # Larger points in scatterplot
      lwd = 3, 
      las = 2, 
      cex.lab = 4,  # Larger labels
      cex.axis = 1.25,  # Larger axis text
      font.lab = 2, 
      family = "serif",
      main = "Combined Correlation Matrix of Different Variables",
      cex.main = 2.5  # Larger title
)


