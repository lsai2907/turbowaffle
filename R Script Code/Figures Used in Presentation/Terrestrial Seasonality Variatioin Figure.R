# Remove all objects from global environment
rm(list = ls())

setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(showtext)

# Enable Times New Roman font (adjust path if needed)
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()

# Read the Excel file, columns J and K from the first sheet
data_raw <- read_excel("Soil Microbe Biomass Calc.xlsx", sheet = 1, range = "J2:K1000", col_names = c("Label", "Biomass"))

# Define month names
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Reconstruct site/month structure and clean missing values
data <- data_raw %>%
  mutate(Site = ifelse(Label %in% months, NA, Label)) %>%
  tidyr::fill(Site) %>%
  filter(Label %in% months) %>%
  rename(Month = Label) %>%
  mutate(
    Month = factor(Month, levels = months),
    Biomass = as.numeric(Biomass)
  ) %>%
  filter(!is.na(Biomass))  # ✅ Remove rows with missing biomass

# Normalize biomass per month (z-score)
data <- data %>%
  group_by(Month) %>%
  mutate(Normalized = scale(Biomass)[,1]) %>%
  ungroup() %>%
  mutate(Normalized = pmax(pmin(Normalized, 0.3), -0.3))  # Clip

# Color-blind friendly palette
cb_palette <- c(
  "#0072B2", "#009E73", "#D55E00", "palevioletred1", "#E69F00", "magenta", "firebrick1",
  "#999999", "#000000", "darkorange4", "darkslateblue", "blue4", "#66A61E", "cornflowerblue",
  "#7570B3", "darkslategrey", "#FFD92F", "#B3DE69", "#8DD3C7", "blue", "#80B1D3", "#BC80BD", "moccasin", "mediumturquoise",
  "indianred", "yellow", "lightsalmon", "steelblue3",
  "palegreen", "powderblue", "springgreen", "darkgoldenrod",
  "darkkhaki", "forestgreen", "rosybrown1", "lavender", "deeppink",
  "thistle", "tomato", "honeydew", "violet", "darkmagenta", "cyan4",
  "wheat", "darkolivegreen", "darkolivegreen1", "sandybrown" 
)

# Plot
ggplot(data, aes(x = Month, y = Biomass, group = Site, color = Site)) +
  geom_line(size = 1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = rep(cb_palette, length.out = length(unique(data$Site)))) +
  labs(x = "Month", y = "Biomass Density") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 40, margin = margin(t = 15)),
    axis.title.y = element_text(size = 40, margin = margin(r = 15)),
    axis.text = element_text(size = 34, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 30, color = "black"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.size = unit(1, "cm"),
    legend.spacing.x = unit(1, "cm"),
    plot.margin = margin(10, 10, 40, 10),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(nrow = 6, byrow = TRUE)) 
 l#_-----------------------------------------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(showtext)
library(Polychrome) 

# Enable Times New Roman font (adjust path if needed)
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()

# Read the Excel file, columns J and K from the first sheet
data_raw <- read_excel("Soil Microbe Biomass Calc.xlsx", sheet = 1, range = "J2:K1000", col_names = c("Label", "Biomass"))

# Define month names
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Reconstruct site/month structure and clean missing values
data <- data_raw %>%
  mutate(Site = ifelse(Label %in% months, NA, Label)) %>%
  tidyr::fill(Site) %>%
  filter(Label %in% months) %>%
  rename(Month = Label) %>%
  mutate(
    Month = factor(Month, levels = months),
    Biomass = as.numeric(Biomass)
  ) %>%
  filter(!is.na(Biomass))  # ✅ Remove rows with missing biomass

# 🔁 GLOBAL normalization (not per month) and clip to ±0.3
global_mean <- mean(data$Biomass, na.rm = TRUE)
global_sd <- sd(data$Biomass, na.rm = TRUE)

data <- data %>%
  mutate(
    Normalized = (Biomass - global_mean) / global_sd,
    Normalized = pmax(pmin(Normalized, 0.3), -0.3)
  )

# Color-blind friendly palette

cb_palette <- c(
  "#0072B2", "#009E73", "#D55E00", "palevioletred1", "#E69F00", "magenta", "firebrick1",
  "#999999", "#000000", "darkorange4", "darkslateblue", "blue4", "#66A61E", "cornflowerblue",
  "#7570B3", "darkslategrey", "#FFD92F", "#B3DE69", "#8DD3C7", "blue", "#80B1D3", "#BC80BD", "moccasin", "mediumturquoise",
  "indianred", "yellow", "lightsalmon", "steelblue3",
  "palegreen", "powderblue", "springgreen", "darkgoldenrod",
  "darkkhaki", "forestgreen", "rosybrown1", "lavender", "deeppink",
  "thistle", "tomato", "honeydew", "violet", "darkmagenta", "cyan4",
  "wheat", "darkolivegreen", "darkolivegreen1", "sandybrown" 
)

# Plot using normalized values
ggplot(data, aes(x = Month, y = Normalized, group = Site, color = Site)) +
  geom_line(size = 1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = rep(cb_palette, length.out = length(unique(data$Site)))) +
  labs(x = "Month", y = "Normalized Biomass Density") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 40, margin = margin(t = 15)),
    axis.title.y = element_text(size = 40, margin = margin(r = 15)),
    axis.text = element_text(size = 34, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 24, color = "black"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.size = unit(1, "cm"),
    legend.spacing.x = unit(0.5, "cm"),
    plot.margin = margin(10, 10, 40, 10),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(nrow = 6, byrow = TRUE))

                                    
                                    