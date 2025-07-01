# Remove all objects from global environment
rm(list = ls())

setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")


# Load libraries
library(readr)
library(dplyr)
library(reshape2)
library(plotly)
library(akima)

# Load data
data <- read_csv("Per-Site_Monthly_Cell_Density_with_Coordinates.csv")

# Reshape to long format
data_long <- melt(data,
                  id.vars = c("Site", "Latitude", "Longitude"),
                  variable.name = "Month",
                  value.name = "Density")

# Preserve month order
data_long$Month <- factor(data_long$Month,
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Normalize
data_long <- data_long %>%
  group_by(Month) %>%
  mutate(Normalized = scale(Density)[,1]) %>%
  ungroup()

# Interpolate surface
interp_data <- with(data_long, interp(
  x = as.numeric(Month),
  y = Latitude,
  z = Normalized,
  duplicate = "mean"
))

# Axis styling with ticks
axis_style <- list(
  titlefont = list(family = "Times New Roman", color = "black", size = 18),
  tickfont = list(family = "Times New Roman", color = "black", size = 16),
  showgrid = TRUE,
  zeroline = FALSE,
  showline = TRUE,
  mirror = TRUE,
  linecolor = "black",
  linewidth = 2,
  ticks = "outside",
  tickwidth = 2,
  tickcolor = "black",
  ticklen = 6
)

# Plot with ticks and larger fonts
plot_ly(
  x = interp_data$x,
  y = interp_data$y,
  z = interp_data$z,
  type = "surface",
  colorscale = list(c(0, "purple"), c(0.25, "blue"), c(0.5, "green"),
                    c(0.75, "yellow"), c(1, "red")),
  colorbar = list(
    title = "MBC normalization",
    titlefont = list(family = "Times New Roman", color = "black", size = 22),
    tickfont = list(family = "Times New Roman", color = "black", size = 22)
  )
) %>%
  layout(
    scene = list(
      xaxis = c(axis_style, list(
        title = "Month",
        tickvals = 1:12,
        ticktext = month.abb
      )),
      yaxis = c(axis_style, list(title = "Latitude (°)")),
      zaxis = c(axis_style, list(title = "MBC normalization"))
    ),
    font = list(family = "Times New Roman", color = "black", size = 18)
  )

#--------------------- 2d Line Graph-----------------------------------------------------
# --- Clip normalization between -0.3 and 0.3 ---
# Load necessary libraries
# Load necessary libraries
# Load necessary libraries
# Load necessary libraries
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(showtext)
library(colorspace)

# Enable high-quality font rendering
font_add("Times New Roman", regular = "times.ttf")  # Adjust path if needed
showtext_auto()

# Load data
data <- read_csv("Per-Site_Monthly_Cell_Density_with_Coordinates.csv")

# Convert to long format and normalize
data_long <- data %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Density") %>%
  mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

# Normalize per month (z-score)
data_long <- data_long %>%
  group_by(Month) %>%
  mutate(Normalized = scale(Density)[,1]) %>%
  ungroup() %>%
  mutate(Normalized = pmax(pmin(Normalized, 0.3), -0.3))  # Clip

# Color palette: Color-blind friendly
cb_palette <- c(
  "#0072B2", "#009E73", "#D55E00", "palevioletred1", "#E69F00", "magenta", "firebrick1", # original 7
  "#999999", "#000000", "darkorange4", "darkslateblue", "blue4", "#66A61E", "cornflowerblue", 
  "#7570B3", "darkslategrey", "#FFD92F", "#B3DE69", "#8DD3C7", "blue", "#80B1D3", "#BC80BD", "moccasin"
)

ggplot(data_long, aes(x = Month, y = Density, group = Site, color = Site)) +
  geom_line(size = 1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = rep(cb_palette, length.out = length(unique(data_long$Site)))) +
  labs(
    x = "Month",
    y = "MBC Density"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman", size = 22, color = "black"),
    axis.title.x = element_text(size = 40, color = "black", margin = margin(t = 15)),
    axis.title.y = element_text(size = 40, color = "black", margin = margin(r = 15)),
    axis.text = element_text(size = 34, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 30, color = "black"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.size = unit(1.5, "cm"),
    legend.spacing.x = unit(1, "cm"),
    plot.margin = margin(10, 10, 40, 10),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))


#---------------------------------------------------------------------------------------------
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)

# Enable Times New Roman font
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()

# Load aquatic data
data <- read_csv("Per-Site_Monthly_Cell_Density_with_Coordinates.csv")

# Define months
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Convert wide to long format and clean
data_long <- data %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Density") %>%
  mutate(
    Month = factor(Month, levels = months),
    Density = as.numeric(Density)
  ) %>%
  filter(!is.na(Density))

# Global z-score normalization and clip to ±0.3
global_mean <- mean(data_long$Density, na.rm = TRUE)
global_sd <- sd(data_long$Density, na.rm = TRUE)

data_long <- data_long %>%
  mutate(
    Normalized = (Density - global_mean) / global_sd,
    Normalized = pmax(pmin(Normalized, 0.3), -0.3)  # Clip to ±0.3
  )

# Color-blind friendly palette
cb_palette <- c(
  "#0072B2", "#009E73", "#D55E00", "palevioletred1", "#E69F00", "magenta", "firebrick1",
  "#999999", "#000000", "darkorange4", "darkslateblue", "blue4", "#66A61E", "cornflowerblue",
  "#7570B3", "darkslategrey", "#FFD92F", "#B3DE69", "#8DD3C7", "blue", "#80B1D3", "#BC80BD", "moccasin"
)

# Plot normalized aquatic biomass
ggplot(data_long, aes(x = Month, y = Normalized, group = Site, color = Site)) +
  geom_line(size = 1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = rep(cb_palette, length.out = length(unique(data_long$Site)))) +
  labs(x = "Month", y = "Normalized Cell Density") +
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
    legend.key.size = unit(1.5, "cm"),
    legend.spacing.x = unit(1, "cm"),
    plot.margin = margin(10, 10, 40, 10),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

