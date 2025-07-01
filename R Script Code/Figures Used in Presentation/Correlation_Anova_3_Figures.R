#Abundance Figures
#Soil
rm(list=ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

library(readxl)
library(ggplot2)
library(scales)

# Load data
soil <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 1)

# PLOT RAW DATA, LET ggplot APPLY LOG SCALE
ggplot(soil, aes(
  x = `Average of meanCopyNumber [Number/Nanogram]`,
  y = `Average of nucleicAcidConcentration [Nanograms]`
)) + 
  geom_point(size = 4) +
  geom_smooth(method = "lm", color = "chocolate", se = TRUE, size = 3) + 
  scale_x_log10(
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_log10(
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(
    title = "Panel A", 
    x = expression("Mean Copy Number [Copies/ng DNA]"),
    y = expression("Nucleic Acid Concentration [Nanograms]")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 30, family = "serif", color = "black"),
    axis.text.y = element_text(size = 30, family = "serif", color = "black"),
    text = element_text(size = 25, family = "serif", color = "black"),
    legend.title = element_text(size = 16, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Regression model: on log-transformed data
summary(lm(
  log10(soil$`Average of nucleicAcidConcentration [Nanograms]`) ~ 
    log10(soil$`Average of meanCopyNumber [Number/Nanogram]`)
))


#Benthic
library(readxl)
library(ggplot2)
library(scales)

# Load data
benthic <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 2)

# ggplot with proper log scales and fixed breaks
ggplot(benthic, aes(
  x = `Average of meanCopyNumber [Number/Nanogram]`,
  y = `Average of nucleicAcidConcentration [Nanograms]`
)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", color = "blue", se = TRUE, size = 3) + 
  scale_x_log10(
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = 10^seq(0, 3, by = 1),   # <-- Force y-axis breaks at 10^0, 10^1, 10^2, 10^3
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(
    title = "Panel B", 
    x = "Mean Copy Number [Copies/ng DNA]", 
    y = "Nucleic Acid Concentration [Nanograms]"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 30, family = "serif", color = "black"),
    axis.text.y = element_text(size = 30, family = "serif", color = "black"),
    text = element_text(size = 25, family = "serif", color = "black"),
    legend.title = element_text(size = 16, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Model on log-log
summary(lm(
  log10(benthic$`Average of nucleicAcidConcentration [Nanograms]`) ~ 
    log10(benthic$`Average of meanCopyNumber [Number/Nanogram]`)
))


#Water
library(readxl)
library(ggplot2)
library(scales)

# Load data
water <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 3)

# Plot raw values, apply log scales
ggplot(water, aes(
  x = `Average of meanCopyNumber [Nanograms]`,
  y = `Average of nucleicAcidConcentration [N/A]`
)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", color = "cyan", se = TRUE, size = 3) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(
    title = "Panel C", 
    x = expression("Mean Copy Number [Copies/ng DNA]"),
    y = expression("Nucleic Acid Concentration [Nanograms]")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 30, family = "serif", color = "black"),
    axis.text.y = element_text(size = 30, family = "serif", color = "black"),
    text = element_text(size = 25, family = "serif", color = "black"),
    legend.title = element_text(size = 16, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Linear model: log10-transformed
summary(lm(
  log10(water$`Average of nucleicAcidConcentration [N/A]`) ~ 
    log10(water$`Average of meanCopyNumber [Nanograms]`)
))


# Clear environment and load libraries
rm(list=ls())
library(readxl)
library(ggplot2)
library(scales)
library(cowplot)

# Set working directory
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

# Load data
soil <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 1)
benthic <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 2)
water <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 3)

# Soil plot (Panel A)
p1 <- ggplot(soil, aes(
  x = `Average of meanCopyNumber [Number/Nanogram]`,
  y = `Average of nucleicAcidConcentration [Nanograms]`
)) + 
  geom_point(size = 4) +
  geom_smooth(method = "lm", color = "chocolate", se = TRUE, size = 3) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Panel A", 
       x = expression("Mean Copy Number [Copies/ng DNA]"),
       y = expression("Nucleic Acid Concentration [Nanograms]")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, family = "serif", color = "black"),
    text = element_text(size = 14, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Benthic plot (Panel B)
p2 <- ggplot(benthic, aes(
  x = `Average of meanCopyNumber [Number/Nanogram]`,
  y = `Average of nucleicAcidConcentration [Nanograms]`
)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", color = "blue", se = TRUE, size = 3) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = 10^seq(0, 3, 1),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Panel B", 
       x = "Mean Copy Number [Copies/ng DNA]",
       y = "Nucleic Acid Concentration [Nanograms]") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, family = "serif", color = "black"),
    text = element_text(size = 14, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Water plot (Panel C)
p3 <- ggplot(water, aes(
  x = `Average of meanCopyNumber [Nanograms]`,
  y = `Average of nucleicAcidConcentration [N/A]`
)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", color = "cyan", se = TRUE, size = 3) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Panel C", 
       x = expression("Mean Copy Number [Copies/ng DNA]"),
       y = expression("Nucleic Acid Concentration [Nanograms]")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, family = "serif", color = "black"),
    text = element_text(size = 14, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Combine with cowplot: top row (p1, p2), bottom row (centered p3)
top_row <- plot_grid(p1, p2, ncol = 2, labels = NULL, align = "hv")
bottom_row <- plot_grid(NULL, p3, NULL, ncol = 3, rel_widths = c(1, 1.2, 1))

# Final panel
final_panel <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))
print(final_panel)



#------------------(ANOVA b/c it is comparing nucleic_acid_concentration vs. soil, benthic, and water)-----------

# Load necessary libraries
library(readxl)
library(dplyr)

# Read the data for all ecosystems and combine
soil <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 1) %>%
  mutate(ecosystem = "soil")

benthic <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 2) %>%
  mutate(ecosystem = "benthic")

water <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 3) %>%
  mutate(ecosystem = "water")

combined_data <- bind_rows(
  soil %>% select(`Average of meanCopyNumber [Number/Nanogram]`, `Average of nucleicAcidConcentration [Nanograms]`, ecosystem),
  benthic %>% select(`Average of meanCopyNumber [Number/Nanogram]`, `Average of nucleicAcidConcentration [Nanograms]`, ecosystem),
  water %>% select(`Average of meanCopyNumber [Nanograms]`, `Average of nucleicAcidConcentration [N/A]`, ecosystem)
) %>%
  rename(
    meanCopyNumber = `Average of meanCopyNumber [Number/Nanogram]`,
    nucleicAcidConcentration = `Average of nucleicAcidConcentration [Nanograms]`
  )

# Perform ANOVA
anova_result <- aov(nucleicAcidConcentration ~ ecosystem, data = combined_data)
summary(anova_result)





