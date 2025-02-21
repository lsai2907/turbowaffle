#Abundance Figures
#Soil
rm(list=ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

library(readxl)
library(ggplot2)

soil <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 1)

#ggplot version
ggplot(soil, aes(`Average of meanCopyNumber [Number/Nanogram]`,`Average of nucleicAcidConcentration [Nanograms]`
))+ 
  geom_point(size=3) +
  geom_smooth(method = "lm", color = "chocolate", se = T ,size=3) + 
  labs(title= "[NAC] vs. [MCN] in Soil", x = "Mean Copy Number [Number/Nanogram]", y = "Nucleic Acid Concentration [Nanograms]")+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 20, family = "serif", color = "black"),
    axis.text.y = element_text(size = 20, family = "serif", color = "black"),
    text = element_text(size = 35, family = "serif", color = "black"),
    legend.title = element_text(size = 16, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")  # Bold the title
  )

summary(lm(soil$`Average of nucleicAcidConcentration [Nanograms]`~soil$`Average of meanCopyNumber [Number/Nanogram]`))

#Benthic
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/Charted_Excel")
library(readxl)
library(ggplot2)

benthic <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 2)

#ggplot version
ggplot(benthic, aes(`Average of meanCopyNumber [Number/Nanogram]`,`Average of nucleicAcidConcentration [Nanograms]`
))+ 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue", se = TRUE,size=3) + 
  labs(title= "[NAC] vs. [MCN] in Benthic Waters", x = "Mean Copy Number [Number/Nanogram]", y = "Nucleic Acid Concentration [Nanograms]")+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 20, family = "serif", color = "black"),
    axis.text.y = element_text(size = 20, family = "serif", color = "black"),
    text = element_text(size = 35, family = "serif", color = "black"),
    legend.title = element_text(size = 16, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")  # Bold the title
  )

summary(lm(benthic$`Average of nucleicAcidConcentration [Nanograms]`~benthic$`Average of meanCopyNumber [Number/Nanogram]`))

#Water
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/Charted_Excel")
library(readxl)
library(ggplot2)

water <- read_excel("ABUNDANCE CHARTING.xlsx", sheet = 3)

#ggplot version
ggplot(water, aes(`Average of meanCopyNumber [Nanograms]`,`Average of nucleicAcidConcentration [N/A]`
))+ 
  geom_point() + 
  geom_smooth(method = "lm", color = "cyan", se = TRUE,size=3) + 
  labs(title= "[NAC] vs. [MCN] in Water", x = "Mean Copy Number [Number/Nanogram]", y = "Nucleic Acid Concentration [Nanograms]")+
  theme_classic()+
  theme(
    axis.text.x = element_text(size = 20, family = "serif", color = "black"),
    axis.text.y = element_text(size = 20, family = "serif", color = "black"),
    text = element_text(size = 35, family = "serif", color = "black"),
    legend.title = element_text(size = 16, family = "serif", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")  # Bold the title
  )
summary(lm(water$`Average of nucleicAcidConcentration [N/A]`~water$`Average of meanCopyNumber [Nanograms]`))


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





