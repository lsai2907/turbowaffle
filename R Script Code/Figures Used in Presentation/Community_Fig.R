#------------BIOL 497 RESEARCH 

#SETTING WORKING DIRECTORY
rm(list=ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

library(readxl)
library(ggplot2)

#Sheet 1 is Benthic Community
# Read data from the first sheet
data_benthic <- read_excel("COM_CHA.xlsx", sheet = 1)

categories <- data_benthic[[1]]
values <- data_benthic[[2]]

dataframe <- data.frame(categories, values)

dataframe$percentage <- dataframe$values / sum(dataframe$values) * 100

# Plotting a pie chart
ggplot(dataframe, aes(x = "", y = values, fill = categories)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Benthic Community", fill = "Categories") + theme_void()
