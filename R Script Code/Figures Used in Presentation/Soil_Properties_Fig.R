rm(list=ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

library(readxl)
library(ggplot2)

# Read data from the Seasonal Pattern sheet
soilprop <- read_excel("SOIL PROP CHARTING.xlsx", sheet = 1)

plot(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of organicCPercent [%]`, type="p", xlab="organicCPercent [%]", 
     ylab="soilFreshMass [Gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of organicCPercent [%]`))

plot(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of nitrogenPercent [%]`, type="p", xlab="Average of nitrogenPercent [%]", 
     ylab="soilFreshMass [Gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of nitrogenPercent [%]`))

plot(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of soilInWaterpH [pH]`, type="p", xlab="soilInWaterpH [pH]", 
     ylab="soilFreshMass [Gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of soilInWaterpH [pH]`))

plot(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of soilMoisture [Gram/Gram]`, type="p", xlab="soilMoisture [Gram/Gram]", 
     ylab="soilFreshMass [Gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of soilMoisture [Gram/Gram]`))

plot(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of soilTemp [Degree]`, type="p", xlab="soilTemp [Degree]", 
     ylab="soilFreshMass [Gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(soilprop$`Average of soilFreshMass [Gram]`~soilprop$`Average of soilTemp [Degree]`))

panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")  # Use only complete cases
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt, cex = 2)
  
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 1.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if (p < 0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2, cex = 2)
}

panel.smooth <- function (x, y, col = "green", bg = NA, pch = 18, cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) {
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

custom_labels_1 <- c(
  expression(bold("Soil Tempreture [°C]")),
  expression(bold("Soil Moisture [Gram/Gram]")),
  expression(bold("Soil pH [pH]")),
  expression(bold("Nitrogen Percentage [%]")),
  expression(bold("Carbon Percentage [%]")),
  expression(bold("Soil Fresh Mass Count [Gram]"))
)

pdf("BIOMASS_correlationmatrix_SOIL_PROP.PDF", width=12, heigh=10)
par(family="serif", font = 1, cex = 1.5)
pairs(soilprop[,c(2,3,4,5,6,7)], lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist,
      labels= custom_labels_1,
      cex=2, lwd=1, las=3, cex.lab=2,cex.axis=1.25,font.lab=2,
      family="serif",
      main="Biomass of Soil vs. Soil Properties",
      cex.main=1.5)
dev.off()
