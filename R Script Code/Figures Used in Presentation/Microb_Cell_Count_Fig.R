#SETTING WORKING DIRECTORY
rm(list=ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")

library(readxl)
library(ggplot2)

# Read data from the Seasonal Pattern sheet
cell <- read_excel("MICR_CELL_COUNT_SUR_WATER_2 CHARTING.xlsx", sheet = 5)

plot(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygen [mg]`, type="p", xlab="Dissolved O2 [mg]", 
     ylab="Microbial Cell Count (number/mL)", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(cell$`Average of totalCellCount [number]`~cell$`Average of dissolvedOxygen [mg]`))

plot(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygenSaturation [%]`, type="p", xlab="Dissolved O2 Sat [%]", 
     ylab="Microbial Cell Density (number/mL)", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygenSaturation [%]`))

plot(cell$`Average Cell Density [number/mL]`~cell$`Average of specificConductance [microsiemens/cm]`, type="p", xlab="Specific Conductance [microsiemens/cm]", 
     ylab="Microbial Cell Density (number/mL)", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of specificConductance [microsiemens/cm]`))

plot(cell$`Average Cell Density [number/mL]`~cell$`Average of waterTemp [C]`, type="p", xlab="Water Temp [C]", 
     ylab="Microbial Cell Density (number/mL)", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of waterTemp [C]`))

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
#------------Description of the data

#Average of totalCellCount [number]
#Average of cellCountSampleVolume [mL]
#Average Cell Density [number/mL]
#Average of dissolvedOxygen [mg]
#Average of dissolvedOxygenSaturation [%]
#Average of specificConductance [microsiemens/cm]
#Average of waterTemp [C]


#Ho:  Dissolved O2, Dissolved O2 Sat, Specific Conductance, and Water temp,
#does not predict average cell density

#Ha:  Dissolved O2, Dissolved O2 Sat, Specific Conductance, and Water temp,
#does predict average cell density

# producing the graph for the correlation matrix
#The correlation matrix will be made with this function just change the bottom values

custom_labels <- c(
  expression(bold("Dissolved O2 [mg]")),
  expression(bold("Dissolved O2 Sat [%]")),
  expression(bold("Specific Conductance [µS/cm]")),
  expression(bold("Water Temp [°C]")),
  expression(bold("Microbial Cell Count [#/mL]"))
)

pdf("newest_correlationmatrix_MCD_WATER_SURFA.PDF", width=12, height=10)
par(family="serif", font = 1, cex = 1.5)
pairs(cell[,c(4,5,6,7,8)], lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist,
      labels = custom_labels,
      cex=2, lwd=1, las=3,
      cex.lab=2,cex.axis=1.25,font.lab=2,family="serif",
      main="MCD of Water vs. Correlation Factors",
      cex.main=1.5)
dev.off()

summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygen [mg]`))
summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygenSaturation [%]`))
summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of specificConductance [microsiemens/cm]`))
plot(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygen [mg]`)
plot(cell$`Average Cell Density [number/mL]`~cell$`Average of dissolvedOxygenSaturation [%]`)
plot(cell$`Average Cell Density [number/mL]`~cell$`Average of specificConductance [microsiemens/cm]`)
plot(cell$`Average Cell Density [number/mL]`~cell$`Average of waterTemp [C]`)
summary(lm(cell$`Average Cell Density [number/mL]`~cell$`Average of waterTemp [C]`))

plot(log(cell$`Average Cell Density [number/mL]`)~cell$`Average of specificConductance [microsiemens/cm]`)
summary(lm(log(cell$`Average Cell Density [number/mL]`)~log(cell$`Average of specificConductance [microsiemens/cm]`)))





  