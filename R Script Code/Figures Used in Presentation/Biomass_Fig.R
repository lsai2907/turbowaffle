#BIOMASS FIG

#SETTING WORKING DIRECTORY
rm(list=ls())
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/SIMPLIFIED_DATA")
library(readxl)
library(ggplot2)

biomass <- read_excel("BIOMASS CHARTING.xlsx", sheet = 1)

plot(biomass$`Average of freezeDryMass [Gram]`~biomass$`Average of totalLipidScaledConcentration [nanomolesPerGram]`, type="p", xlab="Lipid Concentration [nanomoles/gram]", 
     ylab="Freeze Dry Mass [gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(biomass$`Average of freezeDryMass [Gram]`~biomass$`Average of totalLipidScaledConcentration [nanomolesPerGram]`))

plot(biomass$`Average of freezeDryMass [Gram]`~biomass$`Average of lipidInternalStandardResponse [2000picoAmpSecond]`, type="p", xlab="Lipid Internal Standard Response [2000picoAmpSecond]", 
     ylab="Freeze Dry Mass [gram]", cex = 2, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2,family="serif")
summary(lm(biomass$`Average of freezeDryMass [Gram]`~biomass$`Average of lipidInternalStandardResponse [2000picoAmpSecond]`))

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

panel.smooth <- function (x, y, col = "bisque2", bg = NA, pch = 18, cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) {
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
  rect(breaks[-nB], 0, breaks[-1], y, col = "chartreuse3")
}

custom_labels <- c(
  expression(bold("Freeze Dry Mass [Gram]")),
  expression(bold("Total Lipid Concentration [Nanomoles/Gram")),
  expression(bold("Lipid Standard Response [2000picoAmp/Second]"))
)
  
pdf("SCREEN_correlationmatrix_BIOMASS.PDF", width=16, height=13)
par(family="serif", font = 1, cex = 2)
pairs(biomass[,c(2,3,4)], lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist,
      labels = custom_labels,
      cex=2, lwd=1, las=3, cex.lab=2,cex.axis=1.25,font.lab=2,family="serif",
      main="BIOMASS vs. Lipids",
      cex.main=1.5)
dev.off()
