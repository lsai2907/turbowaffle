# The r script for processing NEON data of cell count in water July 25, 2023.
# xxu@sdsu.edu

setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH/GIVEN DATA")
#library("")
cell <- read.table("Annual (2).txt", header=TRUE)
plot(cell$Cell_Count~cell$Lat, type="p", xlab="Latitude", ylab="Microbial cell count (1000 / ml)", cex = 1.5, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2)
summary(lm(cell$Cell_Count~cell$Lat))

plot(cell$Cell_Count~cell$Dissolved_O2, type="p", xlab="Dissolved Oxygen", ylab="Microbial cell count (1000 / ml)", cex = 1.5, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2)
summary(lm(cell$Cell_Count~cell$Dissolved_O2))

plot(cell$Cell_Count~cell$Water_Temp, type="p", xlab="Water temperature (Degree C)", ylab="Microbial cell count (1000 / ml)", cex = 1.5, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2)
summary(lm(cell$Cell_Count~cell$Water_Temp))

plot(log(cell$Cell_Count)~log(cell$Specific_Conductance), type="p", xlab="Specific Conductance", ylab="Microbial cell count (1000 / ml)", cex = 1.5, lwd = 2, las=3, cex.lab=1.5,cex.axis=1.5,font.lab=2)
summary(lm(log(cell$Cell_Count)~log(cell$Specific_Conductance)))

#Do not change this it is for the panel correlation from the internet

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt, cex = 2)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 1.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2, cex=2)
}

panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# producing the graph for the correlation matrix
#The correlation matrix will be made with this function just change the bottom values
pdf("correlationmatrix3.PDF", width=12, heigh=8.4)
pairs(cell[,c(2,3, 4,5,6)], lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist, cex=2, lwd=2, las=3, cex.lab=2,cex.axis=2,font.lab=2)
dev.off()

summary(lm(cell$Cell_Count~cell$Lat))
summary(lm(cell$Cell_Count~cell$Dissolved_O2))
summary(lm(cell$Cell_Count~cell$Water_Temp))
plot(cell$Cell_Count~cell$Lat)
plot(cell$Cell_Count~cell$Dissolved_O2)
plot(cell$Cell_Count~cell$Water_Temp)
plot(log(cell$Cell_Count)~cell$Water_Temp)
summary(lm(log(cell$Cell_Count)~log(cell$Water_Temp)))

plot(cell$Cell_Count~cell$Specific_Conductance)
plot(log(cell$Cell_Count)~log(cell$Specific_Conductance))

lm(log(cell$Cell_Count)~log(cell$Specific_Conductance))
summary(lm(log(cell$Cell_Count)~log(cell$Specific_Conductance)))

#structural equation model

library(lavaan)
model <- '
Cell_Count ~ Specific_Conductance + Water_Temp + Dissolved_O2
Specific_Conductance ~ Dissolved_O2 + Water_Temp
'

fit <- sem(model, data=na.omit(cell), std.ov=TRUE)
summary(fit,standardized=T,fit.measures =T)
standardizedSolution(fit)
fitmeasures(fit) # print key measurs for the SEM
mod_ind <- modificationindices(fit)
head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 10)   # ranking changes for improving SEM

# read in monthly data for graphing
cellmon <- read.table("Monthly.txt", header=TRUE)
mon = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
cellnum = cellmon$Cell_Count[which(cellmon$Site=="HOPB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="HOPB")])
names(cellnum) = mon
plot(cellnum, xaxt="n", ylim=c(0.1, 6.4), cex=2, lwd=2, las=2, cex.lab=1.6,cex.axis=2,font.lab=2, xlab="Month", ylab="Relative Variation of Cell Density", las=1)
axis(1, at=1:12, labels=names(cellnum), cex.axis=2)
text(6.6, 6, "Relative Cell Density (monthly cell density / annual mean cell density)", cex=1.6)

HOPBmonR = cellmon$Cell_Count[which(cellmon$Site=="HOPB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="HOPB")])
LEWImonR = cellmon$Cell_Count[which(cellmon$Site=="LEWI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LEWI")])
POSEmonR = cellmon$Cell_Count[which(cellmon$Site=="POSE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="POSE")])
BARCmonR = cellmon$Cell_Count[which(cellmon$Site=="BARC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BARC")])
FLNTmonR = cellmon$Cell_Count[which(cellmon$Site=="FLNT")] / mean(cellmon$Cell_Count[which(cellmon$Site=="FLNT")])
SUGGmonR = cellmon$Cell_Count[which(cellmon$Site=="SUGG")] / mean(cellmon$Cell_Count[which(cellmon$Site=="SUGG")])
CUPEmonR = cellmon$Cell_Count[which(cellmon$Site=="CUPE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CUPE")])
GUTLmonR = cellmon$Cell_Count[which(cellmon$Site=="GUIL")] / mean(cellmon$Cell_Count[which(cellmon$Site=="GUIL")])
CRAMmonR = cellmon$Cell_Count[which(cellmon$Site=="CRAM")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CRAM")])
LIROmonR = cellmon$Cell_Count[which(cellmon$Site=="LIRO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LIRO")])
KINGmonR = cellmon$Cell_Count[which(cellmon$Site=="KING")] / mean(cellmon$Cell_Count[which(cellmon$Site=="KING")])
MCDImonR = cellmon$Cell_Count[which(cellmon$Site=="MCDI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MCDI")])
LECOmonR = cellmon$Cell_Count[which(cellmon$Site=="LECO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LECO")])
WALKmonR = cellmon$Cell_Count[which(cellmon$Site=="WALK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="WALK")])
BLWAmonR = cellmon$Cell_Count[which(cellmon$Site=="BLWA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLWA")])
MAYFmonR = cellmon$Cell_Count[which(cellmon$Site=="MAYF")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MAYF")])
TMOBmonR = cellmon$Cell_Count[which(cellmon$Site=="TOMB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TOMB")])
PRLAmonR = cellmon$Cell_Count[which(cellmon$Site=="PRLA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRLA")])
PRPOmonR = cellmon$Cell_Count[which(cellmon$Site=="PRPO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRPO")])
ARIKmonR = cellmon$Cell_Count[which(cellmon$Site=="ARIK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="ARIK")])
BLUEmonR = cellmon$Cell_Count[which(cellmon$Site=="BLUE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLUE")])
PRINmonR = cellmon$Cell_Count[which(cellmon$Site=="PRIN")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRIN")])
BLDEmonR = cellmon$Cell_Count[which(cellmon$Site=="BLDE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLDE")])
COMOmonR = cellmon$Cell_Count[which(cellmon$Site=="COMO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="COMO")])
WLOUmonR = cellmon$Cell_Count[which(cellmon$Site=="WLOU")] / mean(cellmon$Cell_Count[which(cellmon$Site=="WLOU")])
SYCAmonR = cellmon$Cell_Count[which(cellmon$Site=="SYCA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="SYCA")])
REDBmonR = cellmon$Cell_Count[which(cellmon$Site=="REDB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="REDB")])
MARTmonR = cellmon$Cell_Count[which(cellmon$Site=="MART")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MART")])
MCRAmonR = cellmon$Cell_Count[which(cellmon$Site=="MCRA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MCRA")])
BIGCmonR = cellmon$Cell_Count[which(cellmon$Site=="BIGC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BIGC")])
TECRmonR = cellmon$Cell_Count[which(cellmon$Site=="TECR")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TECR")])
OKSRmonR = cellmon$Cell_Count[which(cellmon$Site=="OKSR")] / mean(cellmon$Cell_Count[which(cellmon$Site=="OKSR")])
TOOKmonR = cellmon$Cell_Count[which(cellmon$Site=="TOOK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TOOK")])
CARImonR = cellmon$Cell_Count[which(cellmon$Site=="CARI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CARI")])

SiteM = (HOPBmonR+LEWImonR+POSEmonR+CUPEmonR+GUTLmonR+KINGmonR+LECOmonR+WALKmonR+MAYFmonR+ARIKmonR+BLUEmonR+
           PRINmonR+BLDEmonR+COMOmonR+WLOUmonR+MARTmonR+BIGCmonR) / 17.0

#SiteM = (HOPBmonR+LEWImonR+POSEmonR+BARCmonR+FLNTmonR+SUGGmonR+CUPEmonR+GUTLmonR+CRAMmonR+LIROmonR+KINGmonR+MCDImonR+
#           LECOmonR+WALKmonR+BLWAmonR+MAYFmonR+TMOBmonR+PRLAmonR+PRPOmonR+ARIKmonR+BLUEmonR+PRINmonR+BLDEmonR+COMOmonR+
#           WLOUmonR+SYCAmonR+REDBmonR+MARTmonR+MCRAmonR+BIGCmonR+TECRmonR+OKSRmonR+TOOKmonR+CARImonR) / 34.0

#points(cellmon$Cell_Count[which(cellmon$Site=="HOPB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="HOPB")]))
points(cellmon$Cell_Count[which(cellmon$Site=="LEWI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LEWI")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="POSE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="POSE")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="BARC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BARC")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="FLNT")] / mean(cellmon$Cell_Count[which(cellmon$Site=="FLNT")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="SUGG")] / mean(cellmon$Cell_Count[which(cellmon$Site=="SUGG")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="CUPE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CUPE")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="GUIL")] / mean(cellmon$Cell_Count[which(cellmon$Site=="GUIL")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="CRAM")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CRAM")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="LIRO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LIRO")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="KING")] / mean(cellmon$Cell_Count[which(cellmon$Site=="KING")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="MCDI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MCDI")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="LECO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LECO")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="WALK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="WALK")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="BLWA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLWA")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="MAYF")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MAYF")]), cex=2, cex.lab=2,lwd=2)

points(cellmon$Cell_Count[which(cellmon$Site=="TOMB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TOMB")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="PRLA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRLA")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="PRPO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRPO")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="ARIK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="ARIK")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="BLUE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLUE")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="PRIN")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRIN")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="BLDE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLDE")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="COMO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="COMO")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="WLOU")] / mean(cellmon$Cell_Count[which(cellmon$Site=="WLOU")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="SYCA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="SYCA")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="REDB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="REDB")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="MART")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MART")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="MCRA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MCRA")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="BIGC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BIGC")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="TECR")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TECR")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="OKSR")] / mean(cellmon$Cell_Count[which(cellmon$Site=="OKSR")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="TOOK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TOOK")]), cex=2, cex.lab=2,lwd=2)
points(cellmon$Cell_Count[which(cellmon$Site=="CARI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CARI")]), cex=2, cex.lab=2,lwd=2)
lines(SiteM, col="red", lwd=4)
abline(h=1, col = "blue", lwd=2)
legend(3.5, 5.5, legend=c("Multiple site average","Annual average"), col=c("red","blue"), lwd=3, lty=c(1,1),cex=1.8)

# correlation analysis at monthly scale
#plot(cellmon$Dissolved_O2[which(cellmon$Site=="LEWI")], cellmon$Cell_Count[which(cellmon$Site=="LEWI")])
#plot(cellmon$Water_T[which(cellmon$Site=="LEWI")], cellmon$Cell_Count[which(cellmon$Site=="LEWI")])
#plot(cellmon$Specific_Conductance[which(cellmon$Site=="LEWI")], cellmon$Cell_Count[which(cellmon$Site=="LEWI")])


# correlation analysis at monthly scale
cellmon_noNA <- na.omit(cellmon)

lm.out <- lm(cellmon_noNA$Cell_Count ~ cellmon_noNA$Dissolved_O2)
plot(cellmon_noNA$Dissolved_O2, cellmon_noNA$Cell_Count, cex=2, lwd=2, las=2, cex.lab=1.6,cex.axis=2,font.lab=2, xlab="Dissolved O2 (mg/L)", ylab="Cell count (#/mL)", las=3)
#newx = seq(min(cellmon_noNA$Dissolved_O2), max(cellmon_noNA$Dissolved_O2),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=cellmon_noNA$Dissolved_O2), interval="confidence",level = 0.95)
#plot(cellmon$Dissolved_O2, cellmon$Cell_Count, xlab="x", ylab="y", main="Regression")
abline(lm.out, col="red", lwd=3)
lines(cellmon_noNA$Dissolved_O2, conf_interval[,2], col="blue", lty=2)
lines(cellmon_noNA$Dissolved_O2, conf_interval[,3], col="blue", lty=2)
cor.test(cellmon$Dissolved_O2, cellmon$Cell_Count)
text(15, 8500, "r = -0.100; p = 0.06528", cex=2)#text()


lm.out <- lm(cellmon_noNA$Cell_Count ~ cellmon_noNA$Water_T)
plot(cellmon$Water_T, cellmon$Cell_Count, cex=2, lwd=2, las=2, cex.lab=1.6,cex.axis=2,font.lab=2, xlab="Water Temperature (C)", ylab="Cell count (#/mL)", las=3)
conf_interval <- predict(lm.out, newdata=data.frame(x=cellmon_noNA$Water_T), interval="confidence",level = 0.95)
#plot(cellmon$Dissolved_O2, cellmon$Cell_Count, xlab="x", ylab="y", main="Regression")
abline(lm.out, col="red", lwd=3)
lines(cellmon_noNA$Water_T, conf_interval[,2], col="blue", lty=2)
lines(cellmon_noNA$Water_T, conf_interval[,3], col="blue", lty=2)
cor.test(cellmon$Water_T, cellmon$Cell_Count)
text(20, 8500, "r = 0.1201; p = 0.02751", cex=2)#text()


lm.out <- lm(log(cellmon_noNA$Cell_Count) ~ log(cellmon_noNA$Specific_Conductance))
plot(log(cellmon$Specific_Conductance), log(cellmon$Cell_Count), cex=2, lwd=2, las=2, cex.lab=1.5,cex.axis=2,font.lab=2, xlab="log(Specific conductance (uS/cm))", ylab="log(Cell count (#/mL))", las=3)
conf_interval <- predict(lm.out, newdata=data.frame(x=log(cellmon_noNA$Specific_Conductance)), interval="confidence",level = 0.95)
#plot(cellmon$Dissolved_O2, cellmon$Cell_Count, xlab="x", ylab="y", main="Regression")
abline(lm.out, col="red", lwd=3)
lines(log(cellmon_noNA$Specific_Conductance), conf_interval[,2], col="blue", lty=2)
lines(log(cellmon_noNA$Specific_Conductance), conf_interval[,3], col="blue", lty=2)
cor.test(log(cellmon$Specific_Conductance), log(cellmon$Cell_Count))
text(4, 8.6, "r = 0.2288; p < 0.0001", cex=2)#text()



cor.test(cellmon$Water_T, cellmon$Cell_Count)
cor.test((cellmon$Specific_Conductance), (cellmon$Cell_Count))
cor.test(log(cellmon$Specific_Conductance), log(cellmon$Cell_Count))

regression.data = summary(lm.out) #save regression summary as variable
names(regression.data) #get names so we can index this data
a= regression.data$coefficients["(Intercept)","Estimate"] #grab values
b= regression.data$coefficients["x","Estimate"]
abline(a,b) #add the regression line

# Library
#library(ggplot2)
#library(hrbrthemes)

# https://r-graph-gallery.com/50-51-52-scatter-plot-with-ggplot2.html



#points(cellmon$Cell_Count[which(cellmon$Site=="HOPB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="HOPB")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="LEWI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LEWI")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="POSE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="POSE")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="BARC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BARC")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="FLNT")] / mean(cellmon$Cell_Count[which(cellmon$Site=="FLNT")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="SUGG")] / mean(cellmon$Cell_Count[which(cellmon$Site=="SUGG")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="CUPE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CUPE")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="GUIL")] / mean(cellmon$Cell_Count[which(cellmon$Site=="GUIL")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="CRAM")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CRAM")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="LIRO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LIRO")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="KING")] / mean(cellmon$Cell_Count[which(cellmon$Site=="KING")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="MCDI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MCDI")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="LECO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="LECO")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="WALK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="WALK")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="BLWA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLWA")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="MAYF")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MAYF")]))

lines(cellmon$Cell_Count[which(cellmon$Site=="TOMB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TOMB")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="PRLA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRLA")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="PRPO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRPO")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="ARIK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="ARIK")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="BLUE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLUE")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="PRIN")] / mean(cellmon$Cell_Count[which(cellmon$Site=="PRIN")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="BLDE")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BLDE")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="COMO")] / mean(cellmon$Cell_Count[which(cellmon$Site=="COMO")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="WLOU")] / mean(cellmon$Cell_Count[which(cellmon$Site=="WLOU")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="SYCA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="SYCA")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="REDB")] / mean(cellmon$Cell_Count[which(cellmon$Site=="REDB")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="MART")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MART")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="MCRA")] / mean(cellmon$Cell_Count[which(cellmon$Site=="MCRA")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="BIGC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BIGC")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="TECR")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TECR")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="OKSR")] / mean(cellmon$Cell_Count[which(cellmon$Site=="OKSR")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="TOOK")] / mean(cellmon$Cell_Count[which(cellmon$Site=="TOOK")]))
lines(cellmon$Cell_Count[which(cellmon$Site=="CARI")] / mean(cellmon$Cell_Count[which(cellmon$Site=="CARI")]))


plot(cellmon$Cell_Count[which(cellmon$Site=="BIGC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BIGC")]))

lines(cellmon$Cell_Count[which(cellmon$Site=="BIGC")] / mean(cellmon$Cell_Count[which(cellmon$Site=="BIGC")]))

# plot a quantiative variable against a string x-axis
x=c("X11", "X13", "X16", "X17")
y=c(0.8, .9, .87, .84)
names(y)=x
plot(y, xaxt="n")
axis(1, at=1:4, labels = names(y))
#


# site name: 
#"HOPB" "LEWI" "POSE" "BARC" "FLNT" "SUGG" "CUPE" "GUIL"
#"CRAM" "LIRO" "KING" "MCDI" "LECO" "WALK" "BLWA" "MAYF" 
#"TOMB" "PRLA" "PRPO" "ARIK" "BLUE" "PRIN" "BLDE" "COMO"
#"WLOU" "SYCA" "REDB" "MART" "MCRA" "BIGC" "TECR" "OKSR"
#"TOOK" "CARI"


