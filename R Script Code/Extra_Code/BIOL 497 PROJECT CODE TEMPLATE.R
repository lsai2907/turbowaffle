#------------BIOL 497 RESEARCH TEMPLATES

#SETTING WORKING DIRECTORY
setwd("C:/Users/lilha/Downloads/BIOL 497 RESEARCH")

#Clear R's brain
rm(list=ls())

#------------IMPORTING FILES
Excel_Worksheet_Name <- read.csv('Excel_Worksheet_Data_Set')
attach(Excel_Worksheet_Name)


#------------Description of the data
#EX. #LENGTH = THE LENGTH OF THE INVERTEBRATE [mm]
Factor = (description) [Unit]
#Need a null hypothesis H0 and an alternative hypothesis Ha
#Ex. Ho: Fish length does not predict fish weight
#Ex. Ha: Fish length does predict fish weight


#------------ggplot2
#Help with making graphs that are only 2 factors and helps with correlation tests
install.packages('ggplot2')

#WHAT IS THE QUESTION YOU WANT TO ASK EX.IS MAYFLY LENGTH CORRELATED WITH WATER SPEED?
#x-axis = _______
#y-axis = _______

#------------ploting the data
Figure_Of_Data_Sheet <- ggplot(Excel_Worksheet_Name, aes(x,y))
Figure_Of_Data_Sheet + geom_point() + geom_smooth(method = "lm", color = "red", se = T) +
  labs(x = "name of value [unit]", y = "name of value [unit]")

#------------basic R Plot
plot(x = 'literal name variable on x axis', y = 'literal name variable on y axis',
     xlab = "name of value [unit]", ylab = "name of value [unit]")

#Create a scatterplot with linear regression and confidence interval
#you can use base r if you want to and remember how! 
#plot the data
scatter <- ggplot(datasetname, aes(x,y))
scatter + geom_point() +labs(x = "label name", y = "label name", title="title name") 

# ----- Are Breaking Forces Normally Distributed? -----
## Create a Histogram to Verify! Include x axis labels and units!

hist(xvariable, xlab="x-axis label", main="title")

# ----- Do Breaking Forces Differ Between the 2 Species? -----
## Create Boxplots to Verify! Include y axis labels and units!
# HINT: Compare Force.N against Species

boxplot(y ~ x, xlab="x-axis label", ylab="y-axis label", main="title")

# ----- Does Diameter Affect Breaking Force? -----
## Verify Using a Scatter Plot! Include x and y axis labels and units!

#--------Pearson r----------
# You can specify what are you correlating by plugging the variable name into x and y
cor(x,y, use = "everything", method = "pearson")

#Finding the p-value
#In statistical terms, correlation is a method of assessing a 
#possible two-way linear association between two continuous variables.
cor.test(x,y, alternative = "two.sided", method = "pearson", conf.level = 0.95)

#--------Coefficient of determination-------------
# Coefficient of determination (R^2)
R2<-cor(x,y, use = "everything", method = "pearson")^2
R2  # print result

#How to Interpret?

#Pearson's product-moment correlation
#data: NO3 and Algae
#t = 1.9525, df = 23, p-value = 0.06316 (must be under 0.05 to be significant)
#alternative hypothesis: true correlation is not equal to 0 95 percent confidence interval:
#-0.02122921
#0.67206469
#sample estimates:
#cor
#0.3770646 (r value)
#R interpretation- x shows a positive/negative weak/strong correlation between the two variables
#R2 interpretation- x% of the variation of y can be explained by x variable


#------------Regression
#Use when you want to predict how one variable will change with another and describes 
#how the response variable (y) changes as a functionl of the explanatory variable (x) in y=mx+b

# install the packages you need in this lab (only install once, no need to reinstall everytime)
install.packages("car")

# load the packages
library("boot")
library("car")
library("QuantPsyc")
library("ggplot2")

#----------------------------------------
# plot the data. Can use ggplot or base r for Regression Format

#ggplot version
ggplot(yourdataname, aes(x,y))+ 
  geom_point() + 
  geom_smooth(method = "lm", color = "red", se = TRUE) + 
  labs(title= "graphtitle", x = "x-axislabel", y = "y-axislabel")+
  theme_classic()

#base r version
plot(x, y, xlab="x-axislabel", ylab="y-axislabel", main="graphtitle")
abline(lm(y~x))

#---------------------------------------------------------------------------------------
# run a regression analysis using lm() which is the linear model
# Model<-lm(outcome~predictor, data=nameofyourdataset,na.action=na.exclude)
Model<-lm(y~x, data=yourdataname, na.action=na.exclude)
#put na.fail if there are missing date, the model will fail to compute
#na.exclude/na,omit is the same thing

# To output the the results of the model we use summary()
summary(Model)

#------------How to Interpret the output of the regression

#Residuals:
#  Min    1Q     Median 3Q     Max
#-2.9482 -0.8187 0.0577 0.7106 4.1755 Intercept

#Coefficients:
#             Estimate. Sta. Error t value Pr(>absol(t))
#(Intercept) 26.72435     072297    36.97      <2e-16*** (Signif)
#Weight      0.01882      0.00111   16.96      <2e-16*** (Signif)
#Signif. codes: 0*** 0.001 *** 0.01 * 0.05. 0.11
#Residual standard error: 1.354 on 33 degrees of freedom 
#Multiple R-squared: 0.8971 (R^2), Adjusted R-squared: 0.8939 
#F-statistic: 287. on 1 and 33 DF, p-value: < 2.2e-16

#y=26.72435x+0.01882


#------------INDEPENDENT T TEST
#The Independent Samples t Test compares the means of two 
#independent groups in order to determine whether there is 
#statistical evidence that the associated population means are 
#significantly different.

# Description of the data
#male_id: ID number of male spider 
#female_age: 1 = Old adult, 2 = Young adult, 3 = Subadult
#courtship_time: Time male spent courting female (minutes)
#web_cutting_time: Time male spent cutting the female's web (minutes)

# Refer to the original paper by Waver et al., 2018 https://doi.org/10.1016/j.anbehav.2018.01.016

##QUESTION/ASSIGNMENT
# Write R code to test if the time males spent web cutting "web_cutting_time" differs between: 
# Young adult (2) and Subadult (3)

#subset so that only young adult and subadult females are in the dataset 
###can call [row #s, row names, conditional which()statement about rows,same thing w/ columns]
data_subset<-subset(datasetname, columntosubset >=2,
                    select = c("columnname1","columnname2"))
attach(data_subset)

#plot the data 
boxplot(Y~X,
        xlab = "xlabel", ylab = "ylabel", main="title here")

library(ggplot2)
ggplot(data_subset, aes(X,Y,group=groupingvariablehere))+
  geom_boxplot()+
  labs(x="label here", y="label here", title="title here")


#run an independent t test to see if web cutting differs between young and subadult 
web.model<-t.test(Y~X,
                  data=data_subset,paired=FALSE,na.action=na.exclude)


##look at the model output 
web.model



#------------DEPENDENT T TEST
#The dependent samples t-test is used 
#to compare the sample means from two related groups. 


#Data Description
#SpiderID - ID of male spider
#Age- female spider age (young and old)
#Courtship_time- time males spent courting females (hours)
#Webcutting - % time males spent cutting female webs (%)
#Somersaulting - % time males spent somersaulting when exposed to females (%)

## Data Visualization - make a plot
# we visualize t-test data using boxplots because we have one categorical variable and one numerical variable.

#The names line assigns a name to each boxplot in the order specified.
boxplot(y~x, main = "TITLE", xlab = "X AXIS LABEL", ylab = "Y AXIS LABEL")

###code for ggplot if you prefer
ggplot(DATANAME,aes(XVARIABLE,YVARIABLE))+geom_boxplot()+
  labs(x="X AXIS LABEL", y="Y AXIS LABEL", title="TITLE")+
  theme_bw()

#Make the model: pay special attention to the paired = TRUE argument
MODELNAME<-t.test(y~x,data = DATANAME, paired = TRUE,na.action = na.exclude, alternative = "two.sided")

#to see the contents of our model, just run the model name
MODELNAME


#------------ANOVA
#Analysis of variance (ANOVA) is a statistical technique 
#used to determine if the means of two or more groups are 
#significantly different from each other.


# Aspirin data

install.packages("pastecs")
install.packages("Hmisc")

library(ggplot2)
library(pastecs)
library(Hmisc)

#create the data
BodyTemp<-c(temp_values_seperated_by_commas)
dose<-gl(Number_Of_Levels,
         Number_of_samples_per_level,
         labels=c("Medium","Low","Placebo"))
aspirin<-data.frame(dose, BodyTemp)

attach(aspirin)

#Plot the data using ggplot
ggplot(aspirin, aes(x = Independent_variable,y = Dependent_Variable))+ 
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000")+  
  stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000")+ 
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633")+
  labs(x = "X axis label", y = "Y axis label", title="title")

# Carry out the ANOVA
aspirinModel<-aov(Dependent_variable~Independent_variable, data = dataname)

#output the results of the ANOVA
summary(aspirinModel)

#Pairwise T-test
# Use p-adjust method 'bonferroni'
pairwise.t.test(dependent_variable, independent_variable, p.adjust.method = "bonferroni")


#------------CHI SQUARED
#What Is a Chi-Square (χ2) Statistic? A chi-square 
#(χ2) statistic is a test that measures how a model 
#compares to actual observed data.


setwd("YOURWORKINGDIRECTORYHERE")

install.packages("gmodels")
library(gmodels)

DataName <- read.csv('FILENAMEHERE.csv',as.is=FALSE)

#change variables to "factor" type of variable
DataName$Variablename1<-as.factor(DataName$Variablename1)
DataName$Variablename2<-as.factor(DataName$Variablename2)

# Commit the headers' names into the memory of R
attach(DataName)
#-------------------------------------------------------------------------------------
#plot the data
plot(independentvariable,dependentvariable,xlab="x-axislabel", ylab="y-axislabel", main="title")

#--------------------------------------------------------------------------------------
# Run the Chi-square analysis (from the raw scores)
#CrossTable(predictor, Outcome,fisher=TRUE, chisq = TRUE, expected=TRUE, sresid=TRUE, format="SPSS")
CrossTable(independentvariable,dependentvariable, fisher=TRUE, chisq = TRUE, expected=TRUE, sresid=TRUE, format="SPSS")


#------------------------------------------------------------MULTIDIMENSIONAL SCALING TEST

#WEBSITE: https://www.geeksforgeeks.org/multidimensional-scaling-using-r/


#------------------------------------------------------------Sankey Diagram

#WEBSITE: https://rpubs.com/oomiwale1/926103 












