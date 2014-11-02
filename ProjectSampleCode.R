# ECON103: Fall Semester 2014
# Statistical Discrimination Sample Code
# Author: Kory Kantenga
# Institute: University of Pennsylvania
# Date: 2 November 2014

###########################################################################################

#Clear Workspace
rm(list=ls(all=TRUE))

#Set Directory
setwd("/Users/korydkantenga/Dropbox/Teaching/Economic_Statistics/103-601/Project/Data")

###########################################################################################


######################
#### Loading Data ####
######################

Shondaland <- read.csv("shondalandsurvey.csv")

###################################
#### Summarize Wages by Gender ####
###################################

#Removing Missing Wages due to unemployed workers and students
Shondaland <- na.omit(Shondaland)

#Table1a: Mean Wages by Gender
Table1a <- by(Shondaland[,"dailywage"],Shondaland[,"female"],mean)
print("Mean Wages by Gender")
print(Table1a)

#Table1b: Median Wages by Gender
Table1b <- by(Shondaland[,"dailywage"],Shondaland[,"female"],median)
print("Median Wages by Gender")
print(Table1b)

#Table2: Standard Deviation by Gender
Table2 <- by(Shondaland[,"dailywage"],Shondaland[,"female"],sd)
print("Standard Deviation of Wages by Gender")
print(Table2)

###################################
#### Wage Linear Regression #######
###################################

#Construct Age^2
Shondaland$agesq = Shondaland$age*Shondaland$age

#Log Wage Regression
Table3 <- summary(lm(logdailywage ~ age + agesq + factor(educ), data = Shondaland))
print(Table3)

