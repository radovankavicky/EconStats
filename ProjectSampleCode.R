# ECON103: Fall Semester 2014
# Statistical Discrimination Sample Code
# Author: Kory Kantenga
# Institute: University of Pennsylvania
# Date: 1 December 2014

###########################################################################################

#Clear Workspace
rm(list=ls(all=TRUE))

#Set Directory
setwd("/Users/korydkantenga/Dropbox/Teaching/Economic_Statistics/103-601/Project/Data")

###########################################################################################


######################
#### Loading Data ####
######################

ShondalandOrig <- read.csv("shondalandsurvey.csv")


###################################
#### Summarize Wages by Gender ####
###################################

#Removing Missing Wages due to unemployed workers and students
Shondaland <- na.omit(ShondalandOrig)

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

########################################
#### Summarise Mean Wages by Age #######
########################################

#Create Table of Mean Daily Wage by Age
meanwage_age <- aggregate(dailywage ~ age + agesq, data = Shondaland, mean)

#Plot Table
pdf('meanwage_age.pdf') #Name file with plot 'meanwage_age.pdf'
plot(meanwage_age$age, meanwage_age$dailywage, type = "l", col = "red", main = "Average Daily Wage", xlab = "Age", ylab = "Mean of Daily Wage")
dev.off()

########################################
#### Histogram of Wages by Gender ######
########################################

#Create Male/Female Sub-datasets with Log Wages Only
h_men <- subset(Shondaland, female == 0 , select = logdailywage)
h_women <- subset(Shondaland, female == 1 ,select = logdailywage)

#Plot Histograms to Overlap
pdf('histogram.pdf') #Name file with plot 'histogram.pdf'
hist(h_men[,"logdailywage"], col=rgb(1,0,0,0.5), main="Overlapping Histograms Example", xlab="Log Daily Wage")
hist(h_women[,"logdailywage"], col=rgb(0,0,1,0.5), add=T)
box() #put a box around the plot
dev.off() #close and save plot

#Note: All plots will be saved in your current directory. These histograms are presenting overlapping but may be done separately.

########################################
#### Create New Education Variables ####
########################################

#Tablate Education Variable to see what categories have entries
print(t(tabulate(Shondaland$educ))) #t() command makes table output a vector for viewing

#Declare New Dummy Variables in Shondaland Dataset with zeros
Shondaland$belowHS       <- 0
Shondaland$highschool    <- 0
Shondaland$somecollege   <- 0
Shondaland$bachelors     <- 0
Shondaland$bachelorsplus <- 0

#put 1 where education is in that category
Shondaland$belowHS[which(Shondaland$educ < 73)] <- 1 
Shondaland$highschool[which(Shondaland$educ == 73)] <- 1
Shondaland$somecollege[which(Shondaland$educ >= 80 & Shondaland$educ<=110)] <- 1
Shondaland$bachelors[which(Shondaland$educ == 111)] <- 1
Shondaland$bachelors[which(Shondaland$educ >= 112)] <- 1

#Print first few observations to check variables
print(head(Shondaland))




