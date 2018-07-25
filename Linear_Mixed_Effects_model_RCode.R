# This code reads in data from a csv file and fits a linear mixed effects model (fit1), producing plots and summary statistics. 
# This code also fits two additional linear mixed effects models (fit2 and fit3) that are compared to fit1.
# Code developed by Catalina Suarez-Rivera


# Clear workspace
rm(list = ls())

# Attach required libraries to run the analyses
library(lme4)
library(lmerTest)
library(lsmeans)

# Set working directory to the directory where the data sheet is saved. 
#e.g., setwd("~/Downloads")



# Section 1: load data, organize data --------------------------------

#Read in data. Data should be in long format: rows correspond to individual events within a subject.
#Columns correspond to the variables coded for each event, such as subject ID, condition, etc. 
lmer.dat <- read.csv("lme_data.csv", header=TRUE)

#Display names of variables in the data. In the sample data provided there are four variables:
#1.subID-- subject ID that generated each infant look. This is the first random factor. 
#2.duration-- duration of each infant look. This is the Dependent Variable in the analysis.
#3.object_attended-- category ID of the object attended to by the infant in each look. This is the second random factor
#4.cat_clook-- category of each infant look based on combination of overlapping parent behaviors. There are Five mutually exclusive categories (levels)
#cat_clook is the Independent variable in the analysis.
names(lmer.dat)

#Display the minimum duration of infant looks (or all row) to make sure it is at least 0.5 
min(lmer.dat$duration)

#Treat cat_clook, or the independent variable, as a categorical factor with 5 levels instead of a continuous variable.
lmer.dat$cat_clook=as.factor(lmer.dat$cat_clook)

#Document the five levels of categorical factor (cat_clook)
#cat_clook = "1" = infant look without JA 
#cat_clook = "2" = infant look With JA and no additional parent behaviors
#cat_clook = "3" = infant look with JA and touch
#cat_clook = "4" = infant look with JA and talk
#cat_clook = "5" = infant look with JA, touch and talk

# Section 2: Model fit1 --------------------------------

#Fit Linear Mixed Model (fit1): 
  #Dependent variable is natural logarithm of infant look duration to fit assumptions of normality
  #Fixed effect is cat_clook, or the category of infant look based on parent behaviors
  #Random effects are: intercepts for subjects and intercept for object_attended
fit1 <- lmer(log(duration) ~ cat_clook + (1|subID) + (1|object_attended), data = lmer.dat)

#Display output of the model including estimates for random effects and fixed effects. 
summary(fit1)

#Test significance of including fixed effects in the model 
anova(fit1)

#Plot estimated intercepts for the random effects specified: subID and object_attended
lattice::dotplot(ranef(fit1, condVar = TRUE))$subID
lattice::dotplot(ranef(fit1, condVar = TRUE))$object_attended

#Compute least squares means for the categories of infant looks obtained from the model
fit1.lsm <- lsmeans(fit1, ~ cat_clook)

#Display all pairwise comparisons between least squares means of the five categories
pairs(fit1.lsm)

#Plot estimated least squares means of the five categories with 95% confidence intervals around them
plot(lsmeans(fit1, ~cat_clook))

#Plot residuals from the model to look for normality and equal variance
qqnorm(residuals(fit1),main="")
plot(fit1)

# Section 3: Model fit2 --------------------------------

#Fit Linear Mixed Model (fit2):
  #Dependent variable is natural logarithm of infant look duration to fit assumptions of normality
  #There is not a fixed effect specified
  #Random effects are: intercepts for subjects and for object_attended
fit2 <- lmer(log(duration) ~ (1|subID) + (1|object_attended), data = lmer.dat)

#Display output from the model
summary(fit2)

#Compare fit1 and fit2 to test for additional value of fit1, which has cat_clook as a fixed effect
anova(fit1,fit2)


# Section 4: Model fit3 --------------------------------

#Fit Linear Mixed Model (fit 3) with random slopes for infants
  #Dependent variable is natural logarithm of infant look duration to fit assumptions of normality
  #Fixed effect is cat_clook, or the category of infant look based on parent behaviors
  #Random effects are: intercepts for subjects and for object_attended, and slopes for subjects
  #NOTE: the line below does not run with the sample data provided, which has only 2 subjects. With enough data, this command should run.
fit3 <- lmer(log(duration) ~ cat_clook + (1 + cat_clook|subID) +(1|object_attended), data = lmer.dat)

#Display output from the model
summary(fit3)

#Plot estimated slopes for different infants
lattice::dotplot(ranef(fit3, condVar = TRUE))$subID

#Compare fit 1 and fit3, to test for additional value of fit3, which adds random slopes for infants
anova(fit1,fit3)
