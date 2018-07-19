rm(list = ls())
library(lme4)
library(lmerTest)
library(plyr)
library(lsmeans)
#library(gmodels)
library(data.table)

##Custom working directory to directory where the data sheet is saved. 
setwd("~/Downloads")

lmer.dat <- read.csv("lme_data.csv", header=TRUE)

names(lmer.dat)

min(lmer.dat$duration)

lmer.dat$cat_clook=as.factor(lmer.dat$cat_clook)

#Document identity of categories of binary factor (cat_clook)
#cat1= infant look without JA 
#cat2= infant look With JA and no additional parent behaviors
#cat3= infant look with JA and touch
#cat4= infant look with JA and talk
#cat5= infant look with JA, touch and talk

## Model with random intercepts
fit2 <- lmer(log(duration) ~ cat_clook + (1|subID) + (1|object_attended), data = lmer.dat)
summary(fit2)
anova(fit2)
lattice::dotplot(ranef(fit2, condVar = TRUE))$subID
lattice::dotplot(ranef(fit2, condVar = TRUE))$object_attended

fit2b <- lmer(log(duration) ~ (1|subID) + (1|object_attended), data = lmer.dat)
summary(fit2b)

anova(fit2,fit2b)

fit2.lsm <- lsmeans(fit2, ~ cat_clook)
pairs(fit2.lsm)
plot(lsmeans(fit2, ~cat_clook))

qqnorm(residuals(fit2),main="")
plot(fit2)


## Model with random slopes for infants
fit3 <- lmer(log(duration) ~ cat_clook + (1 + cat_clook|subID) +(1|object_attended), data = lmer.dat)
summary(fit3)
lattice::dotplot(ranef(fit3, condVar = TRUE))$subID


fit3.lsm <- lsmeans(fit3, ~ cat_clook)
pairs(fit3.lsm)
plot(lsmeans(fit3, ~cat_clook))

qqnorm(residuals(fit3),main="")
plot(fit3)

anova(fit2,fit3)
