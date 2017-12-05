#install package car
install.packages("car")
install.packages("ggplot2")

#load libraries
library("dplyr")
library("car")
library("ggplot2")

#set working directory
setwd("/Users/cavaughan99/Documents/work")

#call up dataframe
phresh <- read.csv("phreshdata.csv")

#view contents of dataframe
View(phresh)

#examine types and distributions of predictor variables
#sociodemographic characteristics
hist(phresh$age)
class(phresh$age)

table(phresh$male)
class(phresh$male)

hist(phresh$i_adj_inc2)
class(phresh$i_adj_inc2)

table(phresh$edu_college)
class(phresh$edu_college)

table(phresh$traccs)
class(phresh$traccs)

table(phresh$i_received_stamps)
class(phresh$i_received_stamps)

table(phresh$married)
class(phresh$married)

table(phresh$any_kids)
class(phresh$any_kids)

hist(phresh$nhh)
class(phresh$nhh)

#store types: convert variables from factor to integer 
phresh$convst <- as.integer(phresh$FOOD_PURC2A)
phresh$nhoodst <- as.integer(phresh$FOOD_PURC2B)
phresh$dollarst <- as.integer(phresh$FOOD_PURC2C)
phresh$dsgrocst <- as.integer(phresh$FOOD_PURC2D)
phresh$suprcnst <- as.integer(phresh$FOOD_PURC2E)
phresh$whslclst <- as.integer(phresh$FOOD_PURC2F)
phresh$spgrocst <- as.integer(phresh$FOOD_PURC2G)
phresh$fullsvst <- as.integer(phresh$FOOD_PURC2H)
phresh$fnvst <- as.integer(phresh$FOOD_PURC2J)

#examine distributions of dietary outcomes
hist(phresh$avg_ssb_kcal_t)
hist(phresh$avg_add_sug_t)
hist(phresh$avg_discfat_t2)
hist(phresh$avg_fnv_t)

##REGRESSION MODELS
#for each of 4 dietary outcomes, run 3 reg models: 
#1) only sociodemographic characteristics as predictors
#2) only frequency of shopping at different store types as predictors
#3) sociodemographic characteristics and frequency of shopping at different store types as predictors

#create object to abbreviate representation of sociodemographic characteristics in 1st reg model
sociodem <- "age + male + i_adj_inc2 + edu_college +
traccs + i_received_stamps + married + any_kids + nhh"

#create object to abbreviate representation of store types in 2nd reg model
storetyp <- "convst + nhoodst + dollarst + dsgrocst + 
suprcnst + whslclst + spgrocst + fullsvst + fnvst"

#create object to abbreviate representation of sociodemographic chars and store types in 3rd reg model
bothpred <- "age + male + i_adj_inc2 + edu_college +
                traccs + i_received_stamps + married + any_kids + nhh +
                convst + nhoodst + dollarst + dsgrocst + 
             suprcnst + whslclst + spgrocst + fullsvst + fnvst"

#SSB KCALS

   #LINEAR REG MODEL WITH ONLY SOCIODEMOGRAPHIC CHARACTERISTICS AS PREDICTORS

ssbsocio <- lm(as.formula(paste0("avg_ssb_kcal_t ~", sociodem)), data=phresh)

summary(ssbsocio)

   #LINEAR REG MODEL WITH ONLY FREQUENCY OF SHOPPING AT DIFFERENT STORE TYPES AS PREDICTORS

ssbstore <- lm(as.formula(paste0("avg_ssb_kcal_t ~", storetyp)), data=phresh)

summary(ssbstore)

   #LINEAR REG MODEL WITH BOTH SOCIODEMOGRAPHIC CHARACTERISTICS AND STORE TYPES

ssbboth <- lm(as.formula(paste0("avg_ssb_kcal_t ~", bothpred)), data=phresh)

summary(ssbboth)

#extract R^2 values from SSB models for comparison
print(summary(ssbsocio)$r.squared)
print(summary(ssbstore)$r.squared)
print(summary(ssbboth)$r.squared)

#ADDED SUGARS (tsps)

   #LINEAR REG MODEL WITH ONLY SOCIODEMOGRAPHIC CHARACTERISTICS AS PREDICTORS

addsocio <- lm(as.formula(paste0("avg_add_sug_t ~", sociodem)), data=phresh)

summary(addsocio)

   #LINEAR REG MODEL WITH ONLY FREQUENCY OF SHOPPING AT DIFFERENT STORE TYPES AS PREDICTORS

addstore <- lm(as.formula(paste0("avg_add_sug_t ~", storetyp)), data=phresh)

summary(addstore)

   #LINEAR REG MODEL WITH BOTH SOCIODEMOGRAPHIC CHARACTERISTICS AND FREQUENCY OF SHOPPING AT
       #DIFFRENT STORE TYPES AS PREDICTORS

addboth <- lm(as.formula(paste0("avg_add_sug_t ~", bothpred)), data=phresh)

summary(addboth)

#extract added sugar model R^2 values for comparison

#r-squared as percentages
addsocior2 <- summary(addsocio)$r.squared
addsocior2pct <- round(addsocior2*100, digits=0)
print(addsocior2pct)

addstorer2 <- summary(addstore)$r.squared
addstorer2pct <- round(addstorer2*100, digits=0)
print(addstorer2pct)

addbothr2 <- summary(addboth)$r.squared
addbothr2pct <- round(addbothr2*100, digits=0)
print(addbothr2pct)

#r-squared
print(summary(addsocio)$r.squared)
print(summary(addstore)$r.squared)
print(summary(addboth)$r.squared)

#DISCRETIONARY FATS (g)

   #SOCIODEMOGRAPHIC CHARACTERISTICS

discsocio <- lm(as.formula(paste0("avg_discfat_t2 ~", sociodem)), data=phresh)

summary(discsocio)

   #FREQUENCY OF SHOPPING AT DIFFERENT STORE TYPES

discstore <- lm(as.formula(paste0("avg_discfat_t2 ~", storetyp)), data=phresh)

summary(discstore)

   #FULL MODEL: SOCIODEMOGRAPHIC CHARACTERISTICS + STORE TYPES

discboth <- lm(as.formula(paste0("avg_discfat_t2 ~", bothpred)), data=phresh)

summary(discboth)

#extract discretionary fats model R^2 values for comparison
print(summary(discsocio)$r.squared)
print(summary(discstore)$r.squared)
print(summary(discboth)$r.squared)

#FRUIT AND VEGETABLE INTAKE (cups)

   #SOCIODEMOGRAPHIC CHARACTERISTICS

fnvsocio <- lm(as.formula(paste0("avg_fnv_t ~", sociodem)), data=phresh)

summary(fnvsocio)

   #FREQUENCY OF SHOPPING AT DIFFERENT STORE TYPES

fnvstore <- lm(as.formula(paste0("avg_fnv_t ~", storetyp)), data=phresh)

summary(fnvstore)

   #FULL MODEL: SOCIODEMOGRAPHIC CHARACTERISTICS + STORE TYPES

fnvboth <- lm(as.formula(paste0("avg_fnv_t ~", bothpred)), data=phresh)

summary(fnvboth)

#R^2 values as percentages
#sociodemographic chars
fnvsocior2 <- summary(fnvsocio)$r.squared
fnvsocior2pct <- round(fnvsocior2*100, digits=0)
print(fnvsocior2pct)

#store types
fnvstorer2 <- summary(fnvstore)$r.squared
fnvstorer2pct <- round(fnvstorer2*100, digits=0)
print(fnvstorer2pct)

#both
fnvbothr2 <- summary(fnvboth)$r.squared
fnvbothr2pct <- round(fnvbothr2*100, digits=0)
print(fnvbothr2pct)

#extract F and V intake model R^2 values for comparison
print(summary(fnvsocio)$r.squared)
print(summary(fnvstore)$r.squared)
print(summary(fnvboth)$r.squared)

#EXAMINE ASSUMPTIONS OF LINEAR REGRESSION

#EXAMINE DISTRIBUTIONS OF RESIDUALS FROM REG MODELS TO ASSESS TENABILITY OF NORMALITY ASSUMPTION

#SOCIODEMOGRAPHIC CHARACTERISTICS
hist(resid(ssbsocio))
hist(resid(addsocio))
hist(resid(discsocio))
hist(resid(fnvsocio))

#FREQUENCY OF SHOPPING AT DIFFERENT STORE TYPES
hist(resid(ssbstore))
hist(resid(addstore))
hist(resid(discstore))
hist(resid(fnvstore))

#FULL MODEL: SOCIODEMOGRAPHIC CHARACTERISTICS + STORE TYPES
hist(resid(ssbboth))
hist(resid(addboth))
hist(resid(discboth))
hist(resid(fnvboth))

#EXAMINE MULTICOLLINEARITY IN REG MODELS: look for VIF > 10 or average VIF substantially > 1
#SOCIODEMOGRAPHIC CHARACTERISTICS
vif(ssbsocio)
mean(vif(ssbsocio))
vif(addsocio)
mean(vif(addsocio))
vif(discsocio)
mean(vif(discsocio))
vif(fnvsocio)
mean(vif(fnvsocio))

#FREQUENCY OF SHOPPING AT DIFFERENT STORE TYPES
vif(ssbstore)
mean(vif(ssbstore))
vif(addstore)
mean(vif(addstore))
vif(discstore)
mean(vif(discstore))
vif(fnvstore)
mean(vif(fnvstore))

#FULL MODEL: SOCIODEMOGRAPHIC CHARACTERISTICS + STORE TYPES
vif(ssbboth)
mean(vif(ssbboth))
vif(addboth)
mean(vif(addboth))
vif(discboth)
mean(vif(discboth))
vif(fnvboth)
mean(vif(fnvboth))





