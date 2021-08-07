# Microeconometrics
# Workbook 5
# Selection and treatment effects
# Name:

rm(list=ls())

library(psych)
library(sampleSelection)
library(stargazer)
library(censReg)

# setwd("C:\\Users\\Admin\\Documents\\ME2021\\English")        # sets the access path (sometimes change the characters '\' to '/' is needed)
usahealth <- readRDS("me.usahealth2.rds")


## Excercise 1: Estimate OLS and Tobit models in which medical expenditures (med) are explained by other covariates
# 1.1 Compare the results


### OLS

hist(usahealth$med)
frml <- med~income+age+educdec+num+ndisease+factor(female)+factor(child)+ghindx
model_lm<- lm(frml,  data=usahealth)
summary(model_lm)




### Tobit

frml
model_tobit<-censReg::censReg(frml, data=usahealth)
summary(model_tobit)


# The OLS model does not account for many zeros in the dataset, as a result it produces biased coefficients. 


# 1.2 Estimate Heckman selection models with outcome variables being either expenditures or their logarithm
# which model works better? 

frml_2<-usahealth$tookphys~income+age+educdec+num+ndisease+factor(female)+factor(child)+ghindx+physlim+mhi
model_heckman_2step<-selection(frml_2, frml, data=usahealth, method="2step")
summary(model_heckman_2step)

# model_heckman_mle<-selection(frml_2, frml, data=usahealth, method="mle")
# summary(model_heckman_mle)


usahealth$logmed<-ifelse(log(usahealth$med)==-Inf,NA,log(usahealth$med))

frml_logmed<-logmed~income+age+educdec+num+ndisease+factor(female)+factor(child)+ghindx
model_heckman_2step_logmed<-selection(frml_2, frml_logmed, data=usahealth, method="2step")
summary(model_heckman_2step_logmed)




# 1.3 For logarithm of expenditures compare models with and without exclusion restriction - you may use a "tookphys" variable 
# Does it change results? 



## Excercise 2: Investigate an effect of conservative politicians on female education

 rm(list=ls())
 library(readxl)
 
femedu <- read_excel("femedu.xls")

# EduWomen "Percentage of women aged 15-20 with high school education"
# Margin "Difference in vote share between the largest Islamic party and the largest secular party"
# Treat "= 1 if election in 1994 won by Islamic party candidate (Treat = 1 for Margin > 0)"
# partygr2 "= 1 if there were more than 2 parties in 1994 elections"
# ageshr19 - % of population below 19 in 2000
# ageshr60 - % of population above 60 in 2000 
# buyuk - metro center
# EduMen - %  of men aged 15-20 with high school education
# lpop1994 - log of population (1994)
# merkezi - district center
# merkezp - province center
# sexr - gender ratio in 2000
# shhs - household size in 2000
# subbuyuk - sub-metro center

# 2.1 Estimate OLS with the dependent variable being Eduwomen
# We are intrested in effect of Treat variable, but control for other characteristics


# 2.2 Estimate 2SLS and regression with endogenous treatment effect
# Compare results, as an instrument use partygr2 variable


# 2.3 Estimate switching regression


# 2.4 Compare results from previous models with regression discontinuity
# as a running variable use Margin 
