 # Microeconometrics
# Workbook 4
# Endogeneity
# Name:

# rm(list=ls())
# setwd("C:\\Users\\Admin\\Documents\\ME2021\\English")   

library(readxl)
library(psych)
library(statmod)
library(dummies)
library(MASS)
library(mfx)
library(oglmx)
library(pscl)
library(msm)
library(nlWaldTest)
library(glmx)
library(car)
library(lfe)
library(ivpack)
library(ivmodel)
library(REndo)
library(lava)
library(Hmisc)

Culture <- read_excel("Culture.xls")

# Description
# Theatre - no. of visits to theatres in the last 12 months
# motcultu - I care about culture in Warsaw (1- def. agree, 4 - def. disagree, 5 - I dont know)
# tc - travel cost
# cs - consumer surplus per trip (estimate from choice model)
# income (in pln)
# inc_mis = 1 if income is missing (set to average value)
# hhpeop - no. of people in household
# age
# wars - no. of years living in Warsaw
# havejob - = 1 if have a job
# edugroup - = 1 if primary, = 2 if secondary, = 3 if higher
# havech - no of children in household

## Exercise 3: Conduct Travel Cost analysis for visits in theatres in Warsaw
# 3.1 Estimate a basic demand function using Poisson regression


frml<-Theatre~motcultu+cs+income+factor(inc_miss)+hhpeop+age+wars+factor(havejob)+factor(edugroup)+factor(havech)
  
model_poisson<-glm(frml, data=Culture, family=poisson)
summary(model_poisson)



# 3.2 Conduct linktest and test for equidispersion

### LINKTEST

Culture$lambda<-model_poisson$fitted.values

Culture$xb<-log(Culture$lambda)
summary(glm(Theatre ~ xb + I(xb^2), data=Culture, family = poisson))

# the functional form is not correct, as the term I(xb^2) is significant at 0.05 level


### EQUIDISPERSION

Culture$ystar <- ((Culture$Theatre - Culture$lambda)^2 - Culture$Theatre) / Culture$lambda
model_equidis <- lm(ystar ~ lambda - 1, data=Culture)
summary(model_equidis)

hist(Culture$Theatre)
describe(Culture$Theatre) # sd equal to mean ?? -> overdispersion or not ?


 # lambda is significant, which means we probably have overdispersion in the data




# 3.3 Calculate total consumer surplus 

1/coef(model_poisson)["cs"]


# 3.4 Expand the basic model to control for overdispersion and excess no. of zeros


### NEGATIVE BINOMIAL

frml<-Theatre~motcultu+cs+income+factor(inc_miss)+hhpeop+age+wars+factor(havejob)+factor(edugroup)+factor(havech)

model_nb <- glm.nb(frml, data=Culture)
summary(model_nb)

# Theta is nearly 2 -> overdispersion ?? 1/ Theta gives 0.5 

extractAIC(model_nb); extractAIC(model_poisson)

# Negative binomial model does slightly better than regular poisson model in terms of AIC


lrtest(model_poisson, model_nb)

# LR test is significant, which means that our model with Theta (negative binomial) is statistically better than without theta



### ZERO-INFLATED MODEL

Culture$income<-Culture$income/1000

frml_z<-Theatre~motcultu+cs+income+factor(inc_miss)+hhpeop+age+wars+factor(havejob)+factor(edugroup)+factor(havech) | factor(motcultu)+age 
model_z<-zeroinfl(frml_z, data=Culture, dist="poisson")
summary(model_z)



### HURDLE MODEL

model_hurdle<-hurdle(frml_z, dist="poisson", data=Culture)
summary(model_hurdle)


# For the zero hurdle part of the summary - it looks like the less one cares about cultre in Warsaw , the less one is likely to visit a Theatre at least once 
# (coefficients) for factor (motcultu 2-5 ) are negative and generally decreasing )

model_hurdle_zero<-hurdle(frml_z, dist="poisson", zero.dist = "poisson",data=Culture)
summary(model_hurdle_zero)


extractAIC(model_hurdle); extractAIC(model_hurdle_zero) # no difference




# People with higher motcultu level are more likely to not visit Theatre at all (number of visits = 0)


######################################################################################################
# rm(list=ls())
wage <- read_excel("wage.xls")

# Description
# wage - hourly wage in cents
# lwage - log(wage)
# educ - years of schooling
# reg661 etc. - dummies for different regions
# south, south66 - = 1 if respondent from south (in 1976 or 1966)
# smsa, smsa66 - = 1 if respondent lives in the city (in 1976 or 1966)
# black - = 1 if respondent is black
# married = 1 if respondent is married
# exper - experience (age-educ-6)

# fatheduc - father's years of schooling
# motheduc - mother's years of schooling
# nearc2 - = 1 if respondent lived near 2-year college
# nearc4 - = 1 if respondent lived near 4 -year college 


## Excercise 1: Estimate wage equation to find relationship between wages and education

# 1.1 Get rid of missings observation from the data

str(wage)

wage_compl<-wage[complete.cases(wage),]

str(wage_compl)


# 1.2 Estimate OLS with lwage being a dependent variable (dont use variables fatheduc, motheduc, nearc2, nearc4) 

frml<-lwage~educ+age+black+married
model_lm<-lm(frml, data=wage_compl)
summary(model_lm)


# 1.3 Estimate to 2SLS models in which educ is endogeneous. Use either fatheduc, motheduc as instruments or nearc2, nearc4
# Comment on whether results differ from OLS

frml_sls<- lwage~educ+age+black+married | motheduc+age+black+married
model_sls <- ivreg(frml_sls, data=wage_compl)
summary(model_sls)

stargazer(model_lm,model_sls, type="text", df=FALSE)


# no endogenity ?? coeficient in the model with IV is actgually higher than in OLS (!?)



model_educ<-lm(educ~motheduc+age+black+married, data=wage_compl)
summary(model_educ)






# 1.4 Check using Hausman and Wu-Hausman tests whether the estimate models indicate endogeneity of education


### HAUSMANN

coef_diff =  coef(model_sls) - coef(model_lm)
vc_diff   =  vcov(model_sls) - vcov(model_lm)
endo_diff =  as.vector(t(coef_diff) %*% solve(vc_diff) %*% coef_diff)
pchisq(endo_diff, df=6, lower.tail = FALSE)  


# p-value is small, at the level of 0.05 level of significance, we would reject the null hypothesis that there is no endogenity in the model


### WU-HAUSMANN


summary(model_sls,diagnostics=TRUE)

# again - p-value for the Wu-Hausmann test is very small, whch leads us to reject the HO = there is no endogenity in the model





# 1.5 Use Sargan test to check whether instruments are valid and then check whether instruments are weak


frml_sargan<-lwage~educ+age+black+married | fatheduc+motheduc+age+black+married
model_sargan<-ivreg(frml_sargan, data=wage_compl)
summary(model_sargan, diagnostics=TRUE)


# p-value for the Sargan test is higher than 0.05, which does not allow us to reject the H0 that 
# our IVs are not correlated with the error term






# 1.6 Compare estimates from 2SLS models with estimates from LIML and Fuller's LIML 

# not required






## Excercise 2: Use simulation to test how control function approach works in an ordered logit case
# Evaluate estimated coefficients as well as their ratios



