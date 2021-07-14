# Microeconometrics
# Workbook 3
# Count data / ordered responses
# Name:

# rm(list=ls())
# setwd("C:\\Users\\budzi\\Documents\\ME QPE\\Class 3")        # sets the access path (sometimes change the characters '\' to '/' is needed)

library(readxl)

## Exercise 1: Read femlab.rds and estimate OLS model in which no. of hours that women worked (hours) is a dependent variable
femlab <- readRDS("me.femlab.rds")
femlab <-femlab[,-c(1, 7, 22)] # removing variables that are likely to be endogenous

hist(femlab$hours)
mean(femlab$hours == 0)

# Compare estimates with tobit and poisson models

frml<-hours~kidslt6+kidsge6+age+educ+husage+faminc+unem

model_tobit<-censReg(frml, data=femlab)
summary(model_tobit)

model_poisson<-glm(frml, family=poisson(link="log"),data=femlab)
summary(model_poisson)


# Compare models' fit to data

beta_tobit<-coef(model_tobit)[1:8]
sigma_tobit<-exp(coef(model_tobit)["logSigma"])
x_tobit<-model.matrix(~kidslt6+kidsge6+age+educ+husage+faminc+unem, data=femlab)
fit_tobit<-x_tobit%*%beta_tobit


Fit_norm_tobit <- fit_tobit/sqrt(sigma_tobit)
Mills_tobit<-dnorm(Fit_norm_tobit)/pnorm(Fit_norm_tobit)
Pred_tobit <- pnorm(Fit_norm_tobit)*(fit_tobit+sqrt(sigma_tobit)*Mills_tobit)

err_tobit<-abs(femlab$hours-Pred_tobit)

err_poisson<-abs(femlab$hours-predict(model_poisson, type="response"))


mean(err_tobit, na.rm = T); mean(err_poisson)


median(err_tobit, na.rm=T); median(err_poisson)



# Summary of the result: 

# A slightly lower mean and median of the error term is offered by the Tobit model









## Exercise 2: Analyze what affects whether people care about culture in Warsaw
# rm(list=ls())

library(readxl)
Culture <- read_excel("Culture.xls")

# Wi???niewska, A., Budzi???ski, W., & Czajkowski, M. (2020). An economic valuation of access to cultural institutions: museums, theatres, and cinemas. Journal of Cultural Economics, 44(4), 563-587.

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

# 2.1 Recode variable motcultu, so that I dont know would be a third level (rather than fifth)

hist(as.numeric(Culture$motcultu))

Culture$motcultu<-factor(Culture$motcultu,
                         levels = c("1","2","5","4","3"),
                         labels=c("1","2","3","4","5"))

# 2.2 Estimate ordered logit

Culture$income<-Culture$income/1000

frml <- motcultu ~ Theatre + income + inc_miss + hhpeop + age + wars +
  factor(havejob) + factor(edugroup) + factor(havech) 
model_ordered <- ologit.reg(frml, data=Culture)
summary(model_ordered)



# 2.3. Calculate marginal effects and interpret the results

margins.oglmx(model_ordered, AME=TRUE)

# For each outcome possible (1-5) we estimated what is an influence of each variable. 
# For example, for outcome 1 ( definitely agree) it seems that the higher the education level, 
# the more likely a person is to give that answer. by the same token, for the outcome 5 (definately disagree)
# the more visits a person had to a theater the less likely they were to give answer 5.


# 2.4 Estimate model with heteroskedasticity 

model_ordered_htskd<-oglmx(motcultu ~ Theatre + income + inc_miss + hhpeop + age + wars +
                             havejob + edugroup + havech, ~ inc_miss+Theatre+havejob, data=Culture, constantMEAN = F, constantSD = F)

summary(model_ordered_htskd)





 ## Exercise 3: Conduct Travel Cost analysis for visits in theatres in Warsaw
# 3.1 Estimate a basic demand function using Poisson regression


# 3.2 Conduct linktest and test for equidispersion


# 3.3 Calculate total consumer surplus 


# 3.4 Expand the basic model to control for overdispersion and excess no. of zeros