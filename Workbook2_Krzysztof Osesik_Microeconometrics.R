# Microeconometrics
# Workbook 2
# GLMs
# Name: Krzysztof Osesik

# rm(list=ls())
# setwd("C:\\Users\\Admin\\Documents\\ME2021\\English")   

hedonic <- readRDS("me.hedonic.rds")

#  Continue to work on the model you developed in the previous class (Workbook 1)

## Excercise 1: Estimate GLM models for hedonic data
# Compare inverse-gaussian distribution and gamma


hedonic$price<-exp(hedonic$logprice)

###

model_glm_gaussian<-glm(price~age+factor(repair)+factor(rooms)+lot+factor(county)+
                          vacant+population+avincome+farmland+nitro,
                        family=gaussian,
                        data=hedonic)


summary(model_glm_gaussian)



###

model_glm_gaussian_log<-glm(price~age+factor(repair)+factor(rooms)+lot+factor(county)+
                          vacant+population+avincome+farmland+nitro,
                        family=gaussian (link="log"),
                        data=hedonic)


summary(model_glm_gaussian_log)


###

model_glm_inv_gaussian_log<-glm(price~age+factor(repair)+factor(rooms)+lot+factor(county)+
                              vacant+population+avincome+farmland+nitro,
                            family=inverse.gaussian (link="log"),
                            data=hedonic)


summary(model_glm_inv_gaussian_log)



###


model_glm_gamma_log<-glm(price~age+factor(repair)+factor(rooms)+lot+factor(county)+
                              vacant+population+avincome+farmland+nitro,
                            family=Gamma (link="log"),
                            data=hedonic)


summary(model_glm_gamma_log)




extractAIC(model_glm_gaussian)
extractAIC(model_glm_gaussian_log)
extractAIC(model_glm_inv_gaussian_log)
extractAIC(model_glm_gamma_log)


# the lowest AIC is offered by the gaussian family model with "log" link.


# Conduct basic diagnostics for both of these models and calculate influence measures to find whether there are any influential observations

qqnorm(resid(model_glm_gaussian, type="pearson"))
qqline(resid(model_glm_gaussian, type="pearson"))

qqnorm(resid(model_glm_gaussian_log, type="pearson"))
qqline(resid(model_glm_gaussian_log, type="pearson"))

qqnorm(resid(model_glm_inv_gaussian_log, type="pearson"))
qqline(resid(model_glm_inv_gaussian_log, type="pearson"))

qqnorm(resid(model_glm_gamma_log, type="pearson"))
qqline(resid(model_glm_gamma_log, type="pearson"))


# it seems the best fir is offered by the gaussian family model (with and without the "log" link)


im <- influence.measures(model_glm_gaussian)
colSums(im$is.inf)








## Excercise 2: Estimate Quantile regression model

# NOT REQUIRED



 # estimate models for different quantiles
 # analyze how parameters change between different quantiles


  # test whether these are significant differences between parameters for different quantiles
  # visualize the differences on the plot







## Excercise 3: Read femlab.rds and estimate OLS model in which no. of hours that women worked (hours) is a dependent variable

# NOT REQUIRED


rm(list=ls())
setwd("C:\\Users\\Admin\\Documents\\ME2021\\English")        # sets the access path (sometimes change the characters '\' to '/' is needed)
femlab <- readRDS("me.femlab.rds")
femlab <-femlab[,-c(1, 7, 22)] # remove variables that are likely to be endogenous

hist(femlab$hours)
mean(femlab$hours == 0)

# Compare estimates with tobit and poisson models


# Compare models' fit to data




