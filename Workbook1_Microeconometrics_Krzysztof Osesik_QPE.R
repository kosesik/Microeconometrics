# Microeconometrics
# Workbook 1
# OLS
# Name: Krzysztof Osesik

# rm(list=ls())

library(tidyverse)
library(jtools)


hedonic <- readRDS("me.hedonic.rds")

## Hedonic analysis of external effects of farming

## Excercise 1:  Estimate a basic regression model in which logarithm of house prices is explained by the other variables
# Aim of the analysis is to find what is a relationship between Price and variables Farmland and Nitro - this is likely to be nonlinear

hedonic$logprice<-exp(hedonic$logprice)

hedonic<-rename(hedonic, price=logprice)

model<-lm(price~.,data=hedonic)

summary(model)


# 1.1 Check whether assumptions of OLS are valid for this model

lmtest::resettest(model)

# p-value is very low, thus we can reject the H0 that the linear form is correct

lmtest::bptest(model)

# p-value is very low, thus we can reject the H0 that the error term is homoscedastic

plot(model$residuals)
rcompanion::plotNormalHistogram(model$residuals)

tseries::jarque.bera.test(model$residuals)

# p-value is very low, thus we can reject the H0 that the residuals are normally distributed


# 1.2 Analyze visually the relationship between residuals from the model and variables farmland and nitro
# Farmland - what percentege of the landscape is transformed for farming
# Nitro - how much nitrogen is used on the fields in the neighborhood


plot(hedonic$farmland,model$residuals)
stats::scatter.smooth(rstandard(model)~hedonic$farmland, col="grey")


plot(hedonic$nitro,model$residuals)
stats::scatter.smooth(rstandard(model)~hedonic$nitro, col="grey")






## Excercise 2: Try to find a proper functional form for this model
# Focus on the nonlinear relationships for variables farmland and nitro (they are the focus of this analysis)
# Of course you may also transform some other variables (for example, factor variables)
# Try adding some interactions between variables



plot(hedonic$farmland, hedonic$price)
plot(hedonic$nitro, hedonic$price)

# MODEL 2 (log of price)

hedonic$logprice<-log(hedonic$price)

model_2<-lm(logprice~.-price,data=hedonic) # independent variable is everything except for the price variable

summary(model_2)

# after taking a log of price, the R-squared did not improve by much 


lmtest::resettest(model_2) # p-value still <0.05



##################################



plot(hedonic$farmland, hedonic$logprice)
plot(hedonic$nitro, hedonic$logprice)

# MODEL 3 

hedonic$lognitro<-log(hedonic$nitro)

hedonic$lognitro[hedonic$lognitro=="-Inf"]<-0

model_3<-lm(logprice~.-nitro -price,data=hedonic) # independent variable is everything except for the price variable

summary(model_3)

# after taking a log of price, the R-squared did not improve by much 


lmtest::resettest(model_3) # p-value still <0.05


###################

plot(hedonic$lognitro, hedonic$logprice)

# MODEL 4 (log of price, log of nitro, factors)

summary(hedonic)

model_4<-lm(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+farmland+lognitro,data=hedonic)

summary(model_4)

lmtest::resettest(model_4) # p-value still <0.05



#############

jtools::plot_summs(model_4, coefs=c("factor(rooms)2","factor(rooms)3","factor(rooms)4","factor(rooms)5",
                                    "factor(rooms)6","factor(rooms)7")) 


# the line on the plot does not seem to be stright - let us keep the factor for the rooms variable

stats::scatter.smooth(rstandard(model_4)~hedonic$farmland, col="grey") # some curvature for farmland is detectable

stats::scatter.smooth(rstandard(model_4)~hedonic$lognitro, col="grey")



# MODEL 5 (MODEL 4 + square of farmland)

hedonic$farmland_sq<-(hedonic$farmland/10)^2

model_5<-lm(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+farmland_sq+lognitro,data=hedonic)

summary(model_5)

lmtest::resettest(model_5) # p-value still <0.05
lmtest::resettest(model) # there is some improvement as compared to the first model -> RESET statistics dropped from 24.4 to 17.9


###############

stats::scatter.smooth(rstandard(model_5)~hedonic$farmland_sq, col="grey") # some curvature for farmland is detectable

stats::scatter.smooth(rstandard(model_5)~hedonic$lognitro, col="grey")

# MODEL 6 (MODEL 5+bins)

hedonic$farmland_sq_bins<-mltools::bin_data(hedonic$farmland_sq, bins=10)
hedonic$lognitro_bins<-mltools::bin_data(hedonic$lognitro, bins=10)

hedonic$farmland_sq_bins<-unclass(hedonic$farmland_sq_bins)
hedonic$lognitro_bins<-unclass(hedonic$lognitro_bins)


model_6<-lm(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+
              factor(farmland_sq_bins)+factor(lognitro_bins),data=hedonic)

summary(model_6)

lmtest::resettest(model_6) # p-value still <0.05
lmtest::resettest(model) #

stats::scatter.smooth(rstandard(model_5)~hedonic$farmland_sq_bins, col="grey") # some curvature for farmland is detectable

stats::scatter.smooth(rstandard(model_5)~hedonic$lognitro_bins, col="grey")


# Creating bins out of farmland and nitro variables did not help the model, let's return to MODEL 5 as it offered better results


#######

# MODEL 7 (MODEL 5 + interactions)

model_7<-lm(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+farmland_sq:rooms+lognitro:rooms,data=hedonic)

summary(model_7)


lmtest::resettest(model_7) # p-value still <0.05
lmtest::resettest(model_5)

# Adding interactions actually did not help the specification of the model
# The best model is MODEL 5 (lowest RESET value from RESET test)




## Excercise 3: Check whether there are any influential observations in the model from the previous excercise


im<-influence.measures(model_3)
colSums(im$is.inf)


# no influential observations



## Excercise 4: Use splines and box-cox transformations to further explore these non-linear relationships
# Visualize splines on the plot, do they look similarly to the non-linear forms you found in Excercise 2? 


##### BOX-COX


bx<-MASS::boxcox(model_5, lambda=seq(-5, 5, length=200), data=hedonic)
XVal<-bx[["x"]]
BCmax<-XVal[which(bx[["y"]] == max(bx[["y"]]))]


hedonic$priceBC<-(hedonic$price^(BCmax)-1)/BCmax


model_8<-lm(priceBC~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+farmland_sq+lognitro,data=hedonic)

summary(model_8)

lmtest::resettest(model_8)


# to apply box cox transformation to dependent variable did not help the specification of the model


##### SPLINES


model_9<-lm(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+
              ns(farmland_sq, df=10)+ns(lognitro,df=10),data=hedonic)

summary(model_9)

lmtest::resettest(model_9) #


# B-SPLINE


model_10<-lm(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+
              bs(farmland, df=33)+bs(nitro,df=33),data=hedonic)

summary(model_10)

lmtest::resettest(model_10) #


spline <- model_10$model$`bs(farmland, df = 33)` %*% model_10$coefficients[(2:34)]
plot(hedonic$farmland_sq, spline)



# SMOOTH_SPLINE


model_11<-gam(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+vacant+population+avincome+
               s(farmland)+s(lognitro),data=hedonic)

summary(model_11)

lmtest::resettest(model_11) #




# Including splines for farmland and nitro does not seem to improve the model




## Excercise 5: Estimate model with heteroskedasticity
# Is variance of error term constant? If not: which variables does it depend on? 



X_m<-model.matrix(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+
                    vacant+population+avincome+farmland+lognitro, data=hedonic)[,-1]


X_s<-model.matrix(logprice~age+factor(repair)+factor(rooms)+lot+factor(county)+
                    vacant+population+avincome+farmland+lognitro, data=hedonic)[,-1]


model_htscd<-lmvar(hedonic$logprice, X_mu=X_m, X_sigma = X_s)


summary(model_htscd)



# For heteroskedasticity analysis, it turns out that age, repari and county variables are affecting the variance
# of the error term. The variables were found to be significant in terms of affecting the variance of the error term. 
# The higher the age, the higher is the variance of the error term. In contrast, the lower repair and county 
# variables ( either 0 or 1 - both are factors), the higher the variance of the error term.






