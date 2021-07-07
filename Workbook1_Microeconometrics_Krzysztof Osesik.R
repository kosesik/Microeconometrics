# Microeconometrics
# Workbook 1
# OLS
# Name: Krzysztof Osesik

rm(list=ls())
  
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

tseries::jarque.bera.test(model$residuals)

# p-value is very low, thus we can reject the H0 that the residuals are normally distributed


# 1.2 Analyze visually the relationship between residuals from the model and variables farmland and nitro
  # Farmland - what percentege of the landscape is transformed for farming
  # Nitro - how much nitrogen is used on the fields in the neighborhood



## Excercise 2: Try to find a proper functional form for this model
  # Focus on the nonlinear relationships for variables farmland and nitro (they are the focus of this analysis)
  # Of course you may also transform some other variables (for example, factor variables)
  # Try adding some interactions between variables



## Excercise 3: Check whether there are any influential observations in the model from the previous excercise




## Excercise 4: Use splines and box-cox transformations to further explore these non-linear relationships
  # Visualize splines on the plot, do they look similarly to the non-linear forms you found in Excercise 2? 




## Excercise 5: Estimate model with heteroskedasticity
  # Is variance of error term constant? If not: which variables does it depend on? 



## Summary of your findings: 


