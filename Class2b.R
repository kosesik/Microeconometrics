# Microeconometrics
# CLASS 2b
# Dealing with zero responses in continous variables
# Wiktor Budzinski, Marek Giergiczny

################################################# Excercise 5 ########################################

## Reading data and packages
rm(list=ls())

# setwd("C:\\Users\\budzi\\Documents\\ME QPE\\Class 2")        # sets the access path (sometimes change the characters '\' to '/' is needed)
usahealth <- readRDS("me.usahealth.rds")
View(usahealth)

library(psych)
library(censReg)
library(stargazer)
library(mfx)

describe(usahealth)
#options(scipen=999) # disable scientific notation

# Scalling variables 
usahealth$med <- usahealth$med/100
usahealth$coins <- usahealth$coins/100
hist(usahealth$med)

## Models
Eq1 <- med ~ factor(coins) + black + income + age + female + educdec + num + child + femchild + mhi + ndisease + physlim + ghindx + hlthg + hlthf + hlthp
model1 = lm(Eq1, data=usahealth) # OLS
summary(model1)

usahealth$logmed<-log(usahealth$med + 0.01)
Eq1b <- logmed ~ factor(coins) + black + income + age + female + educdec + num + child + femchild + mhi + ndisease + physlim + ghindx + hlthg + hlthf + hlthp
model1b = lm(Eq1b, data=usahealth) # not recommended
summary(model1b)

model2 <- censReg(Eq1, data=usahealth) # Tobit
summary(model2)

stargazer(model1, model2, type="text", df=FALSE)

me<-margEff(model2) # this is what interests us
summary(me)

model3 <- glm(Eq1, family = poisson(link="log"), data=usahealth)
warnings(50)
summary(model3)

model4 <- glm(Eq1, family = quasipoisson(link="log"), data=usahealth)
summary(model4)
stargazer(model3, model4, type="text", df=FALSE)

me2<-poissonmfx(Eq1, data=usahealth ,atmean = FALSE)
me2$mfxest

## Predictions for models comparison

# Predictions from Tobit
Beta<- coef(model2)[1:20]
Sigma<-exp(coef(model2)[21])
X<-model.matrix(~factor(coins) + black + income + age + female + educdec + num + child + femchild + mhi + ndisease + physlim + ghindx + hlthg + hlthf + hlthp, data=usahealth)
Fit<-X%*%Beta

iter <- 1000                             # number of iterations
mat <- matrix(, nrow = 20186, ncol = iter)

for (i in 1:iter) {
  eps <- rnorm(20186,0,sqrt(Sigma))
  V <- Fit + eps
  V[V < 0] <- 0
  mat[,i]<-V
}
Pred2 <- matrix(, nrow = 20186, ncol = 1)
Pred2[,1]<-rowMeans(mat)

err2<- 100*abs(usahealth$med - Pred2)

# alternatively: 
Fit_norm <- Fit/sqrt(Sigma)
Mills<-dnorm(Fit_norm)/pnorm(Fit_norm)
Pred2b <- pnorm(Fit_norm)*(Fit+sqrt(Sigma)*Mills)

plot(Pred2, Pred2b)

# Predictions from other models
err1<-100*abs(usahealth$med - model1$fitted.values)
pred1b<-exp(model1b$fitted.values+0.5*var(model1b$residuals))-0.01
err1b<-100*abs(usahealth$med - pred1b)
err4<-100*abs(usahealth$med - predict(model4, type = "response"))

# MAE
mean(err1); mean(err1b); mean(err2);  mean(err4);
# MAE
median(err1); median(err1b); median(err2); median(err4)

