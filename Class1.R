# Microeconometrics
# CLASS 1
# Continous variables
# Wiktor Budzinski, Marek Giergiczny

rm(list=ls())

# install.packages("readxl") # Install packages if necessary 

library(readxl)
library(rcompanion)
library(jtools)
library(lmtest)
library(tseries)
library(MASS) 
library(splines)
library(glmnet)
library(lmvar)
library(car)
library(mgcv)
library(statmod)
library(stargazer)
library(mltools)

################################################# Exercise 1 ########################################
# ctrl+l
# setwd("C:\\Users\\budzi\\Documents\\ME QPE\Class 1")        # sets the access path (sometimes change the characters '\' to '/' is needed)

data <- read_excel("wine.xlsx")
View(data) 

hist(data$Price) # histogram of data
summary(data$Price) # Basic information about variables

summary(data$Cases) 
data$Cases <- data$Cases/100 # Scalling the covariate
plot(data$Cases/100, data$Price) # simple plot

# Different ways to define equations in R: 

Eq1a <- Price ~ Cases + Score + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99

model1 <- lm(Eq1a, data=data)
summary(model1)
summ(model1, digits = 4) # different function to present the results

Eq1b <- Price ~ .
model1b = lm(Eq1b, data=data)
summ(model1b, digits = 4)


Eq1c<-reformulate(names(data)[-1], names(data)[1])
model1c <- lm(Eq1c, data=data)
summ(model1c, digits = 4)

# Easy way to comapere models next to each other
stargazer(model1, model1b,model1c, type="text", df=FALSE)  

resettest(model1)

# Test RESET conducted 'by hand'
data$mu1 <- model1$fitted.values # fitted values
Eq1d <- Price ~ Cases + Score + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 + 
  I(mu1^2)+ I(mu1^3)
model1d <- lm(Eq1d, data=data)
summ(model1d, digits = 4)
linearHypothesis(model=model1d, c("I(mu1^2)=0","I(mu1^3)=0")) # Test RESET

# Breush-Pagan test
bptest(model1)

# Breush-Pagan test 'by hand'
data$err2 <- rstandard(model1)^2
Eq1e <- err2 ~ Cases + Score + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99
model1e <- lm(Eq1e, data=data)
summ(model1e, digits = 4)
summary(model1e)$r.squared*9600
coefs <- names(coef(model1e))
bptest(model1)
linearHypothesis(model=model1e, coefs[-1], test="Chisq")

jarque.bera.test(model1$residuals) # test for normality

# Useful graphical tests
plotNormalHistogram(model1$residuals) # skewed

scatter.smooth(rstandard(model1) ~ data$Cases, col="grey",
               las=1, ylab="Standardized residuals", xlab="No. of produced cases")
scatter.smooth(rstandard(model1) ~ data$Score, col="grey",
               las=1, ylab="Standardized residuals", xlab="Taste Score")
scatter.smooth(rstandard(model1) ~ model1$fitted.values, col="grey",
               las=1, ylab="Standardized residuals", xlab="Fitted values")

################################################# Exercise 2 ########################################

plot(data$Cases, data$Price)
plot(data$Score, data$Price)

data$LogPrice <- log(data$Price)
Eq2 <- LogPrice ~ Cases + Score + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 
model2 <- lm(Eq2, data=data)
summ(model2, digits = 4)

# Some graphical checks
plotNormalHistogram(model2$residuals)
scatter.smooth(rstandard(model2) ~ data$Cases, col="grey",
               las=1, ylab="Standardized residuals", xlab="No. of produced cases")
resettest(model2)

data$LogCases <- log(data$Cases)
data$LogScore <- log(data$Score)
Eq3 <- LogPrice ~ LogCases + LogScore + factor(Age) + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 
model3 <- lm(Eq3, data=data)
summ(model3, digits = 4)

scatter.smooth(rstandard(model3) ~ data$Cases, col="grey",
               las=1, ylab="Standardized residuals", xlab="No. of produced cases")
scatter.smooth(rstandard(model3) ~ data$Score, col="grey",
               las=1, ylab="Standardized residuals", xlab="Taste Score")

# Plotting coefficients
plot_summs(model3)
plot_summs(model3, coefs = c("2 years" = "factor(Age)2", "3 years" = "factor(Age)3", "4 years" = "factor(Age)4", 
                             "5 years" = "factor(Age)5", "6 years" = "factor(Age)6"))

linearHypothesis(model=model3, c("factor(Age)2 = factor(Age)3 - factor(Age)2","factor(Age)2 = factor(Age)4 - factor(Age)3",
                                  "factor(Age)2 = factor(Age)5 - factor(Age)4", "factor(Age)2 = factor(Age)6 - factor(Age)5")) 

data$ScoreSq <- (data$Score/10)^2
Eq4 <- LogPrice ~ LogCases + Score + ScoreSq + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 
model4 <- lm(Eq4, data=data)
summ(model4, digits = 4)
scatter.smooth(rstandard(model4) ~ data$Score, col="grey",
               las=1, ylab="Standardized residuals", xlab="Taste Score")
resettest(model4)

# You can try to recode continous variable as a categorical one

data$CasesBin <- bin_data(data$Cases, bins=10, binType="quantile") # equal no. of observations in each category
#data$CasesBin2 <- bin_data(data$Cases, bins=10) # equaly spaced categories
data$ScoreBin <- bin_data(data$Score, bins=10, binType="quantile") # equal no. of observations in each category
#data$ScoreBin2 <- bin_data(data$Score, bins=10) # equaly spaced categories

Eq5 <- LogPrice ~ factor(data$CasesBin) + factor(data$ScoreBin) + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 
model5 <- lm(Eq5, data=data)
summ(model5, digits = 4)

data$CasesBin <-unclass(data$CasesBin)
data$ScoreBin <-unclass(data$ScoreBin)
model5 <- lm(Eq5, data=data)
summ(model5, digits = 4)


plot_summs(model5, coefs = c("factor(data$CasesBin)2", "factor(data$CasesBin)3", "factor(data$CasesBin)4", 
                             "factor(data$CasesBin)5", "factor(data$CasesBin)6", "factor(data$CasesBin)7",
                             "factor(data$CasesBin)8", "factor(data$CasesBin)9", "factor(data$CasesBin)10"))

plot_summs(model5, coefs = c("factor(data$ScoreBin)2", "factor(data$ScoreBin)3", "factor(data$ScoreBin)4", 
                             "factor(data$ScoreBin)5", "factor(data$ScoreBin)6", "factor(data$ScoreBin)7",
                             "factor(data$ScoreBin)8", "factor(data$ScoreBin)9"))


Eq6 <- LogPrice ~ LogCases + Score + ScoreSq + Age + NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 
model6 <- lm(Eq6, data=data)
summ(model6, digits = 4)

# Add interaction

Eq7 <- LogPrice ~ LogCases + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  Score + ScoreSq + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99
model7 <- lm(Eq7, data=data)
summ(model7, digits = 4)
resettest(model7)

################################################# Exercise 3 ########################################
# Influential observations

data$h <- hatvalues(model7) # leverage
sort(data$h, decreasing=TRUE)[1:10]
mean(data$h)
hist(data$h)

#ErrStudentized <- rstudent(model7)
#mean(abs(ErrStudentized) > 2.5) # Should be ~ 1.2%

# Influential measures 
cd<- cooks.distance(model7)
dff<-dffits(model7)
db<-dfbetas(model7)

# Influential measures with a single function
im <- influence.measures(model7)
head(im$infmat)
colSums(im$is.inf) # hat is leverage

median(data$Price[which(im$is.inf[,50])]) 
median(data$Price)
mean(data$Price[which(im$is.inf[,50])]> mean(data$Price)) # what % is higher than the average price
scatter.smooth(abs(im$infmat[,50]) ~ data$Price, col="grey",
               las=1, ylab="DFFit", xlab="Price")

################################################# Exercise 4 ########################################
# Box Cox

Eq7b <- Price ~ LogCases + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  Score + ScoreSq + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99
model7b <- lm(Eq7b, data=data)

# fitting models with various Box-Cox transformations
bx<-MASS::boxcox(model7b, lambda=seq(-2, 2, length=200), data=data)
XVal<-bx[["x"]]
XVal[which(bx[["y"]] == max(bx[["y"]]))]

data$PriceBC <- (data$Price^(-0.23)-1)/0.23
Eq7c <- PriceBC ~ LogCases + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  Score + ScoreSq + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99
model7c <- lm(Eq7b, data=data)
resettest(model7c)
stargazer(model7, model7c, type="text", df=FALSE)  

# Box cox for independent variable (nonlinear least squares)

Eq7d <- LogPrice ~ LogCases + Score + ScoreSq + Age
model7d <- lm(Eq7d, data = data)
coef(model7d)
Eq7e <- LogPrice ~ b1+ b2*I((Cases^a1-1)/a1)
model7e <- nls(Eq7e, data = data, start = list(a1 = -0.01, b1 = 22, b2 = -0.1))
summary(model7e)

Eq7e <- LogPrice ~ b1+ b2*I((Cases^a1-1)/a1) + b3*Score+ b4*Age
model7f <- nls(Eq7e, data = data, start = list(a1 = 0.1,  b1 = 3.6, b2 = -0.1, b3 = -0.5, b4 = 0.15))
summary(model7f)

# Splines

# Natual cubic splines
Eq8 <- LogPrice ~ ns(Cases, df=5) + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  ns(Score, df = 5) + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99  
model8 <- lm(Eq8, data=data)
summ(model8)
resettest(model8)

data$Spline1 <- model8$model$`ns(Cases, df = 5)` %*% model8$coefficients[c(2, 3, 4, 5, 6)]
plot(data$Cases, data$Spline1)
points(data$Cases, -0.10*data$LogCases, col="red")

data$Spline2 <- model8$model$`ns(Score, df = 5)` %*% model8$coefficients[c(7, 8, 9, 10, 11)]
plot(data$Score, data$Spline2)
points(data$Score, -0.423*data$Score + 0.268*data$ScoreSq+16.5, col="red")


# B-spline

Eq9 <- LogPrice ~ bs(Cases, df=5) + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  bs(Score, df = 5) + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99  

model9 <- lm(Eq9, data=data)
summ(model9, digits = 4)
resettest(model9)

data$Spline1b <- model9$model$`bs(Cases, df = 5)` %*% model9$coefficients[c(2, 3, 4, 5, 6)]
plot(data$Cases, data$Spline1b)

data$Spline2b <- model9$model$`bs(Score, df = 5)` %*% model9$coefficients[c(7, 8, 9, 10, 11)]
plot(data$Score, data$Spline2b)

# Comparing different models
stargazer(model7, model8,model9, type="text", df=FALSE)  

# Smoothing spline
Eq10 <- LogPrice ~ s(Cases) + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  s(Score) + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99  
model10 <- gam(Eq10, data = data)
summary(model10)
plot(model10)

# Using cubic splines
Eq10b <- LogPrice ~ s(Cases, bs = "cr") + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
  s(Score, bs = "cr") + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
  Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
  NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
  Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
  Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99  
model10b <- gam(Eq10b, data = data)
summary(model10b)
plot(model10b)
stargazer(model10, model10b, type="text", df=FALSE)  

################################################# Exercise 5 ########################################
# Heteroskedascity 

summ(model7, digits = 4)
summ(model7, digits = 4, robust = "HC0") # Robuts covariance matrix

X_m = model.matrix(~LogCases + LogCases:washington + LogCases:Nonvarietal + LogCases:Pinotnoir + LogCases:Cabernet + LogCases:Merlot + LogCases:Shyrah +
                     Score + ScoreSq + Score:washington + Score:Nonvarietal + Score:Pinotnoir + Score:Cabernet + Score:Merlot + Score:Shyrah +
                     Age + Age:washington + Age:Nonvarietal + Age:Pinotnoir + Age:Cabernet + Age:Merlot + Age:Shyrah +
                     NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
                     Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
                     Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 -1, data=data)

# Variables that might affect heteroscedasticity
X_s = model.matrix(~LogCases + Score + ScoreSq + Age +
                     NapaValley+BayArea+sonoma+SouthCoast+carneros+Sierra+mendocino + washington +
                     Nonvarietal + Pinotnoir + Cabernet + Merlot + Shyrah + Reserve + Vineyard + Estate +
                     Vintage91 + Vintage92 + Vintage93 + Vintage94 + Vintage95 + Vintage96  + Vintage97 + Vintage98 + Vintage99 -1, data=data)


model11 = lmvar(data$LogPrice, X_mu=X_m, X_sigma = X_s)
# options(scipen=999)
summary(model11)

# BP test "by hand"
data$mu1 <- fitted(model11, sigma = FALSE)
data$sigma1 <- fitted(model11, mu = FALSE)

data$err <-(data$LogPrice-data$mu1)/data$sigma1
data$err2 <- data$err^2
model11b <- lmvar(data$err2, X_mu=X_m)
summary(model11b)
coefs <- names(coef(model11b))
linearHypothesis(model=model11b, coefs[c(-1,-50)], test="Chisq")

X_s <- X_m
model12 <- lmvar(data$LogPrice, X_mu=X_m, X_sigma = X_s)
summary(model12)

# BP test "by hand"
data$mu2 <- fitted(model12, sigma = FALSE)
data$sigma2 <- fitted(model12, mu = FALSE)

data$err <-(data$LogPrice-data$mu2)/data$sigma2
data$err2 <- data$err^2
model12b <- lmvar(data$err2, X_mu=X_m)
summary(model12b)
coefs <- names(coef(model12b))
linearHypothesis(model=model12b, coefs[c(-1,-50)], test="Chisq")

