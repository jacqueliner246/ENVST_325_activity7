## ENVST 325 Activity 7
## Author: Jacqueline Reynaga
## Date Created: 4-09-26
## Date Last Updated: 4-12-26

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)


# in-class tutorial -------------------------------------------------------

ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")
et <- read.csv("/cloud/project/activity07/ETdata.csv")

## transforming data
### data is mainly very low values, indicates that log transformation
### should be done
ggplot(ghg, aes(x = round(ch4, 0))) +
  geom_histogram(fill = 'royalblue') +
  theme_classic()

### log transform methane data
ghg$log.ch4 <- log(ghg$ch4 + 1)

### comparison of raw data and log transformed data
ggplot(ghg, aes(y = ch4, x = airTemp)) + # non liner relationship
  geom_point(color = 'royalblue') +
  theme_classic()

ggplot(ghg, aes(y = log.ch4, x = airTemp)) + # liner relationship
  geom_point(color = 'maroon') +
  theme_classic()

### log transform other data
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP + 1)
ghg$log.precip <- log(ghg$precipitation)


## binary variables
unique(ghg$Region)

ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0) # boreal
ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0) # tropical
ghg$AlpineV <- ifelse(ghg$Alpine == "yes", 1, 0) # alpine
ghg$HydroV <- ifelse(ghg$hydropower == "yes", 1, 0) # hydropower


## multiple regression
mod.full <- lm(log.ch4 ~ airTemp +
                 log.age + mean.depth +
                 log.DIP +
                 log.precip + BorealV, data = ghg)

summary(mod.full) # gives coefficient table and r-squared

## check assumptions
### main assumptions of regressions
#### 1. Relationships are linear
#### 2. Residuals are normally distributed
#### 3. Residuals have equal variance
#### 4. Residuals are independent and random
#### 5. In a multiple regression, independent variables are not 
#### highly correlated (multicollinearity)

res.full <- rstandard(mod.full) # standardized residuals
fit.full <- fitted.values(mod.full) # fitted values from the regression line at each observation

### normal distribution
qqnorm(res.full, pch = 19, col = "grey50") # qq plot
qqline(res.full)

shapiro.test(res.full) # shapiro-wilks test

### residuals
plot(fit.full, res.full, pch = 19, col = "grey50")
abline(h = 0)

### multicollinearity
#### isolate continuous model variables into data frame:
reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,
                       ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)
chart.Correlation(reg.data, histogram = TRUE, pch = 19) #correlation matrix


## model selection
full.step <- ols_step_forward_aic(mod.full) # stepwise selection
full.step 
full.step$model # full model
### want to interpret how much the AIC and adjusted R-squared change with each added variable
plot(full.step) # AIC vs time


## predictions
predict.lm(mod.full, data.frame(airTemp = 20,
                                log.age = log(2),
                                mean.depth = 15,
                                log.DIP = 3,
                                log.precip = 6, 
                                BorealV = 0),
           interval = "prediction") # interval for a single point of data
predict.lm(mod.full, data.frame(airTemp = 20,
                                log.age = log(2),
                                mean.depth = 15,
                                log.DIP = 3,
                                log.precip = 6, 
                                BorealV = 0),
           interval = "confidence") # 95% confidence interval

## compile preliminary results from the Deemer data for climate policy makers interested 
## in understanding whether increased reservoir creation for hydroelectric power 
## would be expected to affect methane release

ghg$log.chlorA <- log(ghg$chlorophyll.a)
ghg$log.surfArea <- log(ghg$surface.area)
ghg$log.vol <- log(ghg$volume + 1)

### model
hydro_mod.full <- lm(log.ch4 ~ airTemp +
                       log.age + mean.depth +
                       log.precip + BorealV, data = ghg)
summary(hydro_mod.full)

### check assumptions
hydro_res.full <- rstandard(hydro_mod.full) 
hydro_fit.full <- fitted.values(hydro_mod.full) 

qqnorm(hydro_res.full, pch = 19, col = "grey50") 
qqline(hydro_res.full)

shapiro.test(hydro_res.full)

plot(hydro_fit.full, hydro_res.full, pch = 19, col = "grey50")
abline(h = 0)

hydro_reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,
                       ghg$mean.depth,
                       ghg$log.precip,
                       ghg$BorealV)
chart.Correlation(hydro_reg.data, histogram = TRUE, pch = 19)

### model selection
hydro_full.step <- ols_step_forward_aic(hydro_mod.full) 
hydro_full.step 
hydro_full.step$model
plot(hydro_full.step)



