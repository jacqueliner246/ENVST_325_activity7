## ENVST 325 Activity 7
## Author: Jacqueline Reynaga
## Date Created: 4-09-26
## Date Last Updated: 4-16-26

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)


# in-class tutorial -------------------------------------------------------

ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")

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
ghg$log.vol <- log(ghg$volume + 1)
ghg$log.runoff <- log(ghg$runoff + 1)

### model
hydro_mod.full <- lm(log.ch4 ~ airTemp +
                       log.age + mean.depth +
                       log.DIP + HydroV +
                       log.precip + BorealV + surface.area, data = ghg)
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
                             ghg$log.DIP,
                             ghg$HydroV,
                             ghg$mean.depth,
                             ghg$log.precip,
                             ghg$surface.area)
chart.Correlation(hydro_reg.data, histogram = TRUE, pch = 19)

### model selection
hydro_full.step <- ols_step_forward_aic(hydro_mod.full) 
hydro_full.step 
hydro_full.step$model
plot(hydro_full.step)

## count NAs
sapply(ghg, function(x) sum(is.na(x)))




# in-class tutorial pt. 2 -------------------------------------------------
library(lubridate)
library(forecast)
library(ggplot2)
library(dplyr)

ETdat <- read.csv("/cloud/project/activity07/ETdata.csv")
unique(ETdat$crop)

## avg fields for each month for almonds
almond <- ETdat %>% 
  filter(crop == 'Almonds') %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

ggplot(almond, aes(x = ymd(date), y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(x = 'year', y = 'Monthly Evapotranspiration (in)')

## time series
almond_ts <- ts(almond$ET.in, 
                start = c(2016, 1),
                frequency = 12)

## decompose time series
almond_dec <- decompose(almond_ts)
plot(almond_dec)

## autocorrelation
almondTrend <- almond_dec$trend
almondSeason <- almond_dec$seasonal

acf(na.omit(almond_ts),
    lag.max = 24)

## autoregressive (AR) models
pcaf.plot <- pacf(na.omit(almond_ts))
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y,
                order = c(1, 0, 0))
model1

model4 <- arima(almond_y,
                order = c(4, 0, 0))
model4

### compare fitted values between models
AR_fit1 <- almond_y - residuals(model1)
AR_fit4 <- almond_y - residuals(model4)

plot(almond_y)
points(AR_fit1, type = 'l', col = 'purple', lty = 2, lwd = 2)
points(AR_fit4, type = 'l', col = 'steelblue', lty = 2, lwd = 2)
legend("topleft", c('data', 'AR1', "AR4"),
       lty = c(1, 2, 2), lwd = c(1, 2, 2),
       col = c('black', 'purple', 'steelblue'),
       bty = 'n')

## forecast
newAlmond <- forecast(model4)
newAlmond

newAlmondF <- data.frame(newAlmond)
years <- c(rep(2021, 4), rep(2022, 12), rep(2023, 8))
month <- c(seq(9, 12), seq(1, 12), seq(1, 8))
newAlmondF$dateF <- ymd(paste(years, '/', month, '/', 1))

ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(almond$date[1], newAlmondF$dateF[24])) +
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col = 'red') +
  geom_ribbon(data = newAlmondF,
              aes(x = dateF, ymin = Lo.95, ymax = Hi.95),
              fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = 'Year', y = 'Almond evapotranspiration (in)')



# homework ----------------------------------------------------------------

## question 1
## multiple regression analysis of the impact of reservoir characteristics 
## on carbon dioxide fluxes

### transform co2 data
ghg$CO2_transformed = (1 / (ghg$co2 / 1000))
### model
co2_mod.full <- lm(CO2_transformed ~ airTemp +
                     log.runoff + log.DIP +
                     mean.depth + log.age +
                     Latitude + HydroV,
                   data = ghg)
summary(co2_mod.full)
write.csv(summary(co2_mod.full)$coef, file = "/cloud/project/activity07/co2_table")

### check assumptions
co2_res.full <- rstandard(co2_mod.full) 
co2_fit.full <- fitted.values(co2_mod.full) 

qqnorm(co2_res.full, pch = 19, col = "grey50") 
qqline(co2_res.full)

shapiro.test(co2_res.full)

plot(co2_fit.full, co2_res.full, pch = 19, col = "grey50")
abline(h = 0)

co2_reg.data <- data.frame(ghg$airTemp,
                             ghg$log.age,
                             ghg$log.DIP,
                             ghg$HydroV,
                             ghg$mean.depth,
                             ghg$log.runoff,
                           ghg$Latitude)
chart.Correlation(co2_reg.data, histogram = TRUE, pch = 19)

### model selection
co2_full.step <- ols_step_forward_aic(co2_mod.full) 
co2_full.step 
co2_full.step$model
plot(co2_full.step)

## question 3
## evapotranspiration time series for almonds, pistachios, 
## fallow/idle fields, corn, and table grapes

### avg fields for each month
pistachios <- ETdat %>% 
  filter(crop == 'Pistachios') %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

fallow <- ETdat %>% 
  filter(crop == 'Fallow/Idle Cropland') %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

corn <- ETdat %>% 
  filter(crop == 'Corn') %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

grapes <- ETdat %>% 
  filter(crop == 'Grapes (Table/Raisin)') %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

### time series
pistachio_ts <- ts(pistachios$ET.in, 
                start = c(2016, 1),
                frequency = 12)

fallow_ts <- ts(fallow$ET.in, 
                   start = c(2016, 1),
                   frequency = 12)

corn_ts <- ts(corn$ET.in, 
                   start = c(2016, 1),
                   frequency = 12)

grape_ts <- ts(grapes$ET.in, 
                   start = c(2016, 1),
                   frequency = 12)

### decompose time series
pistachio_dec <- decompose(pistachio_ts)
fallow_dec <- decompose(fallow_ts)
corn_dec <- decompose(corn_ts)
grape_dec <- decompose(grape_ts)

### plots
plot(almond_dec)
title("(almonds)", line = 1.1)
plot(pistachio_dec)
title("(pistachios)", line = 1.1)
plot(fallow_dec)
title("(fallow/idle cropland)", line = 1.1)
plot(corn_dec)
title("(corn)", line = 1.1)
plot(grape_dec)
title("(table grapes)", line = 1.1)

## question 4
## autoregressive model for pistachios and fallow/idle fields
## historical and forecasted evapotranspiration plots

### autoregressive (AR) models
pistachio_y <- na.omit(pistachio_ts)
model4p <- arima(pistachio_y,
                order = c(4, 0, 0))
model4p

fallow_y <- na.omit(fallow_ts)
model4f <- arima(fallow_y,
                 order = c(4, 0, 0))
model4f

### forecast
newPistachio <- forecast(model4p)
newPistachio

newFallow <- forecast(model4f)
newFallow

newPistachioF <- data.frame(newPistachio)
years <- c(rep(2021, 4), rep(2022, 12), rep(2023, 8))
month <- c(seq(9, 12), seq(1, 12), seq(1, 8))
newPistachioF$dateF <- ymd(paste(years, '/', month, '/', 1))

newFallowF <- data.frame(newFallow)
years <- c(rep(2021, 4), rep(2022, 12), rep(2023, 8))
month <- c(seq(9, 12), seq(1, 12), seq(1, 8))
newFallowF$dateF <- ymd(paste(years, '/', month, '/', 1))

ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(pistachios$date[1], newPistachioF$dateF[24])) +
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast),
            col = 'red') +
  geom_ribbon(data = newPistachioF,
              aes(x = dateF, ymin = Lo.95, ymax = Hi.95),
              fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = 'Year', y = ' Pistachio evapotranspiration (in)')

ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(fallow$date[1], newFallowF$dateF[24])) +
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col = 'red') +
  geom_ribbon(data = newFallowF,
              aes(x = dateF, ymin = Lo.95, ymax = Hi.95),
              fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = 'Year', y = 'Fallow/idle cropland evapotranspiration (in)')

