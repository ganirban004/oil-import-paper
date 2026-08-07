#########################
## Russia-Ukraine War: ARIMA Counterfactual
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)
library(forecast)

# Read CSV file
oil <- read.csv("Oil Import Data - Raju Sir.csv", header = TRUE)
oil <- na.omit(oil)
head(oil)

oil$Barrels <- oil$COI * 1000 * 6.28981
oil$ActualCost <-
  (oil$Barrels *
     oil$Price *
     oil$Rate)/10^7

oil$MillionBarrels  <- oil$Barrels/10^6


## Construct Baseline
oil$Date <- as.Date(paste(oil$Year, oil$Month, "01"),
                    format="%Y %b %d")
baseline <-
  oil %>%
  filter(Date >= as.Date("2020-07-01"),
         Date <= as.Date("2022-02-01"))

n_post <- sum(oil$Date > max(baseline$Date))

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)


##Counterfactual Import Cost
oil <-
  oil %>%
  mutate(
    Counterfactual =
      Q0*P0*ER0/1e7
  )




## Quantity effect
oil <-
  oil %>%
  mutate(
    QuantityEffect =
      (Barrels-Q0)*Price*Rate/1e7
  )


## Oil price effect
oil <-
  oil %>%
  mutate(
    OilEffect =
      Barrels*(Price-P0)*Rate/1e7
  )

## Exchange Rate effect
oil <-
  oil %>%
  mutate(
    ExchangeEffect =
      Barrels*Price*(Rate-ER0)/1e7
  )

## Interaction effect
oil <-
  oil %>%
  mutate(
    Interaction =
      Barrels*
      (Price-P0)*
      (Rate-ER0)/1e7
  )


## Total extra cost
oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


## Check the above decomposition: it should be zero
oil %>%
  summarise(
    Difference=
      sum(ExtraCost-
            (OilEffect+
               ExchangeEffect+
               Interaction))
  )



Table2 <-
  oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  
  transmute(
    
    Period=paste(Month,Year),
    
    Actual=round(ActualCost,2),
    
    Counterfactual=round(Counterfactual,2),
    
    QuantityEffect = round(QuantityEffect,2),
    
    OilPriceEffect=round(OilEffect,2),
    
    ExchangeRateEffect=round(ExchangeEffect,2),
    
    TotalExtraCost=round(ExtraCost,2)
    
  )

# total_row <- data.frame(
#   Period = "Total",
#   Actual = sum(Table2$Actual, na.rm = TRUE),
#   Counterfactual = sum(Table2$Counterfactual, na.rm = TRUE),
#   QuantityEffect = sum(Table2$QuantityEffect, na.rm = TRUE),
#   OilPriceEffect = sum(Table2$OilPriceEffect, na.rm = TRUE),
#   ExchangeRateEffect = sum(Table2$ExchangeRateEffect, na.rm = TRUE),
#   TotalExtraCost = sum(Table2$TotalExtraCost, na.rm = TRUE)
# )
# 
# Table2 <- rbind(Table2, total_row)
# 
Table2

round(c(mean(Table2$QuantityEffect), mean(Table2$OilPriceEffect), mean(Table2$ExchangeRateEffect)), 2)
round(c(sd(Table2$QuantityEffect), sd(Table2$OilPriceEffect), sd(Table2$ExchangeRateEffect)), 2)





#########################
## US-Iran Conflict: ARIMA Counterfactual
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)
library(forecast)

# Read CSV file
oil <- read.csv("Oil Import Data - Raju Sir.csv", header = TRUE)
oil <- na.omit(oil)
head(oil)

oil$Barrels <- oil$COI * 1000 * 6.28981
oil$ActualCost <-
  (oil$Barrels *
     oil$Price *
     oil$Rate)/10^7

oil$MillionBarrels  <- oil$Barrels/10^6


## Construct Baseline
oil$Date <- as.Date(paste(oil$Year, oil$Month, "01"),
                    format="%Y %b %d")
baseline <-
  oil %>%
  filter(Date >= as.Date("2024-03-01"),
         Date <= as.Date("2026-02-01"))

n_post <- sum(oil$Date > max(baseline$Date))

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)


##Counterfactual Import Cost
oil <-
  oil %>%
  mutate(
    Counterfactual =
      Q0*P0*ER0/1e7
  )




## Quantity effect
oil <-
  oil %>%
  mutate(
    QuantityEffect =
      (Barrels-Q0)*Price*Rate/1e7
  )


## Oil price effect
oil <-
  oil %>%
  mutate(
    OilEffect =
      Barrels*(Price-P0)*Rate/1e7
  )

## Exchange Rate effect
oil <-
  oil %>%
  mutate(
    ExchangeEffect =
      Barrels*Price*(Rate-ER0)/1e7
  )

## Interaction effect
oil <-
  oil %>%
  mutate(
    Interaction =
      Barrels*
      (Price-P0)*
      (Rate-ER0)/1e7
  )


## Total extra cost
oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


## Check the above decomposition: it should be zero
oil %>%
  summarise(
    Difference=
      sum(ExtraCost-
            (OilEffect+
               ExchangeEffect+
               Interaction))
  )



Table2 <-
  oil %>%
  filter(Date >= as.Date("2026-03-01"),
         Date <= as.Date("2026-03-01")) %>%
  
  transmute(
    
    Period=paste(Month,Year),
    
    Actual=round(ActualCost,2),
    
    Counterfactual=round(Counterfactual,2),
    
    QuantityEffect = round(QuantityEffect,2),
    
    OilPriceEffect=round(OilEffect,2),
    
    ExchangeRateEffect=round(ExchangeEffect,2),
    
    TotalExtraCost=round(ExtraCost,2)
    
  )

# total_row <- data.frame(
#   Period = "Total",
#   Actual = sum(Table2$Actual, na.rm = TRUE),
#   Counterfactual = sum(Table2$Counterfactual, na.rm = TRUE),
#   QuantityEffect = sum(Table2$QuantityEffect, na.rm = TRUE),
#   OilPriceEffect = sum(Table2$OilPriceEffect, na.rm = TRUE),
#   ExchangeRateEffect = sum(Table2$ExchangeRateEffect, na.rm = TRUE),
#   TotalExtraCost = sum(Table2$TotalExtraCost, na.rm = TRUE)
# )
# 
# Table2 <- rbind(Table2, total_row)
# 
Table2

round(c(mean(Table2$QuantityEffect), mean(Table2$OilPriceEffect), mean(Table2$ExchangeRateEffect)), 2)
round(c(sd(Table2$QuantityEffect), sd(Table2$OilPriceEffect), sd(Table2$ExchangeRateEffect)), 2)





#########################
## Pre-COVID: ARIMA Counterfactual
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)
library(forecast)

# Read CSV file
oil <- read.csv("Oil Import Data - Raju Sir.csv", header = TRUE)
oil <- na.omit(oil)
head(oil)

oil$Barrels <- oil$COI * 1000 * 6.28981
oil$ActualCost <-
  (oil$Barrels *
     oil$Price *
     oil$Rate)/10^7

oil$MillionBarrels  <- oil$Barrels/10^6


## Construct Baseline
oil$Date <- as.Date(paste(oil$Year, oil$Month, "01"),
                    format="%Y %b %d")
baseline <-
  oil %>%
  filter(Date >= as.Date("2018-01-01"),
         Date <= as.Date("2018-12-01"))

n_post <- sum(oil$Date > max(baseline$Date))

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)


##Counterfactual Import Cost
oil <-
  oil %>%
  mutate(
    Counterfactual =
      Q0*P0*ER0/1e7
  )




## Quantity effect
oil <-
  oil %>%
  mutate(
    QuantityEffect =
      (Barrels-Q0)*Price*Rate/1e7
  )


## Oil price effect
oil <-
  oil %>%
  mutate(
    OilEffect =
      Barrels*(Price-P0)*Rate/1e7
  )

## Exchange Rate effect
oil <-
  oil %>%
  mutate(
    ExchangeEffect =
      Barrels*Price*(Rate-ER0)/1e7
  )

## Interaction effect
oil <-
  oil %>%
  mutate(
    Interaction =
      Barrels*
      (Price-P0)*
      (Rate-ER0)/1e7
  )


## Total extra cost
oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


## Check the above decomposition: it should be zero
oil %>%
  summarise(
    Difference=
      sum(ExtraCost-
            (OilEffect+
               ExchangeEffect+
               Interaction))
  )



Table2 <-
  oil %>%
  filter(Date >= as.Date("2019-01-01"),
         Date <= as.Date("2020-02-01")) %>%
  
  transmute(
    
    Period=paste(Month,Year),
    
    Actual=round(ActualCost,2),
    
    Counterfactual=round(Counterfactual,2),
    
    QuantityEffect = round(QuantityEffect,2),
    
    OilPriceEffect=round(OilEffect,2),
    
    ExchangeRateEffect=round(ExchangeEffect,2),
    
    TotalExtraCost=round(ExtraCost,2)
    
  )

# total_row <- data.frame(
#   Period = "Total",
#   Actual = sum(Table2$Actual, na.rm = TRUE),
#   Counterfactual = sum(Table2$Counterfactual, na.rm = TRUE),
#   QuantityEffect = sum(Table2$QuantityEffect, na.rm = TRUE),
#   OilPriceEffect = sum(Table2$OilPriceEffect, na.rm = TRUE),
#   ExchangeRateEffect = sum(Table2$ExchangeRateEffect, na.rm = TRUE),
#   TotalExtraCost = sum(Table2$TotalExtraCost, na.rm = TRUE)
# )
# 
# Table2 <- rbind(Table2, total_row)
#
# Table2

round(c(mean(Table2$QuantityEffect), mean(Table2$OilPriceEffect), mean(Table2$ExchangeRateEffect)), 2)
round(c(sd(Table2$QuantityEffect), sd(Table2$OilPriceEffect), sd(Table2$ExchangeRateEffect)), 2)





#########################
## COVID-19 Pandemic: ARIMA Counterfactual
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)
library(forecast)

# Read CSV file
oil <- read.csv("Oil Import Data - Raju Sir.csv", header = TRUE)
oil <- na.omit(oil)
head(oil)

oil$Barrels <- oil$COI * 1000 * 6.28981
oil$ActualCost <-
  (oil$Barrels *
     oil$Price *
     oil$Rate)/10^7

oil$MillionBarrels  <- oil$Barrels/10^6


## Construct Baseline
oil$Date <- as.Date(paste(oil$Year, oil$Month, "01"),
                    format="%Y %b %d")
baseline <-
  oil %>%
  filter(Date >= as.Date("2019-03-01"),
         Date <= as.Date("2020-02-01"))

n_post <- sum(oil$Date > max(baseline$Date))

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)


##Counterfactual Import Cost
oil <-
  oil %>%
  mutate(
    Counterfactual =
      Q0*P0*ER0/1e7
  )




## Quantity effect
oil <-
  oil %>%
  mutate(
    QuantityEffect =
      (Barrels-Q0)*Price*Rate/1e7
  )


## Oil price effect
oil <-
  oil %>%
  mutate(
    OilEffect =
      Barrels*(Price-P0)*Rate/1e7
  )

## Exchange Rate effect
oil <-
  oil %>%
  mutate(
    ExchangeEffect =
      Barrels*Price*(Rate-ER0)/1e7
  )

## Interaction effect
oil <-
  oil %>%
  mutate(
    Interaction =
      Barrels*
      (Price-P0)*
      (Rate-ER0)/1e7
  )


## Total extra cost
oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


## Check the above decomposition: it should be zero
oil %>%
  summarise(
    Difference=
      sum(ExtraCost-
            (OilEffect+
               ExchangeEffect+
               Interaction))
  )



Table2 <-
  oil %>%
  filter(Date >= as.Date("2020-03-01"),
         Date <= as.Date("2021-02-01")) %>%
  
  transmute(
    
    Period=paste(Month,Year),
    
    Actual=round(ActualCost,2),
    
    Counterfactual=round(Counterfactual,2),
    
    QuantityEffect = round(QuantityEffect,2),
    
    OilPriceEffect=round(OilEffect,2),
    
    ExchangeRateEffect=round(ExchangeEffect,2),
    
    TotalExtraCost=round(ExtraCost,2)
    
  )

# total_row <- data.frame(
#   Period = "Total",
#   Actual = sum(Table2$Actual, na.rm = TRUE),
#   Counterfactual = sum(Table2$Counterfactual, na.rm = TRUE),
#   QuantityEffect = sum(Table2$QuantityEffect, na.rm = TRUE),
#   OilPriceEffect = sum(Table2$OilPriceEffect, na.rm = TRUE),
#   ExchangeRateEffect = sum(Table2$ExchangeRateEffect, na.rm = TRUE),
#   TotalExtraCost = sum(Table2$TotalExtraCost, na.rm = TRUE)
# )
# 
# Table2 <- rbind(Table2, total_row)
#
# Table2

round(c(mean(Table2$QuantityEffect), mean(Table2$OilPriceEffect), mean(Table2$ExchangeRateEffect)), 2)
round(c(sd(Table2$QuantityEffect), sd(Table2$OilPriceEffect), sd(Table2$ExchangeRateEffect)), 2)





#########################
## Post-war Stabilization: ARIMA Counterfactual
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)
library(forecast)

# Read CSV file
oil <- read.csv("Oil Import Data - Raju Sir.csv", header = TRUE)
oil <- na.omit(oil)
head(oil)

oil$Barrels <- oil$COI * 1000 * 6.28981
oil$ActualCost <-
  (oil$Barrels *
     oil$Price *
     oil$Rate)/10^7

oil$MillionBarrels  <- oil$Barrels/10^6


## Construct Baseline
oil$Date <- as.Date(paste(oil$Year, oil$Month, "01"),
                    format="%Y %b %d")
baseline <-
  oil %>%
  filter(Date >= as.Date("2022-01-01"),
         Date <= as.Date("2022-12-01"))

n_post <- sum(oil$Date > max(baseline$Date))

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)


##Counterfactual Import Cost
oil <-
  oil %>%
  mutate(
    Counterfactual =
      Q0*P0*ER0/1e7
  )




## Quantity effect
oil <-
  oil %>%
  mutate(
    QuantityEffect =
      (Barrels-Q0)*Price*Rate/1e7
  )


## Oil price effect
oil <-
  oil %>%
  mutate(
    OilEffect =
      Barrels*(Price-P0)*Rate/1e7
  )

## Exchange Rate effect
oil <-
  oil %>%
  mutate(
    ExchangeEffect =
      Barrels*Price*(Rate-ER0)/1e7
  )

## Interaction effect
oil <-
  oil %>%
  mutate(
    Interaction =
      Barrels*
      (Price-P0)*
      (Rate-ER0)/1e7
  )


## Total extra cost
oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


## Check the above decomposition: it should be zero
oil %>%
  summarise(
    Difference=
      sum(ExtraCost-
            (OilEffect+
               ExchangeEffect+
               Interaction))
  )



Table2 <-
  oil %>%
  filter(Date >= as.Date("2023-01-01"),
         Date <= as.Date("2023-12-01")) %>%
  
  transmute(
    
    Period=paste(Month,Year),
    
    Actual=round(ActualCost,2),
    
    Counterfactual=round(Counterfactual,2),
    
    QuantityEffect = round(QuantityEffect,2),
    
    OilPriceEffect=round(OilEffect,2),
    
    ExchangeRateEffect=round(ExchangeEffect,2),
    
    TotalExtraCost=round(ExtraCost,2)
    
  )

# total_row <- data.frame(
#   Period = "Total",
#   Actual = sum(Table2$Actual, na.rm = TRUE),
#   Counterfactual = sum(Table2$Counterfactual, na.rm = TRUE),
#   QuantityEffect = sum(Table2$QuantityEffect, na.rm = TRUE),
#   OilPriceEffect = sum(Table2$OilPriceEffect, na.rm = TRUE),
#   ExchangeRateEffect = sum(Table2$ExchangeRateEffect, na.rm = TRUE),
#   TotalExtraCost = sum(Table2$TotalExtraCost, na.rm = TRUE)
# )
# 
# Table2 <- rbind(Table2, total_row)
#
# Table2

round(c(mean(Table2$QuantityEffect), mean(Table2$OilPriceEffect), mean(Table2$ExchangeRateEffect)), 2)
round(c(sd(Table2$QuantityEffect), sd(Table2$OilPriceEffect), sd(Table2$ExchangeRateEffect)), 2)



