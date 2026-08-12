#########################
## Russia-Ukraine War
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)

# Read CSV file
oil <- read.csv("Oil Import Data - 2.csv", header = TRUE)
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
  filter(Date >= as.Date("2021-03-01"),
         Date <= as.Date("2022-02-01"))

Q0 <- mean(baseline$Barrels)
P0 <- mean(baseline$Price)
ER0 <- mean(baseline$Rate)


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

Table2

mean_qnty <- mean(Table2$QuantityEffect)
mean_oil  <- mean(Table2$OilPriceEffect)
mean_exr  <- mean(Table2$ExchangeRateEffect)
mean_excost <- mean(Table2$TotalExtraCost)
sd_qnty <- sd(Table2$QuantityEffect)
sd_oil  <- sd(Table2$OilPriceEffect)
sd_exr  <- sd(Table2$ExchangeRateEffect)
sd_excost <- sd(Table2$TotalExtraCost)
event <- data.frame(
  `Qnty Effect` = sprintf("%0.2f (%0.2f)", mean_qnty, sd_qnty),
  `Oil Price Effect` = sprintf("%0.2f (%0.2f)", mean_oil, sd_oil),
  `ExR Effect` = sprintf("%0.2f (%0.2f)", mean_exr, sd_exr),
  `Total Effect` = sprintf("%0.2f (%0.2f)", mean_excost, sd_excost),
  check.names = FALSE
)
library(knitr)
kable(event, booktabs = FALSE, escape = FALSE, align = "cccc")





#########################
## USA-Iran Conflict
########################
## Conflict Started on 28th Feb 2026

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)

# Read CSV file
oil <- read.csv("Oil Import Data - 2.csv", header = TRUE)
oil <- na.omit(oil)
tail(oil)

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
  filter(Date >= as.Date("2025-03-01"),
         Date <= as.Date("2026-02-01"))

Q0 <- mean(baseline$Barrels)
P0 <- mean(baseline$Price)
ER0 <- mean(baseline$Rate)


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

Table2

mean_qnty <- mean(Table2$QuantityEffect)
mean_oil  <- mean(Table2$OilPriceEffect)
mean_exr  <- mean(Table2$ExchangeRateEffect)
mean_excost <- mean(Table2$TotalExtraCost)
sd_qnty <- sd(Table2$QuantityEffect)
sd_oil  <- sd(Table2$OilPriceEffect)
sd_exr  <- sd(Table2$ExchangeRateEffect)
sd_excost <- sd(Table2$TotalExtraCost)
event <- data.frame(
  `Qnty Effect` = sprintf("%0.2f (%0.2f)", mean_qnty, sd_qnty),
  `Oil Price Effect` = sprintf("%0.2f (%0.2f)", mean_oil, sd_oil),
  `ExR Effect` = sprintf("%0.2f (%0.2f)", mean_exr, sd_exr),
  `Total Effect` = sprintf("%0.2f (%0.2f)", mean_excost, sd_excost),
  check.names = FALSE
)
library(knitr)
kable(event, booktabs = FALSE, escape = FALSE, align = "cccc")





#########################
## Pre-COVID
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)

# Read CSV file
oil <- read.csv("Oil Import Data - 2.csv", header = TRUE)
oil <- na.omit(oil)
tail(oil)

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

Q0 <- mean(baseline$Barrels)
P0 <- mean(baseline$Price)
ER0 <- mean(baseline$Rate)


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

#Table2

mean_qnty <- mean(Table2$QuantityEffect)
mean_oil  <- mean(Table2$OilPriceEffect)
mean_exr  <- mean(Table2$ExchangeRateEffect)
mean_excost <- mean(Table2$TotalExtraCost)
sd_qnty <- sd(Table2$QuantityEffect)
sd_oil  <- sd(Table2$OilPriceEffect)
sd_exr  <- sd(Table2$ExchangeRateEffect)
sd_excost <- sd(Table2$TotalExtraCost)
event <- data.frame(
  `Qnty Effect` = sprintf("%0.2f (%0.2f)", mean_qnty, sd_qnty),
  `Oil Price Effect` = sprintf("%0.2f (%0.2f)", mean_oil, sd_oil),
  `ExR Effect` = sprintf("%0.2f (%0.2f)", mean_exr, sd_exr),
  `Total Effect` = sprintf("%0.2f (%0.2f)", mean_excost, sd_excost),
  check.names = FALSE
)
library(knitr)
kable(event, booktabs = FALSE, escape = FALSE, align = "cccc")





#########################
## COVID-19 Pandemic
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)

# Read CSV file
oil <- read.csv("Oil Import Data - 2.csv", header = TRUE)
oil <- na.omit(oil)
tail(oil)

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

Q0 <- mean(baseline$Barrels)
P0 <- mean(baseline$Price)
ER0 <- mean(baseline$Rate)


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

#Table2

mean_qnty <- mean(Table2$QuantityEffect)
mean_oil  <- mean(Table2$OilPriceEffect)
mean_exr  <- mean(Table2$ExchangeRateEffect)
mean_excost <- mean(Table2$TotalExtraCost)
sd_qnty <- sd(Table2$QuantityEffect)
sd_oil  <- sd(Table2$OilPriceEffect)
sd_exr  <- sd(Table2$ExchangeRateEffect)
sd_excost <- sd(Table2$TotalExtraCost)
event <- data.frame(
  `Qnty Effect` = sprintf("%0.2f (%0.2f)", mean_qnty, sd_qnty),
  `Oil Price Effect` = sprintf("%0.2f (%0.2f)", mean_oil, sd_oil),
  `ExR Effect` = sprintf("%0.2f (%0.2f)", mean_exr, sd_exr),
  `Total Effect` = sprintf("%0.2f (%0.2f)", mean_excost, sd_excost),
  check.names = FALSE
)
library(knitr)
kable(event, booktabs = FALSE, escape = FALSE, align = "cccc")





#########################
## Post-war Stabilization
########################

########################################
########################################
## Comparison between the currencies
library(ggplot2)
library(dplyr)

# Read CSV file
oil <- read.csv("Oil Import Data - 2.csv", header = TRUE)
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

Q0 <- mean(baseline$Barrels)
P0 <- mean(baseline$Price)
ER0 <- mean(baseline$Rate)


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

#Table2

mean_qnty <- mean(Table2$QuantityEffect)
mean_oil  <- mean(Table2$OilPriceEffect)
mean_exr  <- mean(Table2$ExchangeRateEffect)
mean_excost <- mean(Table2$TotalExtraCost)
sd_qnty <- sd(Table2$QuantityEffect)
sd_oil  <- sd(Table2$OilPriceEffect)
sd_exr  <- sd(Table2$ExchangeRateEffect)
sd_excost <- sd(Table2$TotalExtraCost)
event <- data.frame(
  `Qnty Effect` = sprintf("%0.2f (%0.2f)", mean_qnty, sd_qnty),
  `Oil Price Effect` = sprintf("%0.2f (%0.2f)", mean_oil, sd_oil),
  `ExR Effect` = sprintf("%0.2f (%0.2f)", mean_exr, sd_exr),
  `Total Effect` = sprintf("%0.2f (%0.2f)", mean_excost, sd_excost),
  check.names = FALSE
)
library(knitr)
kable(event, booktabs = FALSE, escape = FALSE, align = "cccc")





