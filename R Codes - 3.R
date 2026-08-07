#########################
## Russia-Ukraine War: ARIMA Counterfactual of Expense: Baseline last 19 Months before war
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
fit.arima <- auto.arima(ts(baseline$ActualCost, frequency = 12), seasonal = F)
n_post <- sum(oil$Date > max(baseline$Date))
CF <- forecast(fit.arima, h = n_post)

oil$Counterfactual <- NA_real_
oil$Counterfactual[oil$Date >= min(baseline$Date) &
                     oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit.arima))
oil$Counterfactual[oil$Date > max(baseline$Date)] <- as.numeric(CF$mean)

oil$Lower95 <- NA_real_
oil$Lower95[oil$Date > max(baseline$Date)] <- as.numeric(CF$lower[, 2])
oil$Upper95 <- NA_real_
oil$Upper95[oil$Date > max(baseline$Date)] <- as.numeric(CF$upper[, 2])

oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


# Table2 <-
#   oil %>%
#   filter(Date >= as.Date("2022-03-01"),
#          Date <= as.Date("2022-12-01")) %>%
#   
#   transmute(
#     
#     Period=paste(Month,Year),
#     
#     Actual=round(ActualCost,2),
#     
#     Counterfactual=round(Counterfactual,2),
#     
#     TotalExtraCost=round(ExtraCost,2)
#     
#   )
# 
# Table2
# c(sum(Table2$Actual, na.rm = T), sum(Table2$Counterfactual), sum(Table2$TotalExtraCost))

p1 <- oil %>%
  filter(Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual Expenditure"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "ARIMA Fit",
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none")

p2 <- oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual Expenditure"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  labs(
    title = "Forecast",
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none")





#########################
## Russia-Ukraine War: ARIMA Counterfactual of Expense: Baseline all months before war
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
  filter(Date <= as.Date("2022-02-01"))
fit.arima <- auto.arima(ts(baseline$ActualCost, frequency = 12), seasonal = F)
n_post <- sum(oil$Date > max(baseline$Date))
CF <- forecast(fit.arima, h = n_post)

oil$Counterfactual <- NA_real_
oil$Counterfactual[oil$Date >= min(baseline$Date) &
                     oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit.arima))
oil$Counterfactual[oil$Date > max(baseline$Date)] <- as.numeric(CF$mean)

oil$Lower95 <- NA_real_
oil$Lower95[oil$Date > max(baseline$Date)] <- as.numeric(CF$lower[, 2])
oil$Upper95 <- NA_real_
oil$Upper95[oil$Date > max(baseline$Date)] <- as.numeric(CF$upper[, 2])

oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


# Table2 <-
#   oil %>%
#   filter(Date >= as.Date("2022-03-01"),
#          Date <= as.Date("2022-12-01")) %>%
#   
#   transmute(
#     
#     Period=paste(Month,Year),
#     
#     Actual=round(ActualCost,2),
#     
#     Counterfactual=round(Counterfactual,2),
#     
#     TotalExtraCost=round(ExtraCost,2)
#     
#   )
# 
# Table2
# c(sum(Table2$Actual, na.rm = T), sum(Table2$Counterfactual), sum(Table2$TotalExtraCost))

p3 <- oil %>%
  filter(Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual Expenditure"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "ARIMA Fit",
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

p4 <- oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual Expenditure"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  labs(
    title = "Forecast",
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

library(patchwork)
(p1 | p2) / (p3 | p4)





#########################
## US-Iran Conflict: ARIMA Counterfactual of Expense: Baseline last 24 Months before conflict
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
fit.arima <- auto.arima(ts(baseline$ActualCost, frequency = 12), seasonal = F)
n_post <- sum(oil$Date > max(baseline$Date)) + 9
CF <- forecast(fit.arima, h = n_post)

new_rows <- data.frame(
  Year = rep(2026, 9),
  Month = month.abb[4:12], COI = NA_real_, Price = NA_real_, Rate = NA_real_,
  Expense = NA_real_, Barrels = NA_real_, ActualCost = NA_real_,  MillionBarrels = NA_real_,
  Date = seq(as.Date("2026-04-01"), as.Date("2026-12-01"), by = "month"))
oil <- rbind(oil, new_rows)


oil$Counterfactual <- NA_real_
oil$Counterfactual[oil$Date >= min(baseline$Date) &
                     oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit.arima))
oil$Counterfactual[oil$Date > max(baseline$Date)] <- as.numeric(CF$mean)

oil$Lower95 <- NA_real_
oil$Lower95[oil$Date > max(baseline$Date)] <- as.numeric(CF$lower[, 2])
oil$Upper95 <- NA_real_
oil$Upper95[oil$Date > max(baseline$Date)] <- as.numeric(CF$upper[, 2])

oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


# Table2 <-
#   oil %>%
#   filter(Date >= as.Date("2026-03-01"),
#          Date <= as.Date("2026-03-01")) %>%
#   
#   transmute(
#     
#     Period=paste(Month,Year),
#     
#     Actual=round(ActualCost,2),
#     
#     Counterfactual=round(Counterfactual,2),
#     
#     TotalExtraCost=round(ExtraCost,2)
#     
#   )
# 
# Table2
# c(sum(Table2$Actual, na.rm = T), sum(Table2$Counterfactual), sum(Table2$TotalExtraCost))

p5 <- oil %>%
  filter(Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual Expenditure"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "ARIMA Fit", 
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none")

p6 <- oil %>%
  filter(Date >= as.Date("2026-03-01"), 
         Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_point(
    aes(y = ActualCost,
        colour = "Actual Expenditure")
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  labs(
    title = "Forecast",
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none")





#########################
## US-Iran Conflict: ARIMA Counterfactual of Expense: Baseline all months before conflict
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
  filter(Date <= as.Date("2026-02-01"))
fit.arima <- auto.arima(ts(baseline$ActualCost, frequency = 12), seasonal = F)
n_post <- sum(oil$Date > max(baseline$Date)) + 9
CF <- forecast(fit.arima, h = n_post)

new_rows <- data.frame(
  Year = rep(2026, 9),
  Month = month.abb[4:12], COI = NA_real_, Price = NA_real_, Rate = NA_real_,
  Expense = NA_real_, Barrels = NA_real_, ActualCost = NA_real_,  MillionBarrels = NA_real_,
  Date = seq(as.Date("2026-04-01"), as.Date("2026-12-01"), by = "month"))
oil <- rbind(oil, new_rows)


oil$Counterfactual <- NA_real_
oil$Counterfactual[oil$Date >= min(baseline$Date) &
                     oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit.arima))
oil$Counterfactual[oil$Date > max(baseline$Date)] <- as.numeric(CF$mean)

oil$Lower95 <- NA_real_
oil$Lower95[oil$Date > max(baseline$Date)] <- as.numeric(CF$lower[, 2])
oil$Upper95 <- NA_real_
oil$Upper95[oil$Date > max(baseline$Date)] <- as.numeric(CF$upper[, 2])

oil <-
  oil %>%
  mutate(
    ExtraCost= ActualCost - Counterfactual
    
  )


# Table2 <-
#   oil %>%
#   filter(Date >= as.Date("2026-03-01"),
#          Date <= as.Date("2026-03-01")) %>%
#   
#   transmute(
#     
#     Period=paste(Month,Year),
#     
#     Actual=round(ActualCost,2),
#     
#     Counterfactual=round(Counterfactual,2),
#     
#     TotalExtraCost=round(ExtraCost,2)
#     
#   )
# 
# Table2
# c(sum(Table2$Actual, na.rm = T), sum(Table2$Counterfactual), sum(Table2$TotalExtraCost))

p7 <- oil %>%
  filter(Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual Expenditure"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "ARIMA Fit", 
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

p8 <- oil %>%
  filter(Date >= as.Date("2026-03-01"), 
         Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95,
        ymax = Upper95,
        fill = "95% Forecast Interval")
  ) +
  
  geom_point(
    aes(y = ActualCost,
        colour = "Actual Expenditure")
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Counterfactual"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual Expenditure" = "black",
      "Counterfactual" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "95% Forecast Interval" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  labs(
    title = "Forecast",
    x = "Date",
    y = "Expenditure (₹ Crore)"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

library(patchwork)
(p5 | p6) / (p7 | p8)3



