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
oil <- read.csv("Oil Import Data.csv", header = TRUE)
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

n_post <- sum(oil$Date > max(baseline$Date))

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

oil$Lower95_Q0 <- NA_real_
oil$Lower95_Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$lower[, 2])
oil$Upper95_Q0 <- NA_real_
oil$Upper95_Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$upper[, 2])

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

oil$Lower95_P0 <- NA_real_
oil$Lower95_P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$lower[, 2])
oil$Upper95_P0 <- NA_real_
oil$Upper95_P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$upper[, 2])

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)

oil$Lower95_ER0 <- NA_real_
oil$Lower95_ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$lower[, 2])
oil$Upper95_ER0 <- NA_real_
oil$Upper95_ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$upper[, 2])


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
#     QuantityEffect = round(QuantityEffect,2),
# 
#     OilPriceEffect=round(OilEffect,2),
# 
#     ExchangeRateEffect=round(ExchangeEffect,2),
# 
#     TotalExtraCost=round(ExtraCost,2)
# 
#   )
# 
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

## Plot for Quantity
plot_quantity_1 <- oil %>%
  filter(Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_Q0 / 10^6,
        ymax = Upper95_Q0 / 10^6,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Barrels / 10^6,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Q0 / 10^6,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Quantity",
    x = "Date",
    y = "Million Barrels"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")

plot_quantity_2 <- oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_Q0 / 10^6,
        ymax = Upper95_Q0 / 10^6,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Barrels / 10^6,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Q0 / 10^6,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Quantity",
    x = "Date",
    y = "Million Barrels"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")




## Plot for Price
plot_price_1 <- oil %>%
  filter(Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_P0,
        ymax = Upper95_P0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Price,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = P0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Price",
    x = "Date",
    y = "USD/Barrel"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")

plot_price_2 <- oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_P0,
        ymax = Upper95_P0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Price,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = P0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Price",
    x = "Date",
    y = "USD/Barrel"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")



## Plot for Exchange Rate
plot_rate_1 <- oil %>%
  filter(Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_ER0,
        ymax = Upper95_ER0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Rate,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = ER0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Exchange Rate",
    x = "Date",
    y = "INR/USD"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

plot_rate_2 <- oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_ER0,
        ymax = Upper95_ER0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Rate,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = ER0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Exchange Rate",
    x = "Date",
    y = "INR/USD"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")




## Plot for Expenditure
plot_expense_1 <- oil %>%
  filter(Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_rect(
    data = data.frame(
      xmin = as.Date("2022-03-01"),
      xmax = as.Date("2022-12-01"),
      ymin = -Inf,
      ymax = Inf
    ),
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = "Counterfactual"
    ),
    inherit.aes = FALSE
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Forecast"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecast" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "Counterfactual" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "Expenditure",
    x = "Date",
    y = "₹ Crore"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

plot_expense_2 <- oil %>%
  filter(Date >= as.Date("2022-03-01"),
         Date <= as.Date("2022-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  # geom_rect(
  #   data = data.frame(
  #     xmin = as.Date("2022-03-01"),
  #     xmax = as.Date("2022-12-01"),
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   aes(
  #     xmin = xmin,
  #     xmax = xmax,
  #     ymin = ymin,
  #     ymax = ymax,
  #     fill = "Counterfactual"
  #   ),
  #   inherit.aes = FALSE
  # ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Forecast"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecast" = "blue"
    )
  ) +
  
  # scale_fill_manual(
  #   name = "",
  #   values = c(
  #     "Counterfactual" = "lightblue"
  #   )
  # ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  labs(
    title = "Expenditure (Counterfactual)",
    x = "Date",
    y = "₹ Crore"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

library(patchwork)
(plot_quantity_1 | plot_quantity_2) / (plot_price_1 | plot_price_2) / (plot_rate_1 | plot_rate_2) 
#/ (plot_expense_1 | plot_expense_2)





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
oil <- read.csv("Oil Import Data.csv", header = TRUE)
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

n_post <- sum(oil$Date > max(baseline$Date)) + 9

new_rows <- data.frame(
  Year = rep(2026, 9),
  Month = month.abb[4:12], COI = NA_real_, Price = NA_real_, Rate = NA_real_,
  Expense = NA_real_, Barrels = NA_real_, ActualCost = NA_real_,  MillionBarrels = NA_real_,
  Date = seq(as.Date("2026-04-01"), as.Date("2026-12-01"), by = "month"))
oil <- rbind(oil, new_rows)

##Counterfactual Quantity
fit_Q0 <- auto.arima(ts(baseline$Barrels, frequency = 12), seasonal = F)
cf_Q0 <- forecast(fit_Q0, h = n_post)
oil$Q0 <- NA_real_
oil$Q0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_Q0))
oil$Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$mean)

oil$Lower95_Q0 <- NA_real_
oil$Lower95_Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$lower[, 2])
oil$Upper95_Q0 <- NA_real_
oil$Upper95_Q0[oil$Date > max(baseline$Date)] <- as.numeric(cf_Q0$upper[, 2])

##Counterfactual Oil Price
fit_P0 <- auto.arima(ts(baseline$Price, frequency = 12), seasonal = F)
cf_P0 <- forecast(fit_P0, h = n_post)
oil$P0 <- NA_real_
oil$P0[oil$Date >= min(baseline$Date) & 
         oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_P0))
oil$P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$mean)

oil$Lower95_P0 <- NA_real_
oil$Lower95_P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$lower[, 2])
oil$Upper95_P0 <- NA_real_
oil$Upper95_P0[oil$Date > max(baseline$Date)] <- as.numeric(cf_P0$upper[, 2])

##Counterfactual Exchange Rate
fit_ER0 <- auto.arima(ts(baseline$Rate, frequency = 12), seasonal = F)
cf_ER0 <- forecast(fit_ER0, h = n_post)
oil$ER0 <- NA_real_
oil$ER0[oil$Date >= min(baseline$Date) & 
          oil$Date <= max(baseline$Date)] <- as.numeric(fitted(fit_ER0))
oil$ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$mean)

oil$Lower95_ER0 <- NA_real_
oil$Lower95_ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$lower[, 2])
oil$Upper95_ER0 <- NA_real_
oil$Upper95_ER0[oil$Date > max(baseline$Date)] <- as.numeric(cf_ER0$upper[, 2])


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



# Table2 <-
#   oil %>%
#   filter(Date >= as.Date("2026-03-01")) %>%
#   
#   transmute(
#     
#     Period=paste(Month,Year),
#     
#     Actual=round(ActualCost,2),
#     
#     Counterfactual=round(Counterfactual,2),
#     
#     QuantityEffect = round(QuantityEffect,2),
#     
#     OilPriceEffect=round(OilEffect,2),
#     
#     ExchangeRateEffect=round(ExchangeEffect,2),
#     
#     TotalExtraCost=round(ExtraCost,2)
#     
#   )
# 
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

## Plot for Quantity
plot_quantity_1 <- oil %>%
  filter(Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_Q0 / 10^6,
        ymax = Upper95_Q0 / 10^6,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Barrels / 10^6,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Q0 / 10^6,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Quantity",
    x = "Date",
    y = "Million Barrels"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")

plot_quantity_2 <- oil %>%
  filter(Date >= as.Date("2026-03-01"),
         Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_Q0 / 10^6,
        ymax = Upper95_Q0 / 10^6,
        fill = "95% Forecast Interval")
  ) +
  
  geom_point(
    aes(y = Barrels / 10^6,
        colour = "Actual")
  ) +
  
  geom_line(
    aes(y = Q0 / 10^6,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Quantity",
    x = "Date",
    y = "Million Barrels"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")




## Plot for Price
plot_price_1 <- oil %>%
  filter(Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_P0,
        ymax = Upper95_P0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Price,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = P0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Price",
    x = "Date",
    y = "USD/Barrel"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")

plot_price_2 <- oil %>%
  filter(Date >= as.Date("2026-03-01"),
         Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_P0,
        ymax = Upper95_P0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_point(
    aes(y = Price,
        colour = "Actual")
  ) +
  
  geom_line(
    aes(y = P0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Price",
    x = "Date",
    y = "USD/Barrel"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "none", legend.direction = "vertical")



## Plot for Exchange Rate
plot_rate_1 <- oil %>%
  filter(Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_ER0,
        ymax = Upper95_ER0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_line(
    aes(y = Rate,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = ER0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Exchange Rate",
    x = "Date",
    y = "INR/USD"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

plot_rate_2 <- oil %>%
  filter(Date >= as.Date("2026-03-01"),
         Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_ribbon(
    aes(ymin = Lower95_ER0,
        ymax = Upper95_ER0,
        fill = "95% Forecast Interval")
  ) +
  
  geom_point(
    aes(y = Rate,
        colour = "Actual")
  ) +
  
  geom_line(
    aes(y = ER0,
        colour = "Forecasted"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecasted" = "blue"
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
    title = "Exchange Rate",
    x = "Date",
    y = "INR/USD"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")




## Plot for Expenditure
plot_expense_1 <- oil %>%
  filter(Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  geom_rect(
    data = data.frame(
      xmin = as.Date("2026-03-01"),
      xmax = as.Date("2026-12-01"),
      ymin = -Inf,
      ymax = Inf
    ),
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = "Counterfactual"
    ),
    inherit.aes = FALSE
  ) +
  
  geom_line(
    aes(y = ActualCost,
        colour = "Actual"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Forecast"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecast" = "blue"
    )
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "Counterfactual" = "lightblue"
    )
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "Expenditure",
    x = "Date",
    y = "₹ Crore"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

plot_expense_2 <- oil %>%
  filter(Date >= as.Date("2026-03-01"),
         Date <= as.Date("2026-12-01")) %>%
  ggplot(aes(x = Date)) +
  
  # geom_rect(
  #   data = data.frame(
  #     xmin = as.Date("2022-03-01"),
  #     xmax = as.Date("2022-12-01"),
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   aes(
  #     xmin = xmin,
  #     xmax = xmax,
  #     ymin = ymin,
  #     ymax = ymax,
  #     fill = "Counterfactual"
  #   ),
  #   inherit.aes = FALSE
  # ) +
  
  geom_point(
    aes(y = ActualCost,
        colour = "Actual")
  ) +
  
  geom_line(
    aes(y = Counterfactual,
        colour = "Forecast"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    values = c(
      "Actual" = "black",
      "Forecast" = "blue"
    )
  ) +
  
  # scale_fill_manual(
  #   name = "",
  #   values = c(
  #     "Counterfactual" = "lightblue"
  #   )
  # ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  labs(
    title = "Expenditure (Counterfactual)",
    x = "Date",
    y = "₹ Crore"
  ) +
  
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                     legend.position = "bottom", legend.direction = "vertical")

library(patchwork)
(plot_quantity_1 | plot_quantity_2) / (plot_price_1 | plot_price_2) / (plot_rate_1 | plot_rate_2) 
#/ (plot_expense_1 | plot_expense_2)


