library(tidyverse)
library(knitr)





##### IMPORT DATASET #####
oil_data <- read.csv(file.choose(), header = T) #import the csv file "Oil Import Data - 1.csv"

head(oil_data)
str(oil_data)





##### DATA PREPROCESSING #####
oil_data$Year <- as.integer(oil_data$Year)
oil_data$Month <- factor(oil_data$Month, ordered = T, levels = month.abb)
oil_data <- oil_data %>% mutate(Date = my(paste(Month, Year))) %>% arrange(Date)





##### DESCRIPTIVE STATISTICS #####
library(psych)
describe(oil_data)





##### TIME SERIES PLOT #####
### ALL ###
plot_price <- ggplot(oil_data, aes(Date, Price)) + geom_line(linewidth = 0.7) + 
  labs(title = "Crude Oil Price", y = "USD/Barrel", x = "Year") + theme_bw()

plot_quantity <- ggplot(oil_data, aes(Date, Quantity)) + geom_line(linewidth = 0.7) +
  labs(title = "Import Quantity", y = "Million Barrels", x = "Year") + theme_bw()

plot_rate <- ggplot(oil_data, aes(Date, Rate)) + geom_line(linewidth = 0.7) + 
  labs(title = "Exchange Rate", y = "INR/USD", x = "Year") + theme_bw()

plot_expense <- ggplot(oil_data, aes(Date, Expense)) + geom_line(linewidth = 0.7) + 
  labs(title = "Import Expenditure", y = "₹ Crore", x = "Year") + theme_bw()

library(patchwork)
(plot_price | plot_quantity) / (plot_rate | plot_expense)



### RAINCLOUD PLOT ###
library(ggpp)
library(ggdist)
violin_price <- ggplot(oil_data, aes(x = "", y = Price))+ 
  stat_halfeye(adjust = 0.5, width = 0.3, .width = 0, justification = -0.3, point_colour = NA, fill = "#B2DFDB") + 
  geom_boxplot(width = 0.1, outlier.shape = NA, linewidth = 0.7, color = "black") +
  geom_jitter(alpha = 1, size = 1.5, color = "#00796B",
              position = position_jitternudge(width = 0.05, height = 0, x = -0.15, nudge.from = "jittered")) + 
  labs(title = "Crude Oil Price", x = NULL, y = "USD/Barrel") + theme_bw()

violin_quantity <- ggplot(oil_data, aes(x = "", y = Quantity))+ 
  stat_halfeye(adjust = 0.5, width = 0.3, .width = 0, justification = -0.3, point_colour = NA, fill = "#B2DFDB") + 
  geom_boxplot(width = 0.1, outlier.shape = NA, linewidth = 0.7, color = "black") +
  geom_jitter(alpha = 1, size = 1.5, color = "#00796B",
              position = position_jitternudge(width = 0.05, height = 0, x = -0.15, nudge.from = "jittered")) + 
  labs(title = "Import Quantity", x = NULL, y = "Million Barrels") + theme_bw()

violin_rate <- ggplot(oil_data, aes(x = "", y = Rate))+ 
  stat_halfeye(adjust = 0.5, width = 0.3, .width = 0, justification = -0.3, point_colour = NA, fill = "#B2DFDB") + 
  geom_boxplot(width = 0.1, outlier.shape = NA, linewidth = 0.7, color = "black") +
  geom_jitter(alpha = 1, size = 1.5, color = "#00796B",
              position = position_jitternudge(width = 0.05, height = 0, x = -0.15, nudge.from = "jittered")) + 
  labs(title = "Exchange Rate", x = NULL, y = "INR/USD") + theme_bw()

violin_expense <- ggplot(oil_data, aes(x = "", y = Expense))+ 
  stat_halfeye(adjust = 0.5, width = 0.3, .width = 0, justification = -0.3, point_colour = NA, fill = "#B2DFDB") + 
  geom_boxplot(width = 0.1, outlier.shape = NA, linewidth = 0.7, color = "black") +
  geom_jitter(alpha = 1, size = 1.5, color = "#00796B",
              position = position_jitternudge(width = 0.05, height = 0, x = -0.15, nudge.from = "jittered")) + 
  labs(title = "Import Expenditure", x = NULL, y = "₹ Crore") + theme_bw()

(violin_price | violin_quantity) / (violin_rate | violin_expense)



library(zoo)
### PRICE ###
mean_price <- mean(oil_data$Price, na.rm = TRUE)
price_data <- oil_data %>% arrange(Date) %>% 
  mutate(above_mean = ifelse(Price >= mean_price, Price, mean_price), 
         below_mean = ifelse(Price < mean_price, Price, mean_price), 
         roll12 = rollmean(Price, k = 12, fill = NA, align = "center"))

ggplot(price_data, aes(x = Date)) +
  
  # Area above mean
  geom_ribbon(aes(ymin = mean_price, ymax = above_mean, fill = "Above Mean"), alpha = 0.5) +
  
  # Area below mean
  geom_ribbon(aes(ymin = below_mean, ymax = mean_price, fill = "Below Mean"), alpha = 0.5) +
  
  # Original time series
  geom_line(aes(y = Price, colour = "Original Series"), linewidth = 0.8) +
  
  # Mean line
  geom_hline(aes(yintercept = mean_price, colour = "Mean"), linetype = "dashed", linewidth = 1) +
  
  # 12-month rolling mean
  geom_line(aes(y = roll12, colour = "12-Month Rolling Mean"), linewidth = 1.2) +
  
  scale_fill_manual(values = c( "Above Mean" = "skyblue", "Below Mean" = "lightpink"), 
                    name = "Price Regime") +
  scale_colour_manual(values = c("Original Series" = "black", "12-Month Rolling Mean" = "blue", 
                                 "Mean" = "red"), name = "Series" ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Oil Price", x = "Date", y = "Price (USD/Barrel)") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### EXCHANGE RATE ###
mean_rate <- mean(oil_data$Rate, na.rm = TRUE)
rate_data <- oil_data %>% arrange(Date) %>% 
  mutate(above_mean = ifelse(Rate >= mean_rate, Rate, mean_rate), 
         below_mean = ifelse(Rate < mean_rate, Rate, mean_rate), 
         roll12 = rollmean(Rate, k = 12, fill = NA, align = "center"))

ggplot(rate_data, aes(x = Date)) +
  
  # Area above mean
  geom_ribbon(aes(ymin = mean_rate, ymax = above_mean, fill = "Weaker Rupee"), alpha = 0.5) +
  
  # Area below mean
  geom_ribbon(aes(ymin = below_mean, ymax = mean_rate, fill = "Stronger Rupee"), alpha = 0.5) +
  
  # Original time series
  geom_line(aes(y = Rate, colour = "Original Series"), linewidth = 0.8) +
  
  # Mean line
  geom_hline(aes(yintercept = mean_rate, colour = "Mean"), linetype = "dashed", linewidth = 1) +
  
  # 12-month rolling mean
  geom_line(aes(y = roll12, colour = "12-Month Rolling Mean"), linewidth = 1.2) +
  
  scale_fill_manual(values = c( "Weaker Rupee" = "skyblue", "Stronger Rupee" = "lightpink"), 
                    name = "Rupee Regime") +
  scale_colour_manual(values = c("Original Series" = "black", "12-Month Rolling Mean" = "blue", 
                                 "Mean" = "red"), name = "Series" ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Exchange Rate (INR/USD)", x = "Date", y = "Exchange Rate (INR/USD)") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### QUANTITY ###
mean_quantity <- mean(oil_data$Quantity, na.rm = TRUE)
quantity_data <- oil_data %>% arrange(Date) %>% 
  mutate(above_mean = ifelse(Quantity >= mean_quantity, Quantity, mean_quantity), 
         below_mean = ifelse(Quantity < mean_quantity, Quantity, mean_quantity), 
         roll12 = rollmean(Quantity, k = 12, fill = NA, align = "center"))

ggplot(quantity_data, aes(x = Date)) +
  
  # Area above mean
  geom_ribbon(aes(ymin = mean_quantity, ymax = above_mean, fill = "Higher Import"), alpha = 0.5) +
  
  # Area below mean
  geom_ribbon(aes(ymin = below_mean, ymax = mean_quantity, fill = "Lower Import"), alpha = 0.5) +
  
  # Original time series
  geom_line(aes(y = Quantity, colour = "Original Series"), linewidth = 0.8) +
  
  # Mean line
  geom_hline(aes(yintercept = mean_quantity, colour = "Mean"), linetype = "dashed", linewidth = 1) +
  
  # 12-month rolling mean
  geom_line(aes(y = roll12, colour = "12-Month Rolling Mean"), linewidth = 1.2) +
  
  scale_fill_manual(values = c( "Higher Import" = "skyblue", "Lower Import" = "lightpink"), 
                    name = "Import Regime") +
  scale_colour_manual(values = c("Original Series" = "black", "12-Month Rolling Mean" = "blue", 
                                 "Mean" = "red"), name = "Series" ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Crude Oil Import Quantity (Million Barrels)", x = "Date", y = "Import (Million Barrels)") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))



### EXPENSE ###
mean_expense <- mean(oil_data$Expense, na.rm = TRUE)
expense_data <- oil_data %>% arrange(Date) %>% 
  mutate(above_mean = ifelse(Expense >= mean_expense, Expense, mean_expense), 
         below_mean = ifelse(Expense < mean_expense, Expense, mean_expense), 
         roll12 = rollmean(Expense, k = 12, fill = NA, align = "center"))

ggplot(expense_data, aes(x = Date)) +
  
  # Area above mean
  geom_ribbon(aes(ymin = mean_expense, ymax = above_mean, fill = "Higher Expense"), alpha = 0.5) +
  
  # Area below mean
  geom_ribbon(aes(ymin = below_mean, ymax = mean_expense, fill = "Lower Expense"), alpha = 0.5) +
  
  # Original time series
  geom_line(aes(y = Expense, colour = "Original Series"), linewidth = 0.8) +
  
  # Mean line
  geom_hline(aes(yintercept = mean_expense, colour = "Mean"), linetype = "dashed", linewidth = 1) +
  
  # 12-month rolling mean
  geom_line(aes(y = roll12, colour = "12-Month Rolling Mean"), linewidth = 1.2) +
  
  scale_fill_manual(values = c( "Higher Expense" = "skyblue", "Lower Expense" = "lightpink"), 
                    name = "Expense Regime") +
  scale_colour_manual(values = c("Original Series" = "black", "12-Month Rolling Mean" = "blue", 
                                 "Mean" = "red"), name = "Series" ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Crude Oil Import Expenditure", x = "Date", y = "Expenditure (₹ Crore)") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))





#### BASE PERIOD ANALYSIS #####
period_summary <- oil_data %>% mutate(Period = case_when(
  Year %in% c(2012, 2013) ~ "2012-2013", Year %in% c(2019, 2020) ~ "2019-2020", 
  Year %in% c(2025, 2026) ~ "2025-2026", TRUE ~ NA_character_)) %>% 
  filter(!is.na(Period)) %>% group_by(Period) %>%
  summarise(OilPrice = mean(Price, na.rm = TRUE), CrudeImports = mean(Quantity, na.rm = TRUE), 
            ExchangeRate = mean(Rate, na.rm = TRUE), ImportExpenditure = mean(Expense, na.rm = TRUE))
period_summary

base <- period_summary %>% filter(Period == "2012-2013")
mid <- period_summary %>% filter(Period == "2019-2020")
recent <- period_summary %>% filter(Period == "2025-2026")

comparison_table <- data.frame(
  Variable = c("Oil Price (USD/barrel)", "Exchange Rate (INR/USD)", 
               "Crude Oil Imports (Million Barrels)", "Import Expenditure (Crore)"), 
  Avg_2012_13 = c(base$OilPrice, base$ExchangeRate,  base$CrudeImports, base$ImportExpenditure), 
  Avg_2019_20 = c(mid$OilPrice, mid$ExchangeRate, mid$CrudeImports, mid$ImportExpenditure), 
  PctChange_2019_20 = round(100 * (c(mid$OilPrice,mid$ExchangeRate, mid$CrudeImports, mid$ImportExpenditure) - 
                                     c(base$OilPrice, base$ExchangeRate, base$CrudeImports, base$ImportExpenditure)) / 
                              c(base$OilPrice, base$ExchangeRate, base$CrudeImports, base$ImportExpenditure), 2), 
  Avg_2025_26 = c(recent$OilPrice, recent$ExchangeRate, recent$CrudeImports, recent$ImportExpenditure), 
  PctChange_2025_26 = round(100 * (c(recent$OilPrice, recent$ExchangeRate, 
                                     recent$CrudeImports, recent$ImportExpenditure) - 
                                     c(base$OilPrice, base$ExchangeRate, base$CrudeImports, base$ImportExpenditure)) / 
                              c(base$OilPrice, base$ExchangeRate, base$CrudeImports, base$ImportExpenditure), 2))
comparison_table





##### MAJOR GLOBAL EVENTS #####
event_summary <- oil_data %>%
  mutate(
    Event = case_when(
      Date >= as.Date("2019-01-01") & Date <= as.Date("2020-02-29") ~ "Pre-COVID",
      Date >= as.Date("2020-03-01") & Date <= as.Date("2021-02-28") ~ "COVID-19 Pandemic",
      Date >= as.Date("2022-03-01") & Date <= as.Date("2022-12-31") ~ "Russia--Ukraine War",
      Date >= as.Date("2023-01-01") & Date <= as.Date("2023-12-31") ~ "Post-war Stabilization",
      Date >= as.Date("2026-03-01") & Date <= as.Date("2026-03-31") ~ "Iran--USA Conflict",
      TRUE ~ NA_character_
    ),
    Period = case_when(
      Event == "Pre-COVID" ~ "Jan 2019--Feb 2020",
      Event == "COVID-19 Pandemic" ~ "Mar 2020--Feb 2021",
      Event == "Russia--Ukraine War" ~ "Mar 2022--Dec 2022",
      Event == "Post-war Stabilization" ~ "Jan 2023--Dec 2023",
      Event == "Iran--USA Conflict" ~ "Mar 2026"
    )
  ) %>%
  filter(!is.na(Event)) %>%
  group_by(Event, Period) %>%
  summarise(
    Price_mean = mean(Price, na.rm = TRUE),
    Price_sd   = sd(Price, na.rm = TRUE),
    Rate_mean  = mean(Rate, na.rm = TRUE),
    Rate_sd    = sd(Rate, na.rm = TRUE),
    Qty_mean   = mean(Quantity, na.rm = TRUE),
    Qty_sd     = sd(Quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Price = sprintf("%.2f (%.2f)", Price_mean, Price_sd),
    Rate  = sprintf("%.2f (%.2f)", Rate_mean, Rate_sd),
    Quantity = sprintf("%.2f (%.2f)", Qty_mean, Qty_sd)
  ) %>%
  select(Event, Period, Price, Rate, Quantity)

event_summary

kable(
  event_summary,
  booktabs = TRUE,
  align = c("l", "l", "c", "c", "c"),
  col.names = c(
    "Event",
    "Period",
    "Price (USD/bbl)",
    "Rate (INR/USD)",
    "Quantity (Million bbl/month)"
  ),
  caption = "Average Crude Oil Market Indicators (standard deviations in parentheses) across Major Global Events."
)





