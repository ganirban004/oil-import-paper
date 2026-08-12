# ============================================================
# SHINY APP
#
# Russia-Ukraine War:
# Actual Import Cost vs
# Baseline Average Counterfactual vs
# ARIMA Counterfactual
#
# Animation:
# Historical actual data are shown from the beginning.
# Post-war values are revealed month by month.
# X-axis is fixed through December 2022.
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(forecast)
library(scales)


# ============================================================
# 1. READ CSV FILE
# ============================================================

oil <- read.csv(
  file.choose(),
  header = TRUE
)

oil <- na.omit(oil)

head(oil)


# ============================================================
# 2. CONSTRUCT BASIC VARIABLES
# ============================================================

# Convert crude oil import quantity into barrels

oil$Barrels <-
  oil$COI * 1000 * 6.28981


# Actual import cost

oil$ActualCost <-
  (
    oil$Barrels *
      oil$Price *
      oil$Rate
  ) / 10^7


# Million barrels

oil$MillionBarrels <-
  oil$Barrels / 10^6


# ============================================================
# 3. CONSTRUCT DATE
# ============================================================

oil$Date <-
  as.Date(
    paste(
      oil$Year,
      oil$Month,
      "01"
    ),
    format = "%Y %b %d"
  )


# Sort by date

oil <-
  oil %>%
  arrange(Date)


# ============================================================
# ============================================================
# PART I
# BASELINE-AVERAGE COUNTERFACTUAL
# ============================================================
# ============================================================


# ============================================================
# 4. CONSTRUCT BASELINE
# ============================================================

baseline_avg <-
  oil %>%
  filter(
    Date >= as.Date("2021-03-01"),
    Date <= as.Date("2022-02-01")
  )


# Average quantity

Q0_avg <-
  mean(
    baseline_avg$Barrels
  )


# Average oil price

P0_avg <-
  mean(
    baseline_avg$Price
  )


# Average exchange rate

ER0_avg <-
  mean(
    baseline_avg$Rate
  )


# ============================================================
# 5. BASELINE-AVERAGE COUNTERFACTUAL IMPORT COST
# ============================================================

oil <-
  oil %>%
  mutate(
    
    Counterfactual_Average =
      Q0_avg *
      P0_avg *
      ER0_avg / 1e7
    
  )


# ============================================================
# 6. BASELINE-AVERAGE QUANTITY EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    QuantityEffect_Average =
      (
        Barrels -
          Q0_avg
      ) *
      Price *
      Rate / 1e7
    
  )


# ============================================================
# 7. BASELINE-AVERAGE OIL PRICE EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    OilEffect_Average =
      Barrels *
      (
        Price -
          P0_avg
      ) *
      Rate / 1e7
    
  )


# ============================================================
# 8. BASELINE-AVERAGE EXCHANGE RATE EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    ExchangeEffect_Average =
      Barrels *
      Price *
      (
        Rate -
          ER0_avg
      ) / 1e7
    
  )


# ============================================================
# 9. BASELINE-AVERAGE INTERACTION EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    Interaction_Average =
      Barrels *
      (
        Price -
          P0_avg
      ) *
      (
        Rate -
          ER0_avg
      ) / 1e7
    
  )


# ============================================================
# 10. BASELINE-AVERAGE TOTAL EXTRA COST
# ============================================================

oil <-
  oil %>%
  mutate(
    
    ExtraCost_Average =
      ActualCost -
      Counterfactual_Average
    
  )


# ============================================================
# 11. CHECK BASELINE DECOMPOSITION
# ============================================================

baseline_check <-
  oil %>%
  summarise(
    
    Difference =
      sum(
        
        ExtraCost_Average -
          
          (
            QuantityEffect_Average +
              OilEffect_Average +
              ExchangeEffect_Average +
              Interaction_Average
          ),
        
        na.rm = TRUE
        
      )
    
  )


print(
  "Baseline-average decomposition check:"
)

print(
  baseline_check
)


# ============================================================
# ============================================================
# PART II
# ARIMA COUNTERFACTUAL
# ============================================================
# ============================================================


# ============================================================
# 12. CONSTRUCT ARIMA BASELINE
# ============================================================

# EXACTLY AS IN YOUR ORIGINAL CODE:
# all observations up to February 2022

baseline_arima <-
  oil %>%
  filter(
    Date <= as.Date("2022-02-01")
  )


# ============================================================
# 13. NUMBER OF POST-Baseline OBSERVATIONS
# ============================================================

n_post <-
  sum(
    oil$Date >
      max(
        baseline_arima$Date
      )
  )


# ============================================================
# 14. ARIMA COUNTERFACTUAL QUANTITY
# ============================================================

fit_Q0 <-
  auto.arima(
    
    ts(
      baseline_arima$Barrels,
      frequency = 12
    ),
    
    seasonal = F
    
  )


cf_Q0 <-
  forecast(
    fit_Q0,
    h = n_post
  )


# Create Q0

oil$Q0 <- NA_real_


# Fitted values during the baseline period

oil$Q0[
  oil$Date >=
    min(
      baseline_arima$Date
    ) &
    oil$Date <=
    max(
      baseline_arima$Date
    )
] <-
  as.numeric(
    fitted(
      fit_Q0
    )
  )


# Forecast values after February 2022

oil$Q0[
  oil$Date >
    max(
      baseline_arima$Date
    )
] <-
  as.numeric(
    cf_Q0$mean
  )


# ============================================================
# 15. ARIMA COUNTERFACTUAL OIL PRICE
# ============================================================

fit_P0 <-
  auto.arima(
    
    ts(
      baseline_arima$Price,
      frequency = 12
    ),
    
    seasonal = F
    
  )


cf_P0 <-
  forecast(
    fit_P0,
    h = n_post
  )


# Create P0

oil$P0 <- NA_real_


# Fitted values

oil$P0[
  oil$Date >=
    min(
      baseline_arima$Date
    ) &
    oil$Date <=
    max(
      baseline_arima$Date
    )
] <-
  as.numeric(
    fitted(
      fit_P0
    )
  )


# Forecast values

oil$P0[
  oil$Date >
    max(
      baseline_arima$Date
    )
] <-
  as.numeric(
    cf_P0$mean
  )


# ============================================================
# 16. ARIMA COUNTERFACTUAL EXCHANGE RATE
# ============================================================

fit_ER0 <-
  auto.arima(
    
    ts(
      baseline_arima$Rate,
      frequency = 12
    ),
    
    seasonal = F
    
  )


cf_ER0 <-
  forecast(
    fit_ER0,
    h = n_post
  )


# Create ER0

oil$ER0 <- NA_real_


# Fitted values

oil$ER0[
  oil$Date >=
    min(
      baseline_arima$Date
    ) &
    oil$Date <=
    max(
      baseline_arima$Date
    )
] <-
  as.numeric(
    fitted(
      fit_ER0
    )
  )


# Forecast values

oil$ER0[
  oil$Date >
    max(
      baseline_arima$Date
    )
] <-
  as.numeric(
    cf_ER0$mean
  )


# ============================================================
# 17. ARIMA COUNTERFACTUAL IMPORT COST
# ============================================================

oil <-
  oil %>%
  mutate(
    
    Counterfactual_ARIMA =
      Q0 *
      P0 *
      ER0 / 1e7
    
  )


# ============================================================
# 18. ARIMA QUANTITY EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    QuantityEffect_ARIMA =
      (
        Barrels -
          Q0
      ) *
      Price *
      Rate / 1e7
    
  )


# ============================================================
# 19. ARIMA OIL PRICE EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    OilEffect_ARIMA =
      Barrels *
      (
        Price -
          P0
      ) *
      Rate / 1e7
    
  )


# ============================================================
# 20. ARIMA EXCHANGE RATE EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    ExchangeEffect_ARIMA =
      Barrels *
      Price *
      (
        Rate -
          ER0
      ) / 1e7
    
  )


# ============================================================
# 21. ARIMA INTERACTION EFFECT
# ============================================================

oil <-
  oil %>%
  mutate(
    
    Interaction_ARIMA =
      Barrels *
      (
        Price -
          P0
      ) *
      (
        Rate -
          ER0
      ) / 1e7
    
  )


# ============================================================
# 22. ARIMA TOTAL EXTRA COST
# ============================================================

oil <-
  oil %>%
  mutate(
    
    ExtraCost_ARIMA =
      ActualCost -
      Counterfactual_ARIMA
    
  )


# ============================================================
# 23. CHECK ARIMA DECOMPOSITION
# ============================================================

arima_check <-
  oil %>%
  filter(
    Date >
      as.Date("2022-02-01")
  ) %>%
  summarise(
    
    Difference =
      sum(
        
        ExtraCost_ARIMA -
          
          (
            QuantityEffect_ARIMA +
              OilEffect_ARIMA +
              ExchangeEffect_ARIMA +
              Interaction_ARIMA
          ),
        
        na.rm = TRUE
        
      )
    
  )


print(
  "ARIMA decomposition check:"
)

print(
  arima_check
)


# ============================================================
# ============================================================
# PART III
# DATA FOR ANIMATION
# ============================================================
# ============================================================


# ============================================================
# 24. POST-WAR PERIOD
# ============================================================

app_data <-
  oil %>%
  filter(
    
    Date >=
      as.Date("2022-03-01"),
    
    Date <=
      as.Date("2022-12-01")
    
  ) %>%
  arrange(Date)


# Month number for animation

app_data$MonthNumber <-
  seq_len(
    nrow(app_data)
  )


# ============================================================
# 25. HISTORICAL ACTUAL DATA
# ============================================================

historical_actual <-
  oil %>%
  filter(
    Date <=
      as.Date("2022-02-01")
  ) %>%
  arrange(Date)


# ============================================================
# ============================================================
# PART IV
# SHINY UI
# ============================================================
# ============================================================


ui <-
  dashboardPage(
    
    
    # ========================================================
    # HEADER
    # ========================================================
    
    dashboardHeader(
      
      title =
        "Russia–Ukraine War"
      
    ),
    
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    dashboardSidebar(
      
      sidebarMenu(
        
        
        menuItem(
          
          "Counterfactual Analysis",
          
          tabName =
            "counterfactual",
          
          icon =
            icon(
              "chart-line"
            )
          
        ),
        
        
        menuItem(
          
          "Cost Decomposition",
          
          tabName =
            "decomposition",
          
          icon =
            icon(
              "chart-pie"
            )
          
        )
        
      ),
      
      
      br(),
      
      
      # ------------------------------------------------------
      # ANIMATION SLIDER
      # ------------------------------------------------------
      
      sliderInput(
        
        inputId =
          "month",
        
        label =
          "Forecasted Month:",
        
        min =
          1,
        
        max =
          nrow(
            app_data
          ),
        
        value =
          1,
        
        step =
          1,
        
        animate =
          animationOptions(
            
            interval =
              1800,
            
            loop =
              FALSE
            
          )
        
      ),
      
      
      br(),
      
      
      helpText(
        
        "Press the play button to reveal ",
        "the forecasted months one by one."
        
      )
      
    ),
    
    
    # ========================================================
    # BODY
    # ========================================================
    
    dashboardBody(
      
      
      # ------------------------------------------------------
      # CUSTOM CSS
      # ------------------------------------------------------
      
      tags$head(
        
        tags$style(
          
          HTML(
            
            "

            .content-wrapper {

              background-color:
                #f5f5f5;

            }


            .box {

              border-radius:
                8px;

            }


            .small-box {

              border-radius:
                8px;

            }


            .main-header .logo {

              font-weight:
                bold;

            }

            "
            
          )
          
        )
        
      ),
      
      
      tabItems(
        
        
        # ====================================================
        # TAB 1
        # COUNTERFACTUAL ANALYSIS
        # ====================================================
        
        tabItem(
          
          
          tabName =
            "counterfactual",
          
          
          # --------------------------------------------------
          # CURRENT MONTH
          # --------------------------------------------------
          
          fluidRow(
            
            box(
              
              width =
                12,
              
              title =
                "Current Forecasted Month",
              
              status =
                "primary",
              
              solidHeader =
                TRUE,
              
              
              h2(
                
                textOutput(
                  "current_period"
                ),
                
                style =
                  "text-align:center;"
                
              )
              
            )
            
          ),
          
          
          # --------------------------------------------------
          # THREE COST BOXES
          # --------------------------------------------------
          
          fluidRow(
            
            
            valueBoxOutput(
              
              "actual_box",
              
              width =
                4
              
            ),
            
            
            valueBoxOutput(
              
              "baseline_box",
              
              width =
                4
              
            ),
            
            
            valueBoxOutput(
              
              "arima_box",
              
              width =
                4
              
            )
            
          ),
          
          
          # --------------------------------------------------
          # MAIN COST PLOT
          # --------------------------------------------------
          
          fluidRow(
            
            box(
              
              width =
                12,
              
              title =
                "Actual Cost vs Counterfactual Costs",
              
              status =
                "primary",
              
              solidHeader =
                TRUE,
              
              
              plotOutput(
                
                "cost_plot",
                
                height =
                  "600px"
                
              )
              
            )
            
          ),
          
          
          # --------------------------------------------------
          # EXTRA COST
          # --------------------------------------------------
          
          fluidRow(
            
            
            valueBoxOutput(
              
              "baseline_extra_box",
              
              width =
                6
              
            ),
            
            
            valueBoxOutput(
              
              "arima_extra_box",
              
              width =
                6
              
            )
            
          ),
          
          
          # --------------------------------------------------
          # CUMULATIVE EXTRA COST
          # --------------------------------------------------
          
          fluidRow(
            
            box(
              
              width =
                12,
              
              title =
                "Cumulative Additional Import Cost",
              
              status =
                "info",
              
              solidHeader =
                TRUE,
              
              
              plotOutput(
                
                "cumulative_plot",
                
                height =
                  "450px"
                
              )
              
            )
            
          )
          
        ),
        
        
        # ====================================================
        # TAB 2
        # DECOMPOSITION
        # ====================================================
        
        tabItem(
          
          
          tabName =
            "decomposition",
          
          
          fluidRow(
            
            box(
              
              width =
                12,
              
              title =
                "ARIMA Counterfactual Cost Decomposition",
              
              status =
                "primary",
              
              solidHeader =
                TRUE,
              
              
              plotOutput(
                
                "decomposition_plot",
                
                height =
                  "500px"
                
              )
              
            )
            
          ),
          
          
          fluidRow(
            
            box(
              
              width =
                12,
              
              title =
                "Monthly Decomposition",
              
              status =
                "info",
              
              solidHeader =
                TRUE,
              
              
              tableOutput(
                
                "decomposition_table"
                
              )
              
            )
            
          )
          
        )
        
      )
      
    )
    
  )


# ============================================================
# ============================================================
# PART V
# SHINY SERVER
# ============================================================
# ============================================================


server <-
  function(
    input,
    output,
    session
  ) {
    
    
    # ========================================================
    # CURRENT DATA
    # ========================================================
    
    current_data <-
      reactive({
        
        app_data[
          input$month,
        ]
        
      })
    
    
    # ========================================================
    # CURRENT PERIOD
    # ========================================================
    
    output$current_period <-
      renderText({
        
        format(
          
          current_data()$Date,
          
          "%B %Y"
          
        )
        
      })
    
    
    # ========================================================
    # ACTUAL COST BOX
    # ========================================================
    
    output$actual_box <-
      renderValueBox({
        
        valueBox(
          
          value =
            
            paste0(
              
              "₹ ",
              
              comma(
                
                round(
                  
                  current_data()$ActualCost,
                  
                  2
                  
                )
                
              )
              
            ),
          
          subtitle =
            "Actual Import Cost",
          
          icon =
            icon(
              "oil-can"
            ),
          
          color =
            "blue"
          
        )
        
      })
    
    
    # ========================================================
    # BASELINE AVERAGE BOX
    # ========================================================
    
    output$baseline_box <-
      renderValueBox({
        
        valueBox(
          
          value =
            
            paste0(
              
              "₹ ",
              
              comma(
                
                round(
                  
                  current_data()$
                    Counterfactual_Average,
                  
                  2
                  
                )
                
              )
              
            ),
          
          subtitle =
            "Baseline Average Counterfactual",
          
          icon =
            icon(
              "calculator"
            ),
          
          color =
            "green"
          
        )
        
      })
    
    
    # ========================================================
    # ARIMA BOX
    # ========================================================
    
    output$arima_box <-
      renderValueBox({
        
        valueBox(
          
          value =
            
            paste0(
              
              "₹ ",
              
              comma(
                
                round(
                  
                  current_data()$
                    Counterfactual_ARIMA,
                  
                  2
                  
                )
                
              )
              
            ),
          
          subtitle =
            "ARIMA Counterfactual",
          
          icon =
            icon(
              "chart-line"
            ),
          
          color =
            "yellow"
          
        )
        
      })
    
    
    # ========================================================
    # BASELINE EXTRA COST BOX
    # ========================================================
    
    output$baseline_extra_box <-
      renderValueBox({
        
        extra <-
          current_data()$
          ExtraCost_Average
        
        
        valueBox(
          
          value =
            
            paste0(
              
              "₹ ",
              
              comma(
                
                round(
                  extra,
                  2
                )
                
              )
              
            ),
          
          subtitle =
            "Extra Cost vs Baseline Average",
          
          icon =
            icon(
              "arrow-up"
            ),
          
          color =
            
            ifelse(
              
              extra >= 0,
              
              "red",
              
              "green"
              
            )
          
        )
        
      })
    
    
    # ========================================================
    # ARIMA EXTRA COST BOX
    # ========================================================
    
    output$arima_extra_box <-
      renderValueBox({
        
        extra <-
          current_data()$
          ExtraCost_ARIMA
        
        
        valueBox(
          
          value =
            
            paste0(
              
              "₹ ",
              
              comma(
                
                round(
                  extra,
                  2
                )
                
              )
              
            ),
          
          subtitle =
            "Extra Cost vs ARIMA Counterfactual",
          
          icon =
            icon(
              "arrow-up"
            ),
          
          color =
            
            ifelse(
              
              extra >= 0,
              
              "red",
              
              "green"
              
            )
          
        )
        
      })
    
    
    # ========================================================
    # ========================================================
    # MAIN COST PLOT
    # ========================================================
    # ========================================================
    
    output$cost_plot <-
      renderPlot({
        
        
        # ----------------------------------------------------
        # Current animation step
        # ----------------------------------------------------
        
        k <-
          input$month
        
        
        # ----------------------------------------------------
        # Historical actual data
        #
        # This remains visible in EVERY frame.
        # ----------------------------------------------------
        
        historical_data <-
          historical_actual %>%
          transmute(
            
            Date,
            
            Cost =
              ActualCost,
            
            Type =
              "Actual"
            
          )
        
        
        # ----------------------------------------------------
        # Reveal post-war data one month at a time
        # ----------------------------------------------------
        
        revealed_data <-
          app_data %>%
          filter(
            
            MonthNumber <=
              k
            
          )
        
        
        # ----------------------------------------------------
        # Actual post-war values revealed so far
        # ----------------------------------------------------
        
        revealed_actual <-
          revealed_data %>%
          transmute(
            
            Date,
            
            Cost =
              ActualCost,
            
            Type =
              "Actual"
            
          )
        
        
        # ----------------------------------------------------
        # Baseline average counterfactual
        # ----------------------------------------------------
        
        revealed_baseline <-
          revealed_data %>%
          transmute(
            
            Date,
            
            Cost =
              Counterfactual_Average,
            
            Type =
              "Baseline Average"
            
          )
        
        
        # ----------------------------------------------------
        # ARIMA counterfactual
        # ----------------------------------------------------
        
        revealed_arima <-
          revealed_data %>%
          transmute(
            
            Date,
            
            Cost =
              Counterfactual_ARIMA,
            
            Type =
              "ARIMA"
            
          )
        
        
        # ----------------------------------------------------
        # Combine actual data
        # ----------------------------------------------------
        
        actual_plot_data <-
          bind_rows(
            
            historical_data,
            
            revealed_actual
            
          )
        
        
        # ----------------------------------------------------
        # Plot
        # ----------------------------------------------------
        
        ggplot() +
          
          
          # ==================================================
        # ACTUAL COST
        # ==================================================
        
        geom_line(
          
          data =
            actual_plot_data,
          
          aes(
            
            x =
              Date,
            
            y =
              Cost,
            
            colour =
              "Actual"
            
          ),
          
          linewidth =
            1.3
          
        ) +
          
          
          # Actual post-war points
          
          geom_point(
            
            data =
              revealed_actual,
            
            aes(
              
              x =
                Date,
              
              y =
                Cost,
              
              colour =
                "Actual"
              
            ),
            
            size =
              3
            
          ) +
          
          
          # ==================================================
        # BASELINE COUNTERFACTUAL
        # ==================================================
        
        geom_line(
          
          data =
            revealed_baseline,
          
          aes(
            
            x =
              Date,
            
            y =
              Cost,
            
            colour =
              "Baseline Average"
            
          ),
          
          linewidth =
            1.3,
          
          linetype =
            "dashed"
          
        ) +
          
          
          geom_point(
            
            data =
              revealed_baseline,
            
            aes(
              
              x =
                Date,
              
              y =
                Cost,
              
              colour =
                "Baseline Average"
              
            ),
            
            size =
              3
            
          ) +
          
          
          # ==================================================
        # ARIMA COUNTERFACTUAL
        # ==================================================
        
        geom_line(
          
          data =
            revealed_arima,
          
          aes(
            
            x =
              Date,
            
            y =
              Cost,
            
            colour =
              "ARIMA"
            
          ),
          
          linewidth =
            1.3,
          
          linetype =
            "dotted"
          
        ) +
          
          
          geom_point(
            
            data =
              revealed_arima,
            
            aes(
              
              x =
                Date,
              
              y =
                Cost,
              
              colour =
                "ARIMA"
              
            ),
            
            size =
              3
            
          ) +
          
          
          # ==================================================
        # CURRENT MONTH VERTICAL LINE
        # ==================================================
        
        geom_vline(
          
          xintercept =
            
            as.numeric(
              
              current_data()$
                Date
              
            ),
          
          linetype =
            "longdash",
          
          linewidth =
            0.8
          
        ) +
          
          
          # ==================================================
        # FIXED X-AXIS
        #
        # IMPORTANT:
        # The axis already extends to December 2022.
        # ==================================================
        
        scale_x_date(
          
          limits = c(
            
            min(
              
              oil$Date,
              
              na.rm =
                TRUE
              
            ),
            
            as.Date(
              
              "2022-12-01"
              
            )
            
          ),
          
          date_breaks =
            "6 months",
          
          date_labels =
            "%b\n%Y",
          
          expand =
            c(
              0.01,
              0.01
            )
          
        ) +
          
          
          # ==================================================
        # Y-AXIS
        # ==================================================
        
        scale_y_continuous(
          
          labels =
            
            function(x) {
              
              paste0(
                
                "₹ ",
                
                comma(x)
                
              )
              
            }
          
        ) +
          
          
          # ==================================================
        # COLOURS
        # ==================================================
        
        scale_colour_manual(
          
          values = c(
            
            "Actual" =
              "#D62728",
            
            "Baseline Average" =
              "#2CA02C",
            
            "ARIMA" =
              "#1F77B4"
            
          )
          
        ) +
          
          
          # ==================================================
        # LABELS
        # ==================================================
        
        labs(
          
          title =
            "India's Crude Oil Import Cost",
          
          subtitle =
            
            paste(
              
              "Actual cost and counterfactuals revealed through",
              
              format(
                
                current_data()$
                  Date,
                
                "%B %Y"
                
              )
              
            ),
          
          x =
            NULL,
          
          y =
            "Import Cost",
          
          colour =
            NULL
          
        ) +
          
          
          # ==================================================
        # THEME
        # ==================================================
        
        theme_minimal(
          
          base_size =
            14
          
        ) +
          
          theme(
            
            legend.position =
              "top",
            
            plot.title =
              
              element_text(
                
                face =
                  "bold",
                
                size =
                  17
                
              ),
            
            plot.subtitle =
              
              element_text(
                
                size =
                  12
                
              ),
            
            axis.text.x =
              
              element_text(
                
                angle =
                  0,
                
                hjust =
                  0.5
                
              )
            
          )
        
      })
    
    
    # ========================================================
    # ========================================================
    # CUMULATIVE EXTRA COST PLOT
    # ========================================================
    # ========================================================
    
    output$cumulative_plot <-
      renderPlot({
        
        
        k <-
          input$month
        
        
        # ----------------------------------------------------
        # Only reveal post-war months progressively
        # ----------------------------------------------------
        
        revealed_data <-
          app_data %>%
          filter(
            
            MonthNumber <=
              k
            
          ) %>%
          mutate(
            
            Cumulative_Average =
              cumsum(
                ExtraCost_Average
              ),
            
            Cumulative_ARIMA =
              cumsum(
                ExtraCost_ARIMA
              )
            
          )
        
        
        # ----------------------------------------------------
        # Long format
        # ----------------------------------------------------
        
        cumulative_long <-
          bind_rows(
            
            revealed_data %>%
              transmute(
                
                Date,
                
                Cost =
                  Cumulative_Average,
                
                Type =
                  "Baseline Average"
                
              ),
            
            revealed_data %>%
              transmute(
                
                Date,
                
                Cost =
                  Cumulative_ARIMA,
                
                Type =
                  "ARIMA"
                
              )
            
          )
        
        
        # ----------------------------------------------------
        # Plot
        # ----------------------------------------------------
        
        ggplot(
          
          cumulative_long,
          
          aes(
            
            x =
              Date,
            
            y =
              Cost,
            
            colour =
              Type,
            
            group =
              Type
            
          )
          
        ) +
          
          geom_line(
            
            linewidth =
              1.3
            
          ) +
          
          geom_point(
            
            size =
              2.5
            
          ) +
          
          geom_hline(
            
            yintercept =
              0,
            
            linetype =
              "dashed"
            
          ) +
          
          scale_x_date(
            
            limits = c(
              
              as.Date(
                "2022-03-01"
              ),
              
              as.Date(
                "2022-12-01"
              )
              
            ),
            
            date_breaks =
              "2 months",
            
            date_labels =
              "%b"
            
          ) +
          
          scale_y_continuous(
            
            labels =
              
              function(x) {
                
                paste0(
                  
                  "₹ ",
                  
                  comma(x)
                  
                )
                
              }
            
          ) +
          
          scale_colour_manual(
            
            values = c(
              
              "Baseline Average" =
                "#2CA02C",
              
              "ARIMA" =
                "#1F77B4"
              
            )
            
          ) +
          
          labs(
            
            x =
              NULL,
            
            y =
              "Cumulative Additional Cost",
            
            colour =
              NULL
            
          ) +
          
          theme_minimal(
            
            base_size =
              14
            
          ) +
          
          theme(
            
            legend.position =
              "top",
            
            axis.text.x =
              
              element_text(
                
                angle =
                  0,
                
                hjust =
                  0.5
                
              )
            
          )
        
      })
    
    
    # ========================================================
    # ========================================================
    # DECOMPOSITION PLOT
    # ========================================================
    # ========================================================
    
    output$decomposition_plot <-
      renderPlot({
        
        
        d <-
          current_data()
        
        
        decomposition <-
          data.frame(
            
            Component = c(
              
              "Quantity",
              
              "Oil Price",
              
              "Exchange Rate",
              
              "Interaction"
              
            ),
            
            Effect = c(
              
              d$QuantityEffect_ARIMA,
              
              d$OilEffect_ARIMA,
              
              d$ExchangeEffect_ARIMA,
              
              d$Interaction_ARIMA
              
            )
            
          )
        
        
        ggplot(
          
          decomposition,
          
          aes(
            
            x =
              Component,
            
            y =
              Effect
            
          )
          
        ) +
          
          geom_col(
            
            aes(
              fill =
                Component
            ),
            
            width =
              0.7
            
          ) +
          
          geom_hline(
            
            yintercept =
              0,
            
            linewidth =
              0.8
            
          ) +
          
          scale_y_continuous(
            
            labels =
              
              function(x) {
                
                paste0(
                  
                  "₹ ",
                  
                  comma(x)
                  
                )
                
              }
            
          ) +
          
          labs(
            
            title =
              
              paste(
                
                "ARIMA Counterfactual Decomposition:",
                
                format(
                  
                  d$Date,
                  
                  "%B %Y"
                  
                )
                
              ),
            
            x =
              NULL,
            
            y =
              "Cost Effect"
            
          ) +
          
          theme_minimal(
            
            base_size =
              14
            
          ) +
          
          theme(
            
            legend.position =
              "none",
            
            axis.text.x =
              
              element_text(
                
                angle =
                  20,
                
                hjust =
                  1
                
              )
            
          )
        
      })
    
    
    # ========================================================
    # ========================================================
    # DECOMPOSITION TABLE
    # ========================================================
    # ========================================================
    
    output$decomposition_table <-
      renderTable({
        
        
        d <-
          current_data()
        
        
        data.frame(
          
          Component = c(
            
            "Quantity Effect",
            
            "Oil Price Effect",
            
            "Exchange Rate Effect",
            
            "Interaction",
            
            "Total Extra Cost"
            
          ),
          
          `Cost (₹)` =
            
            round(
              
              c(
                
                d$QuantityEffect_ARIMA,
                
                d$OilEffect_ARIMA,
                
                d$ExchangeEffect_ARIMA,
                
                d$Interaction_ARIMA,
                
                d$ExtraCost_ARIMA
                
              ),
              
              2
              
            )
          
        )
        
      },
      
      striped =
        TRUE,
      
      bordered =
        TRUE,
      
      hover =
        TRUE
      
      )
    
  }


# ============================================================
# 26. RUN SHINY APP
# ============================================================

shinyApp(
  
  ui =
    ui,
  
  server =
    server
  
)