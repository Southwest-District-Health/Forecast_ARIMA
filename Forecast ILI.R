

library(tsibble)
library(fable)
library(feasts)
library(ggplot2)
library(fabletools)
library(purrr)
library(stringr)
library(dplyr)
library(lubridate)
library(fable)

library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(ggrepel)
library(purrr)
library(svglite)


pdraft <- read_excel("Data/Essence/ILI.xlsx")

pdraft$date <- as.Date(pdraft$date, format = "%YYY/%MM/%DD")



# Summarize daily cases into weekly incidence
Pertusis <- pdraft %>%
  rename(case = Data)



# Assuming 'Pertusis' dataset has a 'date' column for time and 'cases' column for the observed values
data <- as_tsibble(Pertusis, index = date)

# Ensure the date column is interpreted as yearweek
data <- data %>%
  mutate(date = yearweek(date)) %>%
  as_tsibble(index = date)

#Filter out latest month
data <- data %>%
  slice(-n())

#Forecasting

# Add 1 to cases to handle log(0) issues
data <- data %>%
  mutate(log_case = log(case + 1))

# Fit the ARIMA model
fit <- data %>%
  model(ARIMA(log_case))

# Create future data for forecasting

# Ensure date column is in Date format
data_new <- data %>%
  mutate(date = as.Date(date))

data_new$date <- data_new$date + 6 

# Generate future dates 
future_dates <- seq.Date(
  from = as.Date("2024-10-27"),
  to = as.Date("2024-11-20"),
  by = "week"
)

# Create a data frame with school closure information
future_data <- tibble(
  date = future_dates
)

# Print the future_data
print(future_data)


# Ensure the date column is interpreted as yearmonth
future_data <- future_data %>%
  mutate(date = yearweek(date)) %>%
  as_tsibble(index = date)


# Forecast
fc <- fit %>%
  forecast(new_data = future_data)

# Convert forecasted values from log_case to case
fc <- fc %>%
  mutate(
    case_forecast = exp(.mean) - 1,
    sd = c(0.087, 0.14, 0.19, 0.25)
  )

# Calculate the 80% CI for each row
fc <- fc %>%
  mutate(
    lower_ci_log = .mean - (1.28 * sd),
    upper_ci_log = .mean + (1.28 * sd),
    lower_ci  = exp(lower_ci_log) -1,
    upper_ci  = exp(upper_ci_log) -1
  )





# Combine original and forecasted data
plot_data <- data %>%
  select(date, case) %>%
  rename(value = case) %>%
  mutate(type = "Observed") %>%
  bind_rows(
    fc %>%
      as_tibble() %>%
      select(date, case_forecast, lower_ci, upper_ci) %>%
      rename(value = case_forecast) %>%
      mutate(type = "Forecast")
  )

# Ensure the date column is in Date format
plot_data$date <- as.Date(plot_data$date)


# Plot the data
plot_forecast <- ggplot(plot_data, aes(x = date, y = value, color = type)) +
  geom_line(size = 1) +
  geom_ribbon(data = filter(plot_data, type == "Forecast"), 
              aes(ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.2, fill = "lightgrey") +
  labs(title = "Observed and Forecasted Cases of ILI; Data as of 10/21/2024",
       subtitle = "Note that predictions extending beyond 2-3 data points exhibit increased uncertainty 
   and a higher likelihood of error",
       caption  = "Shaded area represents uncertainity interval",
       subtitle1 =  "",
       x = "Date",
       y = "Number of Cases",
       color = NULL) +
  theme_light() +
  scale_color_manual(values = c("Observed" = "pink", "Forecast" = "grey"))+
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), # Hide minor grid lines
        panel.grid.major.x = element_line(color = "lightgray", size = 0.5))

plot_forecast

# Save the plot as a .wmf file
ggsave("output_forecast.wmf", plot_forecast, device = "wmf")

ggsave("output_forecast.svg", plot_forecast, device = "svg")



#--------------------------------------------------------------------------------------#
###----------------------------------------------------------------------#########
#######-------------------------------------------------------------------#####

# Convert date column to Date format and ensure cases are numeric
Pertusis <- Pertusis %>%
  mutate(date = as.Date(date), case = as.numeric(case))


# Fit the exponential smoothing model
fit <- data %>%
  model(ets = ETS(case))


# Generate forecasts for the next 12 months
fc <- fit %>%
  forecast(h = "2 months")

# Visualize the forecasts
autoplot(data, case) +
  autolayer(fc, series = "Forecast") +
  labs(title = "Exponential Smoothing Forecast for Monthly Cases", 
       x = "Month", y = "Cases")
