################################################################################
# Install Libraries  
################################################################################
library(readxl)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)
library(ggbiplot)
library(PerformanceAnalytics)
library(texreg)
library(stats)
library(tseries)
library(writexl)
library(openxlsx)
library(forecast)
library(huxtable)
library(Hmisc)
library(modeest)
library(moments)
################################################################################
# Load Data set   
################################################################################
df <- read_excel("NVIDIA.xlsx")
View(df)
attach(df)
str(df)
head(df, 10)
tail(df, 10)
################################################################################
d_df <- df %>%
  mutate(d_CPR = c(NA, diff(Closing_Price)))
d_omit_df <- na.omit(d_df)
attach(d_omit_df)
df$Date <- as.Date(df$Date)
ts_data <- ts(df$Closing_Price, start = c(year(min(df$Date)), month(min(df$Date))), frequency = 12)
print(ts_data)
# Graphical Visualization    
ggplot(df, aes(x = Date, y = Closing_Price)) +
  geom_line(color = "blue", size = 1) +  
  labs(title = "Time Series of NVIDIA Historical Stock Price",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  scale_y_continuous(breaks = pretty(df$Closing_Price, n = 10), 
                     limits = c(min(df$Closing_Price), max(df$Closing_Price) * 1.1), 
                     expand = expansion(mult = c(0, 0.1))) 

plot(ts_data, main = "Time Series of NVIDIA Historical Stock Price", xlab = 'Months', ylab = 'Stock Price')
# Summary Statistics   
NVIDIA_price_summary <- c(
  Mean = mean(df$Closing_Price),
  Median = median(df$Closing_Price),
  SD = sd(df$Closing_Price),
  Skewness = skewness(df$Closing_Price),
  Kurtosis = kurtosis(df$Closing_Price))
print(NVIDIA_price_summary)
# Check for Stationery  
adf_NVIDIA_level <- adf.test(ts_data)
print(adf_NVIDIA_level)
ts_NVIDIA_diff <- diff(ts_data)
adf_NVIDIA_d <- adf.test(ts_NVIDIA_diff)
print(adf_NVIDIA_d)
# Optimal ARIMA Model and Comparison 
model1 <- auto.arima(ts_data)
summary(model1)
model2 <- arima(ts_data, order = c(1,2,1))
summary(model2)
AIC(model1, model2)
BIC(model1, model2)
# Model Diagnostics
NVIDIA_ARIMA_model <- auto.arima(ts_data)
summary(NVIDIA_ARIMA_model)
Residuals <- residuals(NVIDIA_ARIMA_model)
checkresiduals(NVIDIA_ARIMA_model)
shapiro.test(Residuals)
# Model Evaluation Forecast future prices
forecast_NVIDIA <- forecast(NVIDIA_ARIMA_model, h = 12) 
print(forecast_NVIDIA)
plot(forecast_NVIDIA)

# Forecast and model performance
train <- head(Closing_Price, length(Closing_Price) - 12)
test <- tail(Closing_Price, 12)
NVIDIA_arima_forecast <- forecast(NVIDIA_ARIMA_model, h = 12)
accuracy(NVIDIA_arima_forecast, test)
# Comparison with other models
ets_model <- ets(train)
ets_forecast <- forecast(ets_model, h = 12)
accuracy(ets_forecast, test)

# Visualize the results
plot(forecast_NVIDIA, col = "pink")
legend("topleft", c("NVIDIA Actual", "ARIMA Forecasted"), col = c("pink", "blue"), lty = 1)
print(forecast_NVIDIA)
