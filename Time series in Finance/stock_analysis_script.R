# load packages
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)

# Load the datasets
apple_stock_data <- read_csv("apple_stock_data.csv")
iphone_release_dates <- read_csv("iphone_release_dates_cleaned.csv")

# Convert 'Date' to datetime and remove rows with NA dates
apple_stock_data <- apple_stock_data %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Date)) %>%
  na.omit() # This line removes rows with any NA values

# Convert 'Announced' to datetime and remove rows with NA dates
iphone_release_dates <- iphone_release_dates %>%
  mutate(Announced = as.Date(Announced)) %>%
  filter(!is.na(Announced))

# Analyze the effect of the announcement date instead of the release date
# Create a dummy variable for iPhone announcement dates
apple_stock_data$AnnouncementEvent <- ifelse(apple_stock_data$Date %in% iphone_release_dates$Announced, 1, 0)

# Check for stationarity of the closing price
adf_test_result <- adf.test(apple_stock_data$Close, alternative = "stationary")
kpss_test_result <- kpss.test(apple_stock_data$Close, null = "Level")

# Print the test results
print(adf_test_result)
print(kpss_test_result)

# If the series is non-stationary, difference the series
apple_stock_data <- apple_stock_data %>%
  mutate(DiffClose = c(NA, diff(Close))) %>%
  filter(!is.na(DiffClose)) # Remove NA values introduced by differencing

# Recheck stationarity with ADF test on the differenced data
adf_test_result_diff <- adf.test(apple_stock_data$DiffClose, alternative = "stationary")
print(adf_test_result_diff)

# Fit an ARIMA model and use auto.arima to select the best order (p,d,q)
best_arima_model <- auto.arima(apple_stock_data$DiffClose)

# Display the best ARIMA model's summary
summary(best_arima_model)

# Include the announcement effect as an exogenous variable in the ARIMA model
best_arimax_model <- auto.arima(apple_stock_data$DiffClose, xreg = apple_stock_data$AnnouncementEvent)

# Display the best ARIMAX model's summary
summary(best_arimax_model)

# Save the updated apple_stock_data if necessary
#write_csv(apple_stock_data, "/mnt/data/updated_apple_stock_data.csv")

#######################################################################################
# Continue from the previous analysis...

# Re-check stationarity with KPSS test on the differenced data
kpss_test_result_diff <- kpss.test(apple_stock_data$DiffClose, null = "Level")
print(kpss_test_result_diff)

# Plot the ACF and PACF for the differenced data
acf(apple_stock_data$DiffClose, main="ACF of differenced series")
pacf(apple_stock_data$DiffClose, main="PACF of differenced series")

# Fit an ARIMA model without the announcement effect
arima_model_no_event <- auto.arima(apple_stock_data$DiffClose)

# Display the summary of the ARIMA model without the announcement effect
summary(arima_model_no_event)

# Compare the AIC of the ARIMAX model with and without the announcement effect
aic_arimax <- AIC(best_arimax_model)
aic_arima <- AIC(arima_model_no_event)
aic_comparison <- data.frame(Model = c("ARIMAX", "ARIMA"), AIC = c(aic_arimax, aic_arima))
print(aic_comparison)






####################################################

# Perform a Ljung-Box test on the residuals of the selected model
Box.test(best_arima_model$residuals, type = "Ljung-Box")

#The Box-Ljung test results you've obtained from testing the residuals of your ARIMA model
#indicate an X-squared value of 2.9764 with 1 degree of freedom and a p-value of 0.08448.
#This p-value is greater than the common significance level of 0.05, which suggests that
#the null hypothesis (that the residuals are independently distributed) cannot be rejected.
#In simpler terms, there's no strong evidence of autocorrelation in the residuals of your model.
#This is generally a good sign,
#indicating that your model has adequately captured the information in the time series data.


# Create a histogram of the residuals
hist(best_arima_model$residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Check for normality of the residuals with a Shapiro-Wilk test
shapiro.test(best_arima_model$residuals)



######
# Determine the number of observations to hold out for testing
n <- nrow(apple_stock_data)
k <- floor(0.2 * n)  # For example, holding out the last 20%

# Split the data
train_data <- head(apple_stock_data, n - k)
test_data <- tail(apple_stock_data, k)

# Fit the ARIMA model to the training set
arima_model <- auto.arima(train_data$Close)

# Forecast into the future, up to the length of the test set
forecasts <- forecast(arima_model, h=k)

# Compare the forecast to the actual observations in the test set
# Note: Ensure that the 'Close' column exists in the test_data
accuracy_metrics <- accuracy(forecasts, test_data$Close)

# Print the accuracy metrics
print(accuracy_metrics)




######################################





# Assuming apple_stock_data$Close is your closing price column
apple_stock_data$LogClose <- log(apple_stock_data$Close)  # Log transformation

# Fit ARIMA model on the transformed data
log_arima_model <- auto.arima(apple_stock_data$LogClose)



# Define a window around the announcement dates
window_size <- 30  # For example, 50 days before and after

# Create a wider event window around announcement dates
apple_stock_data$WiderEventWindow <- 0
for (date in iphone_release_dates$Announced) {
  apple_stock_data$WiderEventWindow <- ifelse(abs(as.numeric(apple_stock_data$Date - date)) <= window_size, 1, apple_stock_data$WiderEventWindow)
}

# Fit ARIMA model with the wider event window
event_arima_model <- auto.arima(apple_stock_data$Close, xreg = apple_stock_data$WiderEventWindow)






# Check residuals of the log-transformed ARIMA model
log_arima_residuals <- residuals(log_arima_model)

# Diagnostic plots for residuals
plot(log_arima_residuals, main="Diagnostic Plot of Log ARIMA Residuals")

# Perform Shapiro-Wilk normality test
shapiro.test(log_arima_residuals)

#The result of your Shapiro-Wilk normality test, with a W statistic of 0.94022
#and a p-value significantly smaller than 0.05 (p < 2.2e-16),
#strongly suggests that the residuals from your ARIMA model do not follow a normal distribution.
#In time series analysis, especially for models like ARIMA, ideally, 
#you want the residuals (errors) to be normally distributed. 
#This deviation from normality could be an indication to review your model,
#consider data transformation, or explore other modeling approaches. 
#It’s important to address this, as the assumption of normality underpins many 
#statistical inference techniques.


library(forecast)
transformed_data <- BoxCox(apple_stock_data$Close, lambda = BoxCox.lambda(apple_stock_data$Close))
best_arima_model_transformed <- auto.arima(transformed_data)


# Assuming you've already determined p, d, q values
refined_arima_model <- arima(transformed_data, order = c(5,1,4))

es_model <- ets(transformed_data)


# Plotting residuals of the ARIMA model
residuals <- residuals(best_arima_model_transformed)
plot(residuals)
acf(residuals) # Autocorrelation function plot
pacf(residuals) # Partial autocorrelation function plot









# ACF and PACF plots of residuals
acf(log_arima_residuals, main="ACF of Log ARIMA Residuals")
pacf(log_arima_residuals, main="PACF of Log ARIMA Residuals")

# Check stationarity of residuals (you can use ADF or KPSS tests as before)
adf.test(log_arima_residuals, alternative = "stationary")
kpss.test(log_arima_residuals, null = "Level")

#The Augmented Dickey-Fuller (ADF) and KPSS test results on the logarithm of ARIMA residuals
#indicate different aspects of stationarity:
  
  #ADF Test: A Dickey-Fuller value of -15.147 with a p-value of 0.01 suggests 
#that the log-transformed residuals are stationary. 
#The low p-value (below the common threshold of 0.05) allows us to reject the null hypothesis
#of a unit root, indicating stationarity.

#KPSS Test: A KPSS Level of 0.036776 with a p-value of 0.1 implies that
#the null hypothesis of level stationarity cannot be rejected. 
#This means that the series does not exhibit a unit root, reinforcing the conclusion
#of stationarity.

#Together, these tests suggest that the log-transformed residuals of your ARIMA model 
#are STATIONARY, an important property for many time series analyses.


########################################################################

#########################################################################

### Step 1: Addressing Non-Normal Residuals with Transformation

# Assuming apple_stock_data$Close is your closing price column
apple_stock_data$LogClose <- log(apple_stock_data$Close)  # Log transformation

# Fit ARIMA model on the transformed data
log_arima_model <- auto.arima(apple_stock_data$LogClose)


### Step 2: Define a Wider Event Window

# Define a window around the announcement dates
window_size <- 30  # For example, 30 days before and after

# Create a wider event window around announcement dates
apple_stock_data$WiderEventWindow <- 0
for (date in iphone_release_dates$Announced) {
  apple_stock_data$WiderEventWindow <- ifelse(abs(as.numeric(apple_stock_data$Date - date)) <= window_size, 1, apple_stock_data$WiderEventWindow)
}

### Step 3: Fit ARIMA Model with the Wider Event Window

# Fit ARIMA model with the wider event window, use auto arima!
event_arima_model <- auto.arima(apple_stock_data$LogClose, xreg = apple_stock_data$WiderEventWindow)

### Step 4: Analyze the Impact of iPhone Announcements
# View the coefficients of the ARIMA model
coefficients(event_arima_model)


# Hypothesis test for the impact of iPhone announcements
summary(event_arima_model)



# Load the lmtest package if not already loaded
library(lmtest)

# Fit the ARIMA model with the wider event window (event_arima_model)
# Replace 'your_formula' with the actual formula used to fit the ARIMA model
# For example: your_formula <- Close ~ WiderEventWindow
your_formula <- Close ~ WiderEventWindow
arima_with_event <- lm(your_formula, data = apple_stock_data)

# Perform the hypothesis test for the coefficient of WiderEventWindow
# Replace 'your_coefficient_name' with the actual coefficient name (e.g., xregWiderEventWindow)
# For example: your_coefficient_name <- "WiderEventWindow"
your_coefficient_name <- "WiderEventWindow"
hypothesis_test <- coeftest(arima_with_event, hypothesis = your_coefficient_name)

# Print the hypothesis test results
print(hypothesis_test)

#The output of the hypothesis test for the coefficient of WiderEventWindow in the linear regression model
#provides the following results:

#Estimate: This is the estimated coefficient value for WiderEventWindow.
#In your case, it is approximately 12.85001.

#Std. Error: This is the standard error associated with the estimated coefficient. 
#It measures the variability of the coefficient estimate. In your case, it is approximately 2.06667.

#t value: This is the t-statistic, which measures how many standard errors the estimated 
#coefficient is away from zero. In your case, it is approximately 6.2177.

#Pr(>|t|): This is the p-value associated with the t-statistic. It indicates the probability of observing
#a t-statistic as extreme as the one calculated, assuming that the
#true coefficient value is zero (i.e., no effect).

#Now, let's interpret the results:

#Intercept (Intercept):

#Estimate: The estimated intercept value is approximately 47.21130.
#Std. Error: The standard error for the intercept is approximately 0.91848.
#t value: The t-statistic for the intercept is very large (approximately 51.4017).
#Pr(>|t|): The p-value associated with the intercept is extremely small (< 2.2e-16), 
#indicating that the intercept is highly statistically significant.
#This means that there is strong evidence that the intercept is not equal to zero.
#WiderEventWindow (Coefficient of Interest):

#Estimate: The estimated coefficient for WiderEventWindow is approximately 12.85001.
#Std. Error: The standard error for this coefficient is approximately 2.06667.
#t value: The t-statistic for this coefficient is approximately 6.2177.
#Pr(>|t|): The p-value associated with this coefficient is also very small (5.527e-10), indicating that the coefficient is highly statistically significant. This suggests that there is strong evidence of a statistically significant effect of iPhone announcements (WiderEventWindow) on the stock price.
#In summary, based on the results of the hypothesis test, 
#both the intercept and the coefficient of WiderEventWindow are highly statistically significant. 
#This means that there is strong evidence to suggest that 
# iPhone announcements have a statistically significant effect on the Apple stock price, 
# as the coefficient of WiderEventWindow is significantly different from zero.




# Coefficient of interest
coefficient_name <- "WiderEventWindow"

# Get the coefficient estimate
coefficient_estimate <- coef(arima_with_event)[coefficient_name]

# Check the sign of the coefficient
if (coefficient_estimate > 0) {
  cat("The effect of iPhone announcements is statistically positive.\n")
} else if (coefficient_estimate < 0) {
  cat("The effect of iPhone announcements is statistically negative.\n")
} else {
  cat("There is no statistically significant effect of iPhone announcements.\n")
}



###### The effect of iPhone announcements is statistically positive!!!!




############################################################################

# Load the tseries package
library(tseries)

# Fit an ARCH model
arch_model <- garch(apple_stock_data$DiffClose, order = c(1, 1))

# Display the summary of the ARCH model
summary(arch_model)

#EXPLANATION OF SUMMARY:
#GARCH(1,1) model indicates that both the autoregressive term 
#and the moving average term 
#are significant, meaning past data and past volatility are predictive of current volatility.
#The constant term 
#is also significant, representing the long-run average variance.
#The high significance of these terms suggests a good fit for
#the volatility pattern in Apple's stock price changes. 
#The Jarque-Bera test's rejection of normality is typical in financial returns 
#due to heavy tails. This doesn't negate the model but implies caution 
#in assuming normal distribution for risk metrics. The Box-Ljung test shows
#no autocorrelation in the residuals, meaning the model
#captures most of the time-dependent structure in the data.
####################################################
# Assuming fitted(arch_model) is a vector of the fitted values from the GARCH model
fitted_values <- as.numeric(fitted(arch_model))

# Create a new data frame for plotting
plot_data <- data.frame(
  Date = apple_stock_data$Date,
  DiffClose = apple_stock_data$DiffClose,
  Fitted = fitted_values
)

# Plot the actual vs. fitted values
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = DiffClose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(title = "Actual vs. Fitted GARCH(1,1) Model", x = "Date", y = "Differenced Close")



#####################################################


# Assuming apple_stock_data$AnnouncementEvent is a binary variable indicating announcement events
# and apple_stock_data$Close is the closing stock price

# Linear Regression Model
model <- lm(Close ~ AnnouncementEvent, data = apple_stock_data)

# Display the summary of the model
summary(model)

#The results of your linear regression model show that the coefficient for AnnouncementEvent
#is 8.4940 with a p-value of 0.483. This p-value is greater than the typical significance level
#of 0.05, suggesting that the effect of iPhone announcement events on Apple's
#closing stock price is not statistically significant in this model.
#The model's R-squared values are also very low, indicating that the AnnouncementEvent
#variable explains a very small portion of the variance in the stock price. 
#This analysis suggests that, according to this model, iPhone announcement events
#do not have a significant impact on the closing stock price of Apple, 
#at least not in a way captured by this simple linear model.


library(ggplot2)

# Plotting stock prices with annotations for announcement dates
ggplot(apple_stock_data, aes(x = Date, y = Close)) +
  geom_line() +
  geom_vline(data = iphone_release_dates, aes(xintercept = Announced), 
             color = "red", linetype = "dashed") +
  labs(title = "Apple Stock Price Over Time", 
       x = "Date", 
       y = "Closing Price")


##########################################################################



library(ggplot2)

# Assuming 'Date' is the date column and 'Close' is the closing price in apple_stock_data
# And 'Announced' is the column with announcement dates in iphone_release_dates

# Define the event window size (e.g., 10 days before and after the announcement)
window_size <- 30

# Create a new column in apple_stock_data for the event window
apple_stock_data$EventWindow <- 0
for (date in iphone_release_dates$Announced) {
  apple_stock_data$EventWindow <- ifelse(abs(difftime(apple_stock_data$Date, date, units = "days")) <= window_size, 1, apple_stock_data$EventWindow)
}

# plot of Apple Stock Price Around iPhone Announcement Dates
ggplot(apple_stock_data, aes(x = Date, y = Close)) +
  geom_line() +
  geom_vline(data = iphone_release_dates, aes(xintercept = as.Date(Announced)), color = "red", linetype = "dashed") +
  labs(title = "Apple Stock Price Around iPhone Announcement Dates",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()


# plot of Apple Stock Price Around iPhone Release Dates
ggplot(apple_stock_data, aes(x = Date, y = Close)) +
  geom_line() +
  geom_vline(data = iphone_release_dates, aes(xintercept = as.Date(Released)), color = "green", linetype = "dashed") +
  labs(title = "Apple Stock Price Around iPhone Release Dates",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()