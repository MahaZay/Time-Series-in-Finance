# iPhone Impact Analysis (Time Series Analysis)

This project investigates the influence of Apple's iPhone product announcements on its stock performance. Through time series analysis and statistical modeling, we examine trends, volatility, and market reactions around these high-profile events.

## Project Overview

Apple's iPhone launches are significant corporate events that can influence investor sentiment and stock prices. This project aims to assess these impacts using historical data, focusing on identifying patterns and anomalies in stock price movements before, during, and after product launches.

## Objectives

- **Analyze Trends:** Explore stock price behavior around iPhone announcement dates.
- **Model Volatility:** Use ARIMA and GARCH models to understand stock price trends and volatility patterns.
- **Quantify Impact:** Test hypotheses to determine if announcements significantly affect stock performance.

## Data Sources

- **Stock Price Data:** Daily closing prices of Apple Inc. (AAPL), sourced from Yahoo Finance.
- **Event Data:** iPhone announcement dates (2007–2023), compiled from Apple press releases and online archives.

## Methodology

1. **Data Preparation**
   - Clean stock price data and align event dates.
   - Address missing values and ensure stationarity.

2. **Exploratory Data Analysis**
   - Visualize trends and volatility in stock prices.
   - Highlight key events (announcement dates) on time series plots.

3. **Modeling**
   - **ARIMA Models:** Capture trends and seasonal patterns in stock prices.
   - **GARCH Models:** Analyze volatility clustering around iPhone announcements.
   - **Event Study Framework:** Use statistical tests to evaluate stock price reactions.

4. **Hypothesis Testing**
   - Null Hypothesis (H₀): iPhone announcements do not significantly affect stock prices.
   - Alternative Hypothesis (H₁): iPhone announcements significantly influence stock prices.

## Results

- **ARIMA:** Captured stock price trends with high accuracy.
- **GARCH:** Identified increased volatility around announcement dates.
- **Hypothesis Testing:** Extended event windows showed a statistically significant positive impact of iPhone announcements on stock prices.

## Technologies Used

- **Language:** R
- **Libraries:** `forecast`, `tseries`, `ggplot2`
- **Tools:** RStudio for modeling and visualization



