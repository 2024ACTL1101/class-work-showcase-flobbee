---
title: "ACTL1101 Assignment Part B"
author: "Danny Zhou"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyse the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
for (i in 1:nrow(df)) {
  if (i == 1) {
    df$daily_return_AMD <- NA
    df$daily_return_SP500 <- NA
  } # NA, since there is no previous day before 2019-05-20
  else {
    df$daily_return_AMD[i] <- ( df$AMD[i] - df$AMD[i-1] )/df$AMD[i-1]
    df$daily_return_SP500[i] <- ( df$GSPC[i] - df$GSPC[i-1] )/df$GSPC[i-1]
  } # Calculating the daily returns for AMD and S%P 500 using the given formula
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
for (i in 1:nrow(df)) {
  df$daily_RF[i] <- ( 1 + df$RF[i]/100 )^(1/360) - 1
} # Calculating the daily risk-free rate using the given formula
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
for (i in 1:nrow(df)) {
  if (i == 1) {
    df$excess_return_AMD <- NA
    df$excess_return_SP500 <- NA
  } # NA, since there is no daily return at 2019-05-20
  else {
    df$excess_return_AMD[i] <- df$daily_return_AMD[i] - df$daily_RF[i]
    df$excess_return_SP500[i] <- df$daily_return_SP500[i] - df$daily_RF[i]
  } # Calculating the excess returns for AMD and S%P 500 using the given formula
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
df_lm <- lm(excess_return_AMD ~ excess_return_SP500, data = df[-1, ]) # Remove 1st row
summary(df_lm) # Display summary
estimated_beta <- summary(df_lm)$coefficients[2, 1] # Extract slope from summary 
print(estimated_beta) # Display beta (slope)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:** $\beta_{AMD} \approx 1.57$. $\beta_{AMD} > 1$ implies that AMD stocks has more systematic risk (volatile) than the overall market (based on S&P 500). This means that AMD's stocks has high market sensitivity, that is, AMD's stocks is expected to move more significantly in the same direction as any market movements. When the market performs well, AMD is expected to outperform the market, providing investors significant returns. When the market performs poorly, AMD is expected to underperform the market, resulting in greater losses for investors. Thus, potential investors will demand higher returns than the S&P 500 market returns to compensate for bearing a higher risk.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Defining theme for plot
theme_rbook <- function(base_size = 13, 
                        base_family = "", 
                        base_line_size = base_size/22, 
                        base_rect_size = base_size/22) {
  theme(axis.title = element_text(size = 13),                               
    axis.text.x = element_text(size = 10),                              
    axis.text.y = element_text(size = 10),                              
    plot.caption = element_text(size = 10, face = "italic"),            
    panel.background = element_rect(fill="white"),                      
    axis.line = element_line(linewidth = 1, colour = "black"),
    strip.background = element_rect(fill = "#cddcdd"),
    panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
    strip.text = element_text(colour = "black"),
    legend.key = element_blank()
  )}

# Plotting scatter plot of AMD vs. S&P 500 excess returns
plot <- ggplot(aes(x = excess_return_SP500, y = excess_return_AMD), data = df[-1, ]) + # Remove 1st row
  geom_point() + 
  geom_smooth(method = "lm", colour = "#1D8BFF", se = TRUE, level = 0.90) + # CAPM regression line
  labs(title = "Relationship Between AMD and Market Excess Returns") +
  xlab("S&P 500 Excess Returns") +
  ylab("AMD Excess Returns") +
  theme_rbook()
print(plot) # Display scatter plot
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0% p.a, and the annual expected return for the S&P 500 is 13.3% p.a. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*

```{r pi}
# Defining the given values
annual_RF <- 0.05 # Annual risk-free rate
annual_ERM <- 0.133 # Annual expected return of the S&P 500 (market)
daily_excess_return_SP500 <- (annual_ERM - annual_RF) / 252 # Assuming 252 trading days per year
print(daily_excess_return_SP500) # Display given input value for linear regression model

# Calculating the estimated value of annual expected return of AMD (ERI) with the estimated beta 
estimated_annual_ERI <- annual_RF + estimated_beta * (annual_ERM - annual_RF)
print(estimated_annual_ERI) # Display the estimated value of annual expected return of AMD

# Defining the significance level
alpha <- 0.10 # For a 90% confidence interval
# Defining the number of observations
n <- nrow(df[-1, ]) # Remove 1st row
print(n) # Display number of observations
# Calculating the t value for 90% confidence interval with n-1-1 degrees of freedom
t_value <- qt(1 - alpha/2, df = n - 2)
print(t_value) # Display t value

# Calculating the mean of daily excess return of S&P 500
mean_daily_excess_return_SP500 <- mean(df[-1, ]$excess_return_SP500)
print(mean_daily_excess_return_SP500) # Display mean of daily excess return of S&P 500
# Calculating the daily standard error of estimate with n-1-1 degrees of freedom
daily_se <- sqrt(sum(residuals(df_lm)^2) / (n - 2))
print(daily_se) # Display the daily standard error of estimate
# Calculating the sum of squares of daily excess return of S&P 500
SSX <- sum((df[-1, ]$excess_return_SP500 - mean_daily_excess_return_SP500)^2)
print(SSX) # Display SSX
# Calculating the daily standard error of forecast
daily_sf <- daily_se * sqrt(1+1/n+(daily_excess_return_SP500 - mean_daily_excess_return_SP500)^2 / SSX)
print(daily_sf) # Display the daily standard error of forecast
# Convert daily standard error of forecast to annual standard error of forecast
annual_sf <- daily_sf * sqrt(252)
print(annual_sf) # Display the annual standard error of forecast

# Calculating the prediction interval
lower_bound <- estimated_annual_ERI - t_value * annual_sf
upper_bound <- estimated_annual_ERI + t_value * annual_sf
pi <- c(lower_bound, upper_bound) # Defining prediction interval
print(pi) # Print prediction interval
```

**Answer:** Given that the current risk-free rate is 5.0% p.a, and the annual expected return for S&P 500 (market) is 13.3% p.a, the 90% prediction interval for AMD's annual expected return is [-49.07240%, 85.13438%]. This means that we are 90% confident that AMD's annual expected return is between the interval [-49.07240%, 85.13438%] if the current risk-free rate is 5.0% p.a, and the annual expected return for S&P 500 (market) is 13.3% p.a.
