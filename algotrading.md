---
title: "ACTL1101 Assignment Part A"
author: "Danny Zhou"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customise Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyse and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyse the total updated P/L and ROI. 

6. **Discussion:** Summarise your findings.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


### Plotting the Data

Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l', 
     xlab = 'Date', 
     ylab = 'Closing Price', 
     main = 'Closing Prices over Time')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialise necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialising columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

# Initialising variables for trading logic
previous_price <- 0 
share_size <- 100 
accumulated_shares <- 0

# Loop repeating this block for all rows in the AMD data frame
for (i in 1:nrow(amd_df)) {  
  
  # Case 1 for buying: if it is the first day AND previous price is 0
  if (i == 1 && previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    # Cost of buying 100 shares at current price (the negative reflects cash outflows)
    amd_df$costs_proceeds[i] <- -1 * amd_df$close[i] * share_size 
    # Reassigning variable, summing the bought shares with previously accumulated shares
    accumulated_shares <- accumulated_shares + share_size 
  }
  
  # Case 2 for buying: if it is NOT the first day AND current price < previous price
  if (i != 1 && amd_df$close[i] < amd_df$close[i-1]) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -1 * amd_df$close[i] * share_size 
    accumulated_shares <- accumulated_shares + share_size 
    # Now, since i != 1, we can reassign this variable to a non-zero row i-1
    previous_price <- amd_df$close[i-1]
  }

  # Case for NOT buying: if it is NOT the first day AND current price >= previous price
  if (i != 1 && amd_df$close[i] >= amd_df$close[i-1]) {
    amd_df$trade_type[i] <- ''
    # No cost involved with buying 0 shares.
    amd_df$costs_proceeds[i] <- 0
    # No shares were bought, so this variable retains the previous accumulated shares
    accumulated_shares <- accumulated_shares
    previous_price <- amd_df$close[i-1]
  }
  
  # Case for selling: if it is the last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    # (Positive) cash inflow of selling all accumulated shares at current price
    amd_df$costs_proceeds[i] <- +1 * amd_df$close[i] * amd_df$accumulated_shares[i-1]
    # After selling all accumulated shares, we no longer possess any shares
    accumulated_shares <- 0
    previous_price <- amd_df$close[i-1]
  }
  
  # Stores the accumulated shares in the AMD data frame
  amd_df$accumulated_shares[i] <- accumulated_shares
}
```


## Step 3: Customise Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Load the dplyr package without start-up messages (for PDF formatting)
suppressPackageStartupMessages(library('dplyr'))

# Assigning a subset of amd_df as the custom trading period from 2020-01-03 to 2023-01-03
custom_df <- amd_df %>%
  filter(date >= as.Date("2020-01-03") & date <= as.Date("2023-01-03")) %>%
  select(date, close) # We select only date and close so we can easily use Step 2 code

# The same version of the Step 2 trading algorithm for amd_df is used below for custom_df

# Initialising columns for trade type, cost/proceeds, and accumulated shares in custom_df
custom_df$trade_type <- NA
custom_df$costs_proceeds <- NA
custom_df$accumulated_shares <- 0

# Initialising variables for trading logic
previous_price <- 0 
share_size <- 100
accumulated_shares <- 0

# Loop repeating this block for all rows in the custom AMD data frame
for (i in 1:nrow(custom_df)) {  
  
  # Case 1 for buying: if it is the first day AND previous price is 0
  if (i == 1 && previous_price == 0) {
    custom_df$trade_type[i] <- 'buy'
    # Cost of buying 100 shares at current price (the negative reflects cash outflows)
    custom_df$costs_proceeds[i] <- -1 * custom_df$close[i] * share_size 
    # Reassigning variable, summing the bought shares with previously accumulated shares
    accumulated_shares <- accumulated_shares + share_size 
  }
  
  # Case 2 for buying: if it is NOT the first day AND current price < previous price
  if (i != 1 && custom_df$close[i] < custom_df$close[i-1]) {
    custom_df$trade_type[i] <- 'buy'
    custom_df$costs_proceeds[i] <- -1 * custom_df$close[i] * share_size 
    accumulated_shares <- accumulated_shares + share_size 
    # Now, since i != 1, we can reassign this variable to a non-zero row i-1
    previous_price <- custom_df$close[i-1]
  }

  # Case for NOT buying: if it is NOT the first day AND current price >= previous price
  if (i != 1 && custom_df$close[i] >= custom_df$close[i-1]) {
    custom_df$trade_type[i] <- ''
    # No cost involved with buying 0 shares.
    custom_df$costs_proceeds[i] <- 0
    # No shares were bought, so this variable retains the previous accumulated shares
    accumulated_shares <- accumulated_shares
    previous_price <- custom_df$close[i-1]
  }
  
  # Case for selling: if it is the last day
  if (i == nrow(custom_df)) {
    custom_df$trade_type[i] <- 'sell'
    # (Positive) cash inflow of selling all accumulated shares at current price
    custom_df$costs_proceeds[i] <- +1 * custom_df$close[i] * custom_df$accumulated_shares[i-1]
    # After selling all accumulated shares, we no longer possess any shares
    accumulated_shares <- 0
    previous_price <- custom_df$close[i-1]
  }
  
  # Stores the accumulated shares in the custom AMD data frame
  custom_df$accumulated_shares[i] <- accumulated_shares
}
```


## Step 4: Run Your Algorithm and Analyse Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# By a confirmation through Excel, trades were executed as expected in Step 3
# Note that the calculations below for PnL, IC, and ROI are for custom_df

# Total profit/loss calculation: sum of all entries in costs_proceeds column in custom_df
PnL <- sum(custom_df$costs_proceeds)
print(PnL) # Display the PnL result as a dollar amount

# Total invested capital calculation: 'PnL - sell (last) transaction' gives negative IC
IC <- -1 * (PnL - custom_df$costs_proceeds[nrow(custom_df)]) # Multiply -1 for IC
print(IC) # Display the IC result as a dollar amount

# Return on investment calculation using given formula
ROI <- ( (PnL)/(IC) ) * 100
print(ROI) # Display the ROI result as a percentage
```

### Plotting the Data for 2020-01-03 to 2023-01-03

Similar to before, we now plot the closing prices over time to visualize the price movement from 2020-01-03 to 2023-01-03 for a closer examination.
```{r custom plot}
plot(custom_df$date, custom_df$close,'l', 
     xlab = 'Date', 
     ylab = 'Closing Price', 
     main = 'Closing Prices over Time')
```

### Analysis of Financial Metrics

There is a loss of $821,604.10; invested capital of $3,286,374; and return on investment of approximately -25%. By an eyeball analysis of the above plot, recognise that the significant majority of closing prices leading up to the last day at 2023-01-03 are higher than the closing price at the last day. For the default strategy, this means that we are buying shares at higher prices than what we are going to sell at. Obviously, we would then expect a loss and hence a negative return on investment. This aligns with the calculated results.

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanism (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
# The profit-taking strategy (option 1) with 20% shall be additionally implemented

# Initialising new columns associated with the profit-taking strategy in custom_df
# Since this following code names are quite long, we will use the following abbreviations:
  # 'p_' prefix for new columns associated with the profit-taking strategy
  # tt = trade_type
  # cp = costs_proceeds
  # acc = accumulated
  # pp = purchasing_price
  # nop = number_of_purchases
  # avg = average_of
custom_df$p_tt <- ''
custom_df$p_cp <- 0
custom_df$p_acc_shares <- 0
custom_df$p_acc_pp <- 0 # acc_pp is accumulated purchasing price
custom_df$p_acc_nop <- 0 # acc_nop is accumulated number of purchases 
custom_df$p_avg_running_pp <- 0 # avg_running_pp is acc_pp divided by acc_nop 

# Initialising variables for trading logic
p_previous_price <- 0 
share_size <- 100
p_acc_shares <- 0 
p_acc_nop <- 0
p_acc_pp <- 0

# Loop repeating this block for all rows in custom_df
for (i in 1:nrow(custom_df)) {
  
  # Case 1 for buying: if it is the first day AND previous price is 0
  if (i == 1 && p_previous_price == 0) {
    custom_df$p_tt[i] <- 'buy'
    # Cost of buying 100 shares at current price (the negative reflects cash outflows)
    custom_df$p_cp[i] <- -1 * custom_df$close[i] * share_size 
    # Reassigning variable, summing the bought shares with previously acc shares
    p_acc_shares <- p_acc_shares + share_size 
    # Reassigning variable, adding 1 to previously acc_nop, since 1 'buy' transaction occurred
    p_acc_nop <- p_acc_nop + 1
    # Since 1 'buy' transaction occurred, the pp is simply just the closing (current) price
    p_pp <- custom_df$close[i]
    # For i == 1, the acc_pp is just pp
    p_acc_pp <- p_pp
  } 
  
  # Case 1 for selling: if it is NOT the first day AND current price is >= 120% of p_avg_running_pp
  else if (i != 1 && custom_df$close[i] >= 1.2 * custom_df$p_avg_running_pp[i-1]) {
    custom_df$p_tt[i] <- 'sell'
    # (Positive) cash inflow of selling half of all holdings at current price
    custom_df$p_cp[i] <- +1 * custom_df$close[i] * (1/2) * custom_df$p_acc_shares[i-1]
    # After selling half of all holdings, half of all holdings remain
    p_acc_shares <- (1/2) * custom_df$p_acc_shares[i-1]
    p_previous_price <- custom_df$close[i-1]
    # Since no 'buy' transaction occurred, the acc_nop does not change
    p_acc_nop <- p_acc_nop
    # Since no 'buy' transaction occurred, the pp is 0
    p_pp <- 0
    # Since no 'buy' transaction occurred, the acc_pp remains unchanged
    p_acc_pp <- custom_df$p_acc_pp[i-1]
  }
  
  # Case 2 for buying: if it is NOT the first day AND current price < previous price AND
  # current price is < 120% of p_avg_running_pp
  else if (i != 1 && custom_df$close[i] < custom_df$close[i-1]) {
    custom_df$p_tt[i] <- 'buy'
    # Cost of buying 100 shares at current price (the negative reflects cash outflows)
    custom_df$p_cp[i] <- -1 * custom_df$close[i] * share_size 
    # Reassigning variable, summing the bought shares with previously acc shares
    p_acc_shares <- p_acc_shares + share_size
    p_previous_price <- custom_df$close[i-1]
    # Reassigning variable, adding 1 to previously acc_nop, since 1 'buy' transaction occurred
    p_acc_nop <- p_acc_nop + 1
    # Since 1 'buy' transaction occurred, the pp is simply just the closing (current) price
    p_pp <- custom_df$close[i]
    # For i != 1, the acc_pp is the sum of pp and previously acc_pp
    p_acc_pp <- p_pp + custom_df$p_acc_pp[i-1]
  } 
  
  # Case 2 for selling: if it is the last day
  if (i == nrow(custom_df)) {
    custom_df$p_tt[i] <- 'sell'
    # (Positive) cash inflow of selling all acc shares at current price
    custom_df$p_cp[i] <- +1 * custom_df$close[i] * custom_df$p_acc_shares[i-1]
    # After selling all acc shares, we no longer possess any shares
    p_acc_shares <- 0
    p_previous_price <- custom_df$close[i-1]
    # Since no 'buy' transaction occurred, the acc_nop does not change
    p_acc_nop <- p_acc_nop
    # Since no 'buy' transaction occurred, the pp is 0
    p_pp <- 0
    # Since no 'buy' transaction occurred, the acc_pp remains unchanged
    p_acc_pp <- custom_df$p_acc_pp[i-1] 
  }
  
  # Stores p_acc_shares; p_pp; p_acc_nop; and p_acc_pp in custom_df
  custom_df$p_acc_shares[i] <- p_acc_shares
  custom_df$p_acc_nop[i] <- p_acc_nop
  custom_df$p_acc_pp[i] <- p_acc_pp 
  # Stores p_avg_running_pp column, given by p_acc_pp divided by p_acc_nop, in custom_df
  custom_df$p_avg_running_pp[i] <- p_acc_pp/p_acc_nop
}
```

## Step 6: Summarise Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
# Again, we will use the 'p_' prefix for PnL, IC, and ROI using the profit-taking strategy

# Total profit/loss calculation: sum of all entries in p_cp column in custom_df
p_PnL <- sum(custom_df$p_cp)
print(p_PnL) # Display the p_PnL result as a dollar amount

# Total invested capital calculation: negative sum of all 'buy' transactions in p_cp (all p_cp < 0)
p_IC <- (sum(custom_df %>% # Note that the 'dplyr' package has already been loaded in Step 3
              filter(p_cp < 0) %>%
              select(p_cp))
  ) * -1 # Multiply -1 for positive IC
print(p_IC) # Display the p_IC result as a dollar amount

# Return on investment calculation using given formula
p_ROI <- ( (p_PnL)/(p_IC) ) * 100
print(p_ROI) # Display the p_ROI result as a percentage

# Total profit/loss comparison between two strategies
change_in_PnL <- p_PnL - PnL
print(change_in_PnL) # Display change_in_PnL as a dollar amount

# Return on investment comparison between two strategies
change_in_ROI <- p_ROI - ROI
print(change_in_ROI) # Display change_in_ROI as a percentage
```

### Plotting the Data for 2020-01-03 to 2023-01-03 with Trend Lines

Below is the plot of closing prices over time from 2020-01-03 to 2023-01-03 with two trend lines. The green increasing trend line is a linear model from the first day to the highest peak (this trend line is suppose to stop at 2021-11-29). The red decreasing trend line is a linear model from 2021-11-29 to the end of the trading period (this trend line is suppose to start at 2021-11-29).

```{r step 6 plot}
# Plotting closing prices over time from 2020-01-03 to 2023-01-03
plot(custom_df$date, custom_df$close,'l', 
     xlab = 'Date', 
     ylab = 'Closing Price', 
     main = 'Closing Prices over Time')
# Assigning increasing trend line to linear model from first day to highest peak
itl <- lm(custom_df$close[1:481] ~ custom_df$date[1:481]) # row 481 has highest peak
abline(itl, col = 'green', lwd = 2) # abline adds itl to existing plot with line width 2
# Assigning decreasing trend line to linear model from highest peak to last day
dtl <- lm(custom_df$close[481:nrow(custom_df)] ~ custom_df$date[481:nrow(custom_df)])
abline(dtl, col = 'red', lwd = 2)
```

### Analysis of Financial Metrics (Changes in Profit/Loss and Return on Investment)

The 'change_in_PnL' shows a $905,646.60 increase in profit to $84,042.47 by using the profit-taking strategy instead of the default strategy. Moreover, the 'change_in_ROI' shows an approximately 45.59% increase in return on investment to about 20.59% by using the profit-taking strategy instead of the default strategy. It is clear that implementing the profit-taking strategy was more effective in generating earnings compared to the default strategy.

By an eyeball analysis of the above plot, recognise that the significant majority of closing prices leading up to the last day at 2023-01-03 are higher than the closing price at the last day. For the default strategy, this means that we are buying shares at higher prices than what we are going to sell at. Obviously, we would then expect a loss and hence a negative return on investment. For the profit-taking strategy, this means that we are at least not selling all shares at a relatively lower closing price on the last day, but instead selling half of our current holdings at certain peaks leading up to the last day - which is at a generally higher closing price compared to the closing price on the last day. Hence, we should expect an improvement in profit/loss and return on investment when using the profit-taking strategy instead of the default strategy. This aligns with the calculated results detailed in the previous paragraph.

### Market Events that affected trends in Closing Prices over Time from 2020-01-03 to 2023-01-03 

AMD's share price saw an increasing trend from 2020-01-03 to 2021-11-29, reaching a peak of $161.91. After this turning point, the AMD share price followed a decreasing trend to the end of the custom trading period 2023-01-03. The overall direction of trends can be informally described as an upside down 'V' shape.

```{r step 6 image}
# Inserting image of Australian interest rates from 2020-01-03 to 2023-01-03 from tradingeconomics.com
library('png') # Load 'png' package
AUIR <- readPNG("Interest Rates.png") # Reads image file
grid::grid.raster(AUIR) # Displays image
```

Australia used conventional monetary policy, dropping interest rates 3 times to 0.1% in response to COVID-19 from the start of 2020 to the end of 2020. Global interest rates also decreased as most countries employed an expansionary monetary policy stance, decreasing interest rates. Australian interest rates remained at 0.1% until May of 2022. This can make borrowing cheaper and may stimulate investment in the AMD stock market, leading to higher share prices. This aligns with AMD's steady increase in share prices until 2021-11-29, reaching a peak of $161.91. However, Australia's overstimulation in macroeconomic policies in response to COVID-19 resulted in high levels of inflation - which was addressed by contractionary monetary policy, with rapid increases in interest rates starting at May of 2022. This can make borrowing expensive and may disincentivise investment in the AMD stock market, leading to lower share prices. Again, this aligns with AMD's decreasing trend in share prices from the peak in 2021-11-29 until the end of the trading period at 2023-01-03.
