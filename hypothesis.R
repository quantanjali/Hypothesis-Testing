#*******************************************************************************************************************
# Developer : Kumari Anjali
# Case Study: Hypothesis Testing and Stock Returns
#******************************************************************************************************************

# NOTE : 
# All relevant Standard Packages are mentioned below. If its not installed on your system, please install on prompt by this program
# Platform : This code has been developed on R 3.4.3 GUI 1.70 El Capitan build (7463), Normal R version 3.4.3 for MAC OS El Capitan
# Disclaimer : I have developed and tested these codes on MacBook Pro, MAC OS - El Capitan,where its running fine. 
# This program may not run on older version of R software as well as there may be some error on other operating system platform


# Code to Automatically Install useful Library Function, if required

if (!require(quantmod)) install.packages('quantmod')
if (!require(xts)) install.packages('xts')
if (!require(zoo)) install.packages('zoo')
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols') 
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(graphics)) install.packages('graphics')
if (!require(PerformanceAnalytics)) install.packages('PerformanceAnalytics')
if (!require(readr)) install.packages('readr')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(stringr)) install.packages('stringr')
if (!require(data.table)) install.packages('data.table')
if (!require(Matrix)) install.packages('Matrix')
if (!require(sde)) install.packages('sde')
if (!require(portfolio)) install.packages('portfolio')
if (!require(base)) install.packages('base')
if (!require(stats)) install.packages('stats')
if (!require(reshape2)) install.packages('reshape2')
if (!require(plyr)) install.packages('plyr')

# Loading useful Library Function

library(quantmod)
library(xts)
library(zoo)
library(BatchGetSymbols) 
library(ggplot2)
library(graphics)
library(PerformanceAnalytics)
library(readr)
library(tidyverse)
library(stringr)
library(data.table)
library(Matrix)
library(sde)
library(portfolio)
library(base)
library(stats)
library(reshape2)
library(plyr)

###########################################################################################################################
# Download data for last 1 year for the S&P500 and any 10 of its component stocks. Data has been downloaded from a financial website  Yahoo Finance
###########################################################################################################################

# -----------------------------------------------------------------
# Download data for last one year for the S&P500 
# -----------------------------------------------------------------

#Assign dates to set range for stock quotes
sDate <- as.Date("2017-03-28")
eDate <- as.Date("2018-03-28")

# PLEASE Wait !!! It takes 1 -2 minutes to download all 10 stock data from internet. 
# Warning : If program does not run, please check your internet. It may be slow or no connection ! 
# If unable to download S&P Data, there may be server or internet problem. Please try later when Broadband is faster !!!

SnP_500  <-  getSymbols("^GSPC", auto.assign = FALSE,  src='yahoo', from=sDate, to=eDate)
SnP500_Adj_price <- (SnP_500[ ,6])    # Showing only Date and Adjusted Closing Price of S&P500.
colnames(SnP500_Adj_price) <- "S&P500"

# If no data downloaded, Please try later when Broadband is faster !!!
head (SnP500_Adj_price)

# SnP500_Adj_price # You can use this command to view entire downloaded data


# -------------------------------------------------------------------------------------------------------
# Download data for last one year for S&P500 : 10 separate component stocks
# -------------------------------------------------------------------------------------------------------

# Ticker of all 10 stocks from S&P500 has been assigned in Vector data type
stocks <- c("MMM", "JNJ", "INTC", "HD", "MRK", "UTX", "CSCO", "PFE", "AXP", "WMT")

# Download daily price for 10 stocks using getSymbols() function of Quantmod Package.
# PLEASE Wait !!! It takes 1 -2 minutes to download all 10 stock data from internet. 
# Warning : If program does not run, please check your internet. It may be slow or no connection ! Please try later when Broadband is faster !!!

getSymbols(stocks, auto.assign = TRUE,  src='yahoo', from=sDate, to=eDate)

# R Code to generate Time Series of Price on the basis of Adjusted Close Price
prices <- do.call(merge, lapply(stocks, function(x) Ad(get(x))))      
colnames(prices) <- stocks

# Downloaded  daily data for last one years, which are too big to show, hence showing only first five Adjusted Closing Price for 10 component stocks.
head(prices)        
# prices # Use this to view entire data

###########################################################################################################################
# Calculate daily returns of the S&P500 index and the downloaded stocks over the period under study.
###########################################################################################################################

# Daily Return for S&P500 index on the basis of adjusted closing price
#--------------------------------------------------------------------------------------------

SNP_DailyReturn<- dailyReturn(SnP500_Adj_price, subset=NULL, type="arithmetic", leading=TRUE)
SNP_DailyReturn = SNP_DailyReturn[-1]    # remove first NA observation
colnames(SNP_DailyReturn) <- "S&P500_return"
# SNP_DailyReturn  # Remove comment # in this line to view entire data
head(SNP_DailyReturn)


# Daily Return for 10 S&P500 component stocks on the basis of adjusted closing price
#-----------------------------------------------------------------------------------------------------------------

# Daily return of 10 S&P500 component stocks
# setNames allows us to update the column names without having to write another replacement function on another line, 

Daily_Return <- setNames(do.call(cbind, lapply(prices, dailyReturn)), stocks)
# Code to remove first row where return was 'NA"
Daily_Return = Daily_Return[-1]    # remove first NA observation
# Daily_Return # Remove # to see complete Daily Return for 10 S&P500 component stocks
head(Daily_Return) 

###########################################################################################################################
# For each of the selected stocks and the index, Perform a Student's T test, Calculate the p-value and t-value and test the Null Hypothesis that the mean daily stock return is zero
###########################################################################################################################

#----------------------------------------------------------------------------------------------------------------------------------------
# Perform Student T Test FOR  S&P500 Index and each of selected stock : One Sample t-test for each stock and Index
# Null Hypothesis : true mean  (Mean daily stock return) is equal to 0
# Alternative Hypothesis: true mean is not equal to 0
# Calculate the p-value and t-value and test the Null Hypothesis that the mean daily stock return is zero
#---------------------------------------------------------------------------------------------------------------------------------------

return <- cbind(SNP_DailyReturn, Daily_Return)

# Code to convert "xtx" "zoo" data type to data.frame type for lappy() function
return <- data.frame(return)
head(return)

stock <- c("S&P500","MMM", "JNJ", "INTC", "HD", "MRK", "UTX", "CSCO", "PFE", "AXP", "WMT")

# Student's T Tests for each of the selected stocks and S&P500 index.
# If variance will be unequal, it will become Welch T Test
test = lapply(seq(1,(length(return))),function(x){t.test(return[ ,x], var.equal=TRUE,  mu=0, conf.level=0.95)})
setNames(test, stock)  # Name of T Tests has been changed as name of stock

# Calculate the p-value and t-value
pvalue = sapply(test, function(x){x$p.value})
tvalue = sapply(test, function(x){x$statistic})

# Display T & P Vlaue of S&P500 Index and each stock in Table format
TnP = cbind(pvalue, tvalue)
rownames(TnP) <- stock
TnP



#----------------------------------------------------------------------------------------------------------------------------------------
# Perform Student  T Test FOR  each of selected stock versus S&P500 Index (Two Sample t-test)
# Calculate the p-value and t-value and test the Null Hypothesis that the mean daily stock return is zero
#---------------------------------------------------------------------------------------------------------------------------------------

# Two Sample Student's T Tests for each of the selected stocks versus S&P500 index. Data = Unpaired, Variance = Equal for Students T Test.
# If variance will be unequal, it will become Welch T Test
tests = lapply(seq(1,(length(return)-1)),function(x){t.test(return[ ,x+1],return[ ,1], paired =FALSE, var.equal=TRUE,  mu=0, conf.level=0.95)})
setNames(tests, stocks)  # Name of T Tests has been changed as name of stock

# Calculate the p-value and t-value
pvalues = sapply(tests, function(x){x$p.value})
tvalues = sapply(tests, function(x){x$statistic})

# Display T & P Vlaue of S&P500 Index and each stock in Table format
TnP_Alternate = cbind(pvalues, tvalues)
rownames(TnP_Alternate) <- stocks
TnP_Alternate

#************************************************************************************************************
# The End
#************************************************************************************************************
