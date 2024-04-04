library(tswge)
library(vars)
library(nnfor)

# Setting the working directory to the source file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading the Key Economic Indicators data
kei = read.csv("Data/Key_Economic_Indicators_20240401.csv", header = TRUE)

# Checking that data was loaded correctly
head(kei)

# We are only interested 
(kei$Consumer.Price.Index.TX)
na.omit((kei$Consumer.Price.Index.TX))
# We are interested in the modeling the Existing Single Family Home Price in 
# Texas based on the evolution of the Consumer Price Index and Unemployment in 
# Texas from January 2007 (since we have missing data from previous time 
# periods) to February 2024 so we will create a time series object with this 
# information

cpi_ts = ts(data = na.omit(kei$Consumer.Price.Index.TX), start = c(2007,1), frequency = 12)
unemp_ts = ts(data = na.omit(kei$Unemployment.TX), start = c(2007,1), frequency = 12)
home_price_ts = ts(data = na.omit(kei$Existing.Single.Family.Home.Price.TX), start = c(2007,1), end = c(2024,2), frequency = 12)

# Checking that the time series have the correct information
cpi_ts
unemp_ts
home_price_ts

# Plotting the data
plotts.sample.wge(cpi_ts)
plotts.sample.wge(unemp_ts)
plotts.sample.wge(home_price_ts)

df = data.frame(cpi_ts, unemp_ts, home_price_ts)

# TO DO: CREATE TRAINING AND TEST SET


# 4a. Fitting ARMA / ARIMA / ARUMA / Signal Plus Noise (univariate analysis)


# 4b. Fitting VAR model

# TO DO: very basic model, need to refine it
VARselect(df, type = "none")
lsfit=VAR(df,p=2,type="none")
lsfit


# 4c. Fitting Neural Network (mlp) model

# TO DO: very basic model, need to refine it
fit = mlp(home_price_ts, xreg = data.frame(cpi_ts, unemp_ts))
fit


# 4d. Fitting Ensemble model using at least two of the above. 


# 5. Pick a short and long term forecast horizon based on your “problem” from part 3 above and compare all models with the ASE and the rolling window RMSE for both the short and long term forecasts 
# Horizon: maybe 6 months or 1 year for short term and 3 or 5 years for long term?


# 6. Provide the forecasts and prediction limits (when possible) for both the short and long term forecasts. 