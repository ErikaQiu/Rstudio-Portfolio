### Forecasting the Export of the Central African Republic
#### Overview
This project explores the exportation trends of the Central African Republic from 1960 to 2017, with a focus on predicting future export rates using time series analysis. Given the country's economic reliance on exports such as timber, diamonds, cotton, and coffee, understanding and forecasting export trends is crucial. By employing an ARIMA model, this study aims to provide insights into the exportation trajectory post-2017, contributing to strategic planning and economic forecasting.
#### Analysis Highlights
Stationarity Check: The initial time series was found to be non-stationary due to a visible downward trend. Differencing was applied to achieve stationarity, a prerequisite for ARIMA modeling. 

Model Selection: Through diagnostics including PACF and ACF plots, AR(2) and MA(3) models were initially considered. AR(2) was ultimately chosen based on p-values, AIC, and BIC criteria.  

Forecasting: The AR(2) model was used to forecast the exportation rate post-2017, indicating a continued decline in the ratio of exports to GDP for the Central African Republic.

#### Getting Started
To replicate this analysis or explore the data further:  
Clone the Repository: Ensure you have R and RStudio installed on your machine.  
Install Required R Packages: Packages such as astsa and forecast are required.  
install.packages(c("astsa", "forecast"))  
Run the Analysis: Navigate to the /Scripts directory and execute the R scripts.
