# CountryETFForecaster
Function for making naïve forecasts for country ETFs using Shiller P/E.

The return forecast function is -0.075 * log(CAPE) + 0.2775.

Left side tells the CAPE levels, right side tells the expected returns and the red circle indicates the CAPE measurement date.

The closer the forecast is to the current date, the more accurate it is as CAPE is calculated by using a rolling 10-year period.

![China](https://github.com/KaroRonty/CountryETFForecaster/blob/master/china.PNG?raw=true)
