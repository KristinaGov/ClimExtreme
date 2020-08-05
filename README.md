# ClimExtreme

# Working files, input data and source code for the paper to be published
## How does climate change affect the transition of power systems: the case of Germany
###### *Alexander Golub, Kristina Govorukha, Philip Mayer, Dirk Rübbelke*

###### *Corresponding author: e-mail: kristina.govorukha@vwl.tu-freiberg.de*

### What you can find in *script_temperatures.r*:

This script reads the data on temperatures in Germany from the source files of the [Deutscher Wetterdienst(DWD)](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/soil_temperature/historical)

In the folder *[./data_temp/stations_hr]* you can find time series of the temperature at 2m high, taken for the selected weather stations in Germany from earlies 1893.

Below you find the figure of temperatures time series since 1893 and smoothed time series showing the trend (using simple moving average).
 

![Alt text](https://github.com/KristinaGov/ClimExtreme/blob/master/Rplots/temperatures.png?raw=true "Temperature trends (with SMA) [t°C]")           | ![Alt text](https://github.com/KristinaGov/ClimExtreme/blob/master/Rplots/temperature_trends.png?raw=true "Temperature [t°C]")
:-------------------------:|:-------------------------:
(1)                        | (2)

Before 1945, only one station in the sample was reporting the measurements (see Figure 1). This explains the difference in the red and beige lines: they show maximum *of all reporting stations* or *average of all reporting stations* (in the sample of 20 selected stations) in Germany for each hour.

Maximum and average of all measurements from the reporting weather stations are growing steadily since the 1950s (see Figure 2). 

### What you can find in *script_out_mw_price.r*:

![Alt text](https://github.com/KristinaGov/ClimExtreme/blob/master/Rplots/raw_outage_plot.png?raw=true "Temperature trends (with SMA) [t°C]")           | ![Alt text](https://github.com/KristinaGov/ClimExtreme/blob/master/Rplots/mounthly_max_hr_outage.png?raw=true "Temperature [t°C]")
:-------------------------:|:-------------------------:
(3)                        | (4)

On Figure 3, you see the plot of time series describing the capacity mix unavailable at every hour. Figure 4 illustrates the maximum capacity mix unavailable for a specific hour among all hours of the respective month. Highest values amount to 8 GW, as can be seen for summers 2018 and 2016. 


### What you can find in *script_api_entsoe_request.r*:
This script connects to ENTSO-e API and downloads the data about outages.

**My credits to github project for useful hints [GitHub: krose/entsoeR ](https://github.com/krose/entsoeR/blob/master/R/outages_get.R.com)**
I refer to this package as it gives a great look inside the API request and helped me to write the part for just downloading the data I needed for this project. I found it cumbersome to use the *entsoeR*, probably due to being newby solving the versions and platform conflicts.
