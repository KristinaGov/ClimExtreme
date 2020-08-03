# ClimExtreme

# Working files and input data and source code for the paper to be published.

### What you can find in *script_temperatures.r*

This script reads the data on temperatures in Germany from the source files of the [Deutscher Wetterdienst(DWD)](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/soil_temperature/historical)

In the folder *[./data_temp/stations_hr]* you can find timeseries of the temperature measurments at 2 m hogh, taken for the selected weather stations in Germany from earlies 1893.

Below you find the figure of temperatures time series since 1893 and smoothed time series showing the trend (using simple moving average).

![Alt text](https://github.com/KristinaGov/ClimExtreme/blob/master/Rplots/temperatures.png?raw=true "Temperature trends (with SMA) [t°C]")

...Before 1945 ony one station in the sample was reporting the measurments. This explains the difference in the red and beige lines: they show maximum *of all reporting stations* or *average of all reporting stations* in Germany for each hour.

![Alt text](https://github.com/KristinaGov/ClimExtreme/blob/master/Rplots/temperature_trends.png?raw=true "Temperature [t°C]")

...Highest temperatures from all reporting weather stations is growing steadily since 1950s. 

### What you can find in *script_api_entsoe_request.r*
This script connects to ENTSO-e API and downloads the data about outages.

**My credits to github project for useful hints [GitHub: krose/entsoeR ](https://github.com/krose/entsoeR/blob/master/R/outages_get.R.com)**
I refer to this package as it gives a great look inside the API request and helped me to wtire the part for just downloading the data I needed for this project. I found it cumbersome to use the *entsoeR*, probably due to being newby solving the versions and platform conflicts.
