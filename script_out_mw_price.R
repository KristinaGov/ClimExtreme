# This script analyses power plant outages reported to ENTSO-e transparency platform.
# Here you can find how there outages are transformed in the hourly time series,
# denoting the amount of MW withdrawn from the market and graphical representation.
# This script also contains an estimation of the proxy value lost due to outage for 
# coal, gas, lignite and nuclear generation capacities, defined as following:
# [MW-out_per_hour_per_gen_type * hourly_wholesale_electricity_price - variable_costs_per_gen_type]

# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "plyr","raster","ncdf4","ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "pastecs", "ggExtra",
           "CommEcol", "fBasics", "EnvStats", "geosphere", "ggpmisc",
           "lubridate","tidyquant", "gridExtra", "tidyverse", "viridis",
           "ts", "fxregime", "zoo", "fitdistrplus", "cowplot")
for (i in 1:length(packs)) {
  pack <- packs[i]
  if(!is.element(pack, installed.packages()[,1]))
  {install.packages(pack)
  }else {print(paste(packs[i]," already installed"))}
  require(pack,character.only=TRUE)
}
###################################################################

#########             set working directory             ###########
mydir<-getwd()
setwd(mydir)
###################################################################

Sys.setenv(LANGUAGE="en")
Sys.setlocale("LC_TIME", "C")

##################################################################
##################################################################
#                         Read data                              #
##################################################################
##################################################################
# Steps:
# 1. Read big_out_data containing outages+ all outages (per generation and generation unit).
# Start and end of outage are in separate columns - thus we can not see how much capacity was out at the same moment of time.
big.out.df<-read.csv("./data_outage/big_out_data_2019.csv", sep=",")

# 2. Read the file linking PLZ to lon/lat found at the: http://download.geonames.org/export/zip/
# This data was revised and overworked as contained typos in PLZ and city names.
plztolat.df<-as.data.frame(read_excel("./data_plz_util/plztolat.xlsx", 
                                      sheet = "plztolat_upd", col_names = T))
# 3. Read data about EICs. Be careful - PLZ data in this document in not a power plant location, but headquarters location.
# To be ready to use, the location data is taken from the Transparency platform.
eic.info.df<- read.csv("./data_outage/EIC_codes.csv", sep=";")
# Delete columns we will not need in the file:
eic.info.df[,c("UID","Strasse","PLZ","city","Website","EIC_Typ","EIC_Display_Name","International")]<-NULL

# 4. Read the file about the fuel types of the power plants and EIC codes: p-units - production units; g-units: generation units
eic_bcode.df<-as.data.frame(readxl::read_excel("./data_outage/eic_units_type.xlsx", 
                                               sheet = "eic_punits_type", col_names = T))
dim(eic_bcode.df) # 694 12
# 5. Read the table that links fuel type to b-codes (designation of the primary fuel type):
bcode.df<-as.data.frame(readxl::read_excel("./data_outage/eic_units_type.xlsx", 
                                           sheet = "bcode", col_names = T))
# 6. Read the data about hourly electricity prices from the OpenMod database: 
price.df<-as.data.frame(readxl::read_excel("./data_openmod/time_series_60min_singleindex_filtered.xlsx" , 
                                           sheet = "ts", col_names = T))
# Take necessary columns:
price.df<-price.df[,c("mon", "day", "year", "hour", "DE_price_day_ahead")]

# 7. Read data on variable costs of power plants:
# (They are estimated based on the Entso-e TYNDP input data for 2020)
vc.df<-as.data.frame(readxl::read_excel("./data_tyndp/TYNDP2018mmd.xlsx", 
                                               sheet = "vc", col_names = T))
# 8. Read the data on PLZ, BNA and EIC codes of the production units, crosschecked with BNA and Entso-e data:
plz.eic.df<-as.data.frame(readxl::read_excel("./data_plz_util/plz_eic_data.xlsx", 
                                                       sheet = "eic_plz", col_names = T))
##################################################################


##################################################################
#########            Prepare data on prices            ###########
##################################################################
price.df<-tidyr::unite(price.df,"date",c("year","mon","day"), sep="-", 
                       remove=T) %>% unite( "date.time", c("date","hour"), sep=" ", remove=T)
price.df$date.time<-as.POSIXct(price.df$date.time, format="%Y-%m-%d %H") 
price.df<-dplyr::filter(price.df, date.time>="2015-01-01 00:00:00")
price.df<-drop_na(price.df)
##################################################################


##################################################################
#########           Prepare data on outages            ###########
##################################################################
dim(big.out.df) # 9636 22
# Check if there are duplicated entries due to the index of days:
big.out.df.check<-big.out.df[!duplicated(big.out.df),]
dim(big.out.df.check) # 9636 22
length(unique(big.out.df.check$production_RegisteredResource.mRID)) # check: 106 unique EICs
# Merge two files to have fuel types and EIC codes in one data frame:
eic.fuel.df<-inner_join(eic_bcode.df, bcode.df, by=c("bcode"="bcode") )
length(unique(eic.fuel.df$peic)) # Check: 195 unique production units EICs

# The eic_bcode.df has data about last updates that includes additions of 
# capacities to the power plants within the last years. 
# That is why, first extract rows with the latest updates as of 2018-2019:
filter.unq<-unique(eic.fuel.df$peic)
extract.eic.fuel.ls<-list()
for (i in 1:length(filter.unq)) {
  filer1.eic.df<-eic.fuel.df[eic.fuel.df$peic==filter.unq[i],]
  filer2.eic.df<-dplyr::filter(eic.fuel.df, eic.fuel.df$val_from==max(filer1.eic.df$val_from) 
                               & eic.fuel.df$peic==filter.unq[i])
  extract.eic.fuel.ls[[i]]<-filer2.eic.df[!duplicated(filer2.eic.df$peic), ]
}
extract.eic.fuel.df<- dplyr::bind_rows(extract.eic.fuel.ls[1:length(extract.eic.fuel.ls)])
# Remove columns we will not need in the following:
extract.eic.fuel.df[, c ("bcode","name", "zone","val_from", "val_to","voltage",
                         "control_area","bid_zone","upd")]<- NULL

# Merge together two databases about outages and fuel and installed installed capacities:
outages.df<-dplyr::left_join(big.out.df, extract.eic.fuel.df, by=c("production_RegisteredResource.mRID"="peic"))
outages.df<-dplyr::filter(outages.df, fuel!="NA")
# Add time stamps:
outages.df$st.datetime<-as.POSIXct(outages.df$st.datetime, format="%Y-%m-%d %H:%M:%S")
outages.df$end.datetime<-as.POSIXct(outages.df$end.datetime, format="%Y-%m-%d %H:%M:%S")  
# Remove columns we will not need in the following:
outages.df[,c("start_DateAndOrTime.date" , "start_DateAndOrTime.time" ,  
              "end_DateAndOrTime.date" , "end_DateAndOrTime.time",
              "end.mon" , "end.d","reas_t",  "st.mon" , "st.d") ]<-NULL
##################################################################

##################################################################
##################################################################
#                  Expand to HOURLY time series                  #
##################################################################
##################################################################
# Expand start and end dates to turn it to time series - showing the length of outages.
# Desired frequency of the  time series - one hour:
for.ls<-list()
for (row in 1:nrow(outages.df)) {
  #expand start and end times to time series with a step of one hour
  for.time.df<-outages.df %>% do( data.frame(date.time = seq.POSIXt(floor_date(outages.df$st.datetime[row], "hour"), 
                                                                    floor_date(outages.df$end.datetime[row], "hour"), by = 3600 ) ))
  l<-length(seq.POSIXt(floor_date(outages.df$st.datetime[row], "hour"), floor_date(outages.df$end.datetime[row], "hour"), by = 3600 ))
  for.det.df<-outages.df[row, ]
  for.df<-cbind(for.time.df, for.det.df )
  for.ls[[row]]<-for.df
  }
hourly.ts.out<-dplyr::bind_rows(for.ls[1:length(for.ls)])
dim(hourly.ts.out) # 464637     17
# Find the quantity [MW] on outage - in the database "0" means "0" MW from the installed capacity was available. 
# Quantity shows AVAILABLE capacity from installed:
hourly.ts.out$quantity_out<-hourly.ts.out$production_RegisteredResource.pSRType.powerSystemResources.nominalP-hourly.ts.out$quantity
# Remove unavailable capacities below 10-50 MW:
hourly.ts.out<-dplyr::filter(hourly.ts.out, quantity_out>=10)
hourly.ts.out$date.time<-ymd_hms(hourly.ts.out$date.time)
##################################################################

##################################################################
write.csv(hourly.ts.out, file="./data_outage/outages_hr_value.csv")
##################################################################

##################################################################
##################################################################
#                      Plot and glimpse                          #
##################################################################
##################################################################
# Set time sequence of the timeseries. The .xml data contains outages 
# from 2015-01-01 00:00:01 to 2019-12-31 00:00:59.
FROM<-as.POSIXct("2015-01-01 00:00:01", "%Y-%m-%d %H:%M:%S", tz="GMT")
TO  <-as.POSIXct("2019-12-31 00:00:59", "%Y-%m-%d %H:%M:%S", tz="GMT")
# Choose what capacities to take into focus:
capnames<-data.frame(cap=c("gas","coal","lignite","nuc"), names=c("Gas","Coal","Lignite","Nuclear"))
# Set time limits necessary for the data axis of the plot:
lims <- as.POSIXct(strptime(c(FROM, TO),format = "%Y-%m-%d %H:%M:%S", tz="GMT"))
# Filter to take the desired capacity types to the new data frame:
f1<- dplyr::filter(hourly.ts.out, fuel %in% capnames[,"cap"],
                   date.time>=FROM,
                   date.time<=TO)
# Set the datetime format to the colum of dates:
f1$date.time<-as.POSIXct(f1$date.time , format = "%Y-%m-%d %H:%M:%S", tz="GMT")
##################################################################
#                Bar plot of the raw time series                 #
##################################################################
ggplot2::ggplot(f1, 
                aes(fill=factor(fuel, levels=capnames[,"cap"]),
                    y=quantity_out, x=date.time))+
  scale_fill_brewer(palette="Set3", labels=capnames[,"names"])+
  geom_bar(position="stack", stat="identity") +
  scale_x_datetime(labels = scales::date_format("%Y-%m", tz="CET"), date_breaks = "months",
                   limits = lims,
                   timezone = "GMT",
                   expand = c(0,0),
                   name="Date")+ 
  scale_y_continuous(name="Capacity unavailabe per hour [MW]", labels = scales::comma, 
                     breaks = seq(0,8000, by = 1000), 
                     expand = c(0, 0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=18),
        axis.text.y = element_text(angle = 90, hjust = 1, size=18),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.justification="center", 
        legend.position=c(0.95,0.89),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        legend.key.size = unit(1,"line"),
        legend.title.align=0.5)+ 
  labs(fill="Fuel")
ggsave("./Rplots/raw_outage_plot.png", dpi = 600, width = 540, height = 310, units = "mm")
##################################################################

##################################################################
##################################################################
#                 Summarize and Plot again!                      #
##################################################################
##################################################################
# Summarize the outages per hour (the initial resolution is in minutes):
mon.hourly.ts.out<-ddply(hourly.ts.out, .(date.time, fuel), summarise,  sum.h=sum(quantity_out))
# Summarize per hour and fuel:
hourly.sums<-ddply(mon.hourly.ts.out, .(date.time), summarise,  sum.hf=sum(sum.h))
# Bind data frames:
mon.hourly.ts.out<-left_join(mon.hourly.ts.out, hourly.sums, by=c("date.time"="date.time"))
mon.hourly.ts.out$date<-floor_date(as.Date(mon.hourly.ts.out$date.time), "month")
# Gather only highest entries in a month and filter to leave just hours in the month with highest outage value:
mon.hourly.ts.out<- mon.hourly.ts.out %>% group_by(date, fuel) %>% slice(which.max(sum.hf))
##################################################################
#             Bar plot of the summarized time series             #
##################################################################
ggplot2::ggplot(dplyr::filter(mon.hourly.ts.out, fuel %in% capnames[,"cap"]),
                aes(fill=factor(fuel, levels=capnames[,"cap"]), 
                    y=sum.h, x=date))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Set3", labels=capnames[,"names"])+
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "months",
               expand = c(0,0), 
               limits = as.Date(lims),
               name="Date")+ 
  scale_y_continuous(name="Capacity unavailabe per hour [MW]", labels = scales::comma,
                     breaks = seq(0,8000, by = 1000),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=18),
        axis.text.y = element_text(angle = 90, hjust = 1, size=18),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.justification="center", 
        legend.position=c(0.95,0.89),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        legend.key.size = unit(1,"line"),
        legend.title.align=0.5)+ 
  labs(fill="Fuel")
ggsave("./Rplots/mounthly_max_hr_outage.png", dpi = 600, width = 540, height = 310, units = "mm")
##################################################################


##################################################################
##################################################################
#                   Calculate value of outages                   #
##################################################################
##################################################################
# Set time sequence of the timeseries:
FROM<-as.POSIXct("2015-01-01 00:00:01", "%Y-%m-%d %H:%M:%S", tz="GMT")
TO  <-as.POSIXct("2019-12-31 00:00:59", "%Y-%m-%d %H:%M:%S", tz="GMT")

##################################################################
#  Sum the value of outages for all years between 2015 and 2019  #
##################################################################
# Filter the data frame with hourly wholesale market prices in Germany:
Y.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>=FROM, date.time<=TO,
                                 fuel %in% c("nuc","gas","coal","lignite"))
# Add year number to a separate column:
Y.hourly.ts.out$year<-year(Y.hourly.ts.out$date.time)
# Merge data frame with outages[MW] summed up per hour, and hourly wholesale electricity prices:
hourly.ts.out.price<-left_join(Y.hourly.ts.out, price.df, by=c("date.time"="date.time"))
# Merge the achieved data frame again with the estimated proxy electricity generation variable 
# costs for each generation type:
hourly.ts.out.price<-left_join(hourly.ts.out.price, vc.df, by=c("fuel"="fuel", "year"="year"))
# Estimate the proxy value of the MW lost in outage:
hourly.ts.out.price$value<-hourly.ts.out.price$quantity_out*(hourly.ts.out.price$DE_price_day_ahead-hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
hourly.ts.out.price$value[hourly.ts.out.price$value <= 0] <- 0
# Drop lines if we don't have price data for:
hourly.ts.out.price<-hourly.ts.out.price[!is.na(hourly.ts.out.price$DE_price_day_ahead),]
dim(hourly.ts.out.price)   # 364277
sum(hourly.ts.out.price$value) # 534234526

##################################################################
write.csv(hourly.ts.out.price, file="./data_outage/outages_hr_value_price.csv")
##################################################################

##################################################################
#  Sum the value of outages for every year between 2015 and 2019 #
##################################################################
# Estimate for each year: 2015
##################################################################
Y15.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2015-01-01 00:00:00", date.time<="2015-12-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
Y15.hourly.ts.out.price<-left_join(Y15.hourly.ts.out, price.df, by=c("date.time"="date.time"))
Y15.hourly.ts.out.price<-left_join(Y15.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
Y15.hourly.ts.out.price$value<-Y15.hourly.ts.out.price$quantity_out*(Y15.hourly.ts.out.price$DE_price_day_ahead-Y15.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
Y15.hourly.ts.out.price$value[Y15.hourly.ts.out.price$value <= 0] <- 0
Y15.sum<-plyr::ddply(Y15.hourly.ts.out.price, .(fuel), summarise, summmary15=sum(value, na.rm = T)/1000000)

# Estimate for each year: 2016
##################################################################
Y16.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2016-01-01 00:00:00", date.time<="2016-12-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
Y16.hourly.ts.out.price<-left_join(Y16.hourly.ts.out, price.df, by=c("date.time"="date.time"))
Y16.hourly.ts.out.price<-left_join(Y16.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
Y16.hourly.ts.out.price$value<-Y16.hourly.ts.out.price$quantity_out*(Y16.hourly.ts.out.price$DE_price_day_ahead-Y16.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
Y16.hourly.ts.out.price$value[Y16.hourly.ts.out.price$value <= 0] <- 0
Y16.sum<-plyr::ddply(Y16.hourly.ts.out.price, .(fuel), summarise, summmary16=sum(value, na.rm = T)/1000000)

# Estimate for each year: 2017
##################################################################
Y17.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2017-01-01 00:00:00", date.time<="2017-12-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
Y17.hourly.ts.out.price<-left_join(Y17.hourly.ts.out, price.df, by=c("date.time"="date.time"))
Y17.hourly.ts.out.price<-left_join(Y17.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
Y17.hourly.ts.out.price$value<-Y17.hourly.ts.out.price$quantity_out*(Y17.hourly.ts.out.price$DE_price_day_ahead-Y17.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
Y17.hourly.ts.out.price$value[Y17.hourly.ts.out.price$value <= 0] <- 0
Y17.sum<-plyr::ddply(Y17.hourly.ts.out.price, .(fuel), summarise, summmary17=sum(value, na.rm = T)/1000000)

# Estimate for each year: 2018
##################################################################
Y18.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2018-01-01 00:00:00", date.time<="2018-12-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
Y18.hourly.ts.out.price<-left_join(Y18.hourly.ts.out, price.df, by=c("date.time"="date.time"))
Y18.hourly.ts.out.price<-left_join(Y18.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
Y18.hourly.ts.out.price$value<-Y18.hourly.ts.out.price$quantity_out*(Y18.hourly.ts.out.price$DE_price_day_ahead-Y18.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
Y18.hourly.ts.out.price$value[Y18.hourly.ts.out.price$value <= 0] <- 0
Y18.sum<-plyr::ddply(Y18.hourly.ts.out.price, .(fuel), summarise, summmary18=sum(value, na.rm = T)/1000000)

# Estimate for each year: 2019
##################################################################
Y19.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2019-01-01 00:00:00", date.time<="2019-12-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
Y19.hourly.ts.out.price<-left_join(Y19.hourly.ts.out, price.df, by=c("date.time"="date.time"))
Y19.hourly.ts.out.price<-left_join(Y19.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
Y19.hourly.ts.out.price$value<-Y19.hourly.ts.out.price$quantity_out*(Y19.hourly.ts.out.price$DE_price_day_ahead-Y19.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
Y19.hourly.ts.out.price$value[Y19.hourly.ts.out.price$value <= 0] <- 0
Y19.sum<-plyr::ddply(Y19.hourly.ts.out.price, .(fuel), summarise, summmary19=sum(value, na.rm = T)/1000000)

# Estimate the total sum:
##################################################################
total.sum<-bind_cols(Y15.sum,Y16.sum,Y17.sum,Y18.sum,Y19.sum)

##################################################################
#   Sum the value of outages for summers between 2015 and 2019   #
##################################################################
# Filter summer time 2015:
##################################################################
sY15.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2015-06-01 00:00:00", date.time<="2015-08-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
sY15.hourly.ts.out.price<-left_join(sY15.hourly.ts.out, price.df, by=c("date.time"="date.time"))
sY15.hourly.ts.out.price<-left_join(sY15.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
sY15.hourly.ts.out.price$value<-sY15.hourly.ts.out.price$quantity_out*(sY15.hourly.ts.out.price$DE_price_day_ahead-sY15.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
sY15.hourly.ts.out.price$value[sY15.hourly.ts.out.price$value <= 0] <- 0
sY15.sum<-plyr::ddply(sY15.hourly.ts.out.price, .(fuel), summarise, summmary15=sum(value, na.rm = T)/1000000)

# Filter summer time 2016:
##################################################################
sY16.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2016-06-01 00:00:00", date.time<="2016-08-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
sY16.hourly.ts.out.price<-left_join(sY16.hourly.ts.out, price.df, by=c("date.time"="date.time"))
sY16.hourly.ts.out.price<-left_join(sY16.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
sY16.hourly.ts.out.price$value<-sY16.hourly.ts.out.price$quantity_out*(sY16.hourly.ts.out.price$DE_price_day_ahead-sY16.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
sY16.hourly.ts.out.price$value[sY16.hourly.ts.out.price$value <= 0] <- 0
sY16.sum<-plyr::ddply(sY16.hourly.ts.out.price, .(fuel), summarise, summmary16=sum(value, na.rm = T)/1000000)

# Filter summer time 2017:
##################################################################
sY17.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2017-06-01 00:00:00", date.time<="2017-08-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
sY17.hourly.ts.out.price<-left_join(sY17.hourly.ts.out, price.df, by=c("date.time"="date.time"))
sY17.hourly.ts.out.price<-left_join(sY17.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
sY17.hourly.ts.out.price$value<-sY17.hourly.ts.out.price$quantity_out*(sY17.hourly.ts.out.price$DE_price_day_ahead-sY17.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
sY17.hourly.ts.out.price$value[sY17.hourly.ts.out.price$value <= 0] <- 0
sY17.sum<-plyr::ddply(sY17.hourly.ts.out.price, .(fuel), summarise, summmary17=sum(value, na.rm = T)/1000000)

# Filter summer time 2018:
##################################################################
sY18.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2018-06-01 00:00:00", date.time<="2018-08-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
sY18.hourly.ts.out.price<-left_join(sY18.hourly.ts.out, price.df, by=c("date.time"="date.time"))
sY18.hourly.ts.out.price<-left_join(sY18.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
sY18.hourly.ts.out.price$value<-sY18.hourly.ts.out.price$quantity_out*(sY18.hourly.ts.out.price$DE_price_day_ahead-sY18.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
sY18.hourly.ts.out.price$value[sY18.hourly.ts.out.price$value <= 0] <- 0
sY18.sum<-plyr::ddply(sY18.hourly.ts.out.price, .(fuel), summarise, summmary18=sum(value, na.rm = T)/1000000)

# Filter summer time 2019:
##################################################################
sY19.hourly.ts.out<-dplyr::filter(hourly.ts.out, date.time>="2019-06-01 00:00:00", date.time<="2019-08-31 00:00:00",
                                 fuel %in% c("nuc","gas","coal"))
sY19.hourly.ts.out.price<-left_join(sY19.hourly.ts.out, price.df, by=c("date.time"="date.time"))
sY19.hourly.ts.out.price<-left_join(sY19.hourly.ts.out.price,vc.df, by=c("fuel"="fuel", "year"="year"))
sY19.hourly.ts.out.price$value<-sY19.hourly.ts.out.price$quantity_out*(sY19.hourly.ts.out.price$DE_price_day_ahead-sY19.hourly.ts.out.price$vc)
# If price was less than the proxy variable costs - then set to zero:
sY19.hourly.ts.out.price$value[sY19.hourly.ts.out.price$value <= 0] <- 0
sY19.sum<-plyr::ddply(sY19.hourly.ts.out.price, .(fuel), summarise, summmary19=sum(value, na.rm = T)/1000000)

# Estimate the total sum of summers:
##################################################################
total.ssum<-bind_cols(sY15.sum,sY16.sum,sY17.sum,sY18.sum,sY19.sum)
