# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "plyr","tidyr","raster","ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "pastecs", "ggExtra",
           "CommEcol", "fBasics", "EnvStats", "geosphere", "ggpmisc","cowplot",
           "lubridate","tidyquant", "gridExtra", "matrixStats", "viridis",
           "psych","plyr","car","semPLS","semTools","lmtest", "lavaan","semTools")
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

##################################################################
#########                Read files                    ###########
##################################################################
# Steps:
# 1. Read big_out_data containing outages+ all outages (per generation and generation unit).
# Start and end of outage are in separate columns - thus we can not see how much capacity was out at the same moment of time.
big.out.df<-read.csv("./data_outage/big_out_data_2019.csv", sep=",")
big.out.df$X<-NULL

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

# 6. Read the data on PLZ, BNA and EIC codes of the production units, crosschecked with BNA and Entso-e data:
plz.eic.df<-as.data.frame(readxl::read_excel("./data_plz_util/plz_eic_data.xlsx", 
                                             sheet = "eic_plz", col_names = T))

# 7. Create time stamp:
timedate.df<-data.frame(date=seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months"))
timedate.df$period<-seq(1,dim(timedate.df)[1], by=1)
##################################################################


##################################################################
#########             Read HOURLY temperatures         ###########
##################################################################
files.ls<-as.list(list.files(path = "./data_temp/stations_hr/tempfiles/", 
                             all.files = T, 
                             full.names = T,
                             pattern = "produkt_tu_stunde") )
length(files.ls) # 20
# Check that the list items exist:
exist.list1<-list()
for (i in 1:length(files.ls)) {
  exist.list1[[i]]<-file.exists(files.ls[[i]])
}
take<-c("STATIONS_ID","MESS_DATUM","TT_TU")
stations.data.ls<-list()
for (i in 1:length(files.ls)) {
  temp.stations.tb<-read.table(files.ls[[i]], header = TRUE, sep = ";")
  stations.data.ls[[i]]<-temp.stations.tb[take]
}
stations.data.df<-as.data.frame(dplyr::bind_rows(stations.data.ls[1:length(stations.data.ls)]))
# -999 is default NA value in the CDC data. remove it for defaul R designation NA
stations.data.df$TT_TU[stations.data.df$TT_TU == "-999"]<-NA
# Create date stamps
stations.data.df$MESS_DATUM<-strptime(stations.data.df$MESS_DATUM,format = "%Y%m%d%H")
colnames(stations.data.df)<-c("id","date","tmpr")

# Read data about weather stations:
stations.det.df<-as.data.frame(readxl::read_excel("./data_temp/list_stations.xlsx", 
                                                  sheet = "stat", col_names = T))
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
outages.df.t<-dplyr::left_join(big.out.df, extract.eic.fuel.df, by=c("production_RegisteredResource.mRID"="peic"))
outages.df.t<-dplyr::filter(outages.df.t, fuel!="NA")
outages.df<-dplyr::left_join(outages.df.t, plz.eic.df, by=c("production_RegisteredResource.mRID"="production_RegisteredResource.mRID"))

# Add time stamps:
outages.df$st.datetime<-as.POSIXct(outages.df$st.datetime, format="%Y-%m-%d %H:%M:%S")
outages.df$end.datetime<-as.POSIXct(outages.df$end.datetime, format="%Y-%m-%d %H:%M:%S")  
# Remove columns we will not need in the following:
outages.df[,c("start_DateAndOrTime.date" , "start_DateAndOrTime.time" ,  
              "end_DateAndOrTime.date" , "end_DateAndOrTime.time",
              "end.mon" , "end.d","reas_t",  "st.mon" , "st.d",
              "biddingZone_Domain.mRID", "quantity_Measure_Unit.name",
              "production_RegisteredResource.pSRType.powerSystemResources.mRID",
              "production_RegisteredResource.pSRType.powerSystemResources.name"
) ]<-NULL
##################################################################

##################################################################
#              Expand to hourly outages time series              #
##################################################################
# Expand start and end dates to turn it to time series - showing the length of outages.
# And showing how much capacity was out at a time.
# Desired frequency of the  time series - one hour:
for.ls<-list()
for (row in 1:nrow(outages.df)) {
  # expand start and end times to time series with a step of one hour
  for.time.df<-outages.df %>% do( data.frame(date.time = seq.POSIXt(floor_date(outages.df$st.datetime[row], "hour"), 
                                                                    floor_date(outages.df$end.datetime[row], "hour"), by = 3600 ) ))
  l<-length(seq.POSIXt(floor_date(outages.df$st.datetime[row], "hour"), floor_date(outages.df$end.datetime[row], "hour"), by = 3600 ))
  for.det.df<-outages.df[row, ]
  for.df<-cbind(for.time.df, for.det.df )
  for.ls[[row]]<-for.df
}
hourly.ts.out<-dplyr::bind_rows(for.ls[1:length(for.ls)])
dim(hourly.ts.out) # 464637     16
# Find the quantity [MW] on outage - in the database "0" means "0" MW from the installed capacity was available. 
# Quantity shows AVAILABLE capacity from installed:
hourly.ts.out$quantity_out<-hourly.ts.out$production_RegisteredResource.pSRType.powerSystemResources.nominalP-hourly.ts.out$quantity
# Remove unavailable capacities below 10-50 MW:
hourly.ts.out<-dplyr::filter(hourly.ts.out, quantity_out>=10)
hourly.ts.out$date.time<-ymd_hms(hourly.ts.out$date.time)
##################################################################


##################################################################
#########               Plot some outages              ###########
##################################################################
# Quick scatter plot for nuclear power, outage duration (in days):
ggscatter(dplyr::filter(outages.df, fuel=="nuc"), x="st.datetime", y="duration_days")

# Scatter plot of outages per months for the chosen capacity:
# (here we use outages.df that has data only about occurrence,
# therefore the plot shows points - occurrences, and position of
# the point shows the duration estimated in the df by st.date-end.date.
# This plot does not show how much power was out at each month, as some outages
# exceed one month.)
main.plot<- ggplot2::ggplot( dplyr::filter(outages.df,  fuel %in% c("coal"), duration_days<100 ), 
                             aes(month(st.datetime), duration_days, color=fuel)) + 
  geom_point() + 
  ggpubr::color_palette("jco")+
  theme_classic() + 
  labs(title="Duration of outages started in the respective month",
       subtitle = "(outages data from 2015 to 2019)",
       x= paste("Months"), 
       y = "Duration of outages [days]",
       color="Fuel types")+ 
  scale_x_continuous(labels = scales::number_format(accuracy = 2))
main.plot

xdens.plot<-axis_canvas(main.plot, axis="x")+
  geom_density(data=dplyr::filter(outages.df,  fuel %in% c("nuc")),
               aes(x=month(st.datetime), fill=as.factor(fuel)),
               alpha=0.3, size=0.2) +
  ggpubr::fill_palette("jco")
xdens.plot
p.main <- insert_xaxis_grob(main.plot, xdens.plot, grid::unit(.2, "null"), position = "top")
ggdraw(p.main)
#ggsave("./Rplots/scatterplot_outages_months.png")
##################################################################
# Density plot for 2015 - 2019 of outages for the following capacities:
capnames<-data.frame(cap=c("coal","gas","lignite","nuc"), names=c("Coal","Gas","Lignite","Nuclear"))
# density plot of outages
plt<-ggplot(data=dplyr::filter(outages.df , year  %in% c("2015", "2016","2017", "2018","2019")  , 
                               fuel %in% c("coal", "nuc","gas", "lignite")),
            aes(x=month(st.datetime), fill=as.factor(fuel))) +
  facet_grid(. ~ year)+
  #facet_grid(rows = year)+
  geom_density(alpha=0.5) +
  theme_classic() + 
  #ggpubr::color_palette("jco")+
  scale_fill_brewer(palette="Dark2", labels=capnames[,"names"])+
  scale_x_continuous(labels = scales::number_format(accuracy = 2))+
  labs(title="Density plot of outages started in the respective month",
       subtitle = "(outages data from 2015 to 2019)",
       x= paste("Months"), 
       y = "Density",
       fill="Fuel types")
plt
#ggsave("./Rplots/densityplot_outages_months.png")
##################################################################


##################################################################
#########                  TEMPERATURES                ###########
##################################################################
# Filter for the dates for which we have the outages data:
stations.data.df.r<-subset(stations.data.df, date>=as.Date("2015-01-01"))
stations.df<-as.data.frame(unique(stations.data.df.r$date))
colnames(stations.df)<-"date"
# Take unique stations id:
length(unique(stations.data.df$id)) # Check: 20
stations.id<-as.data.frame(unique(stations.data.df$id))
colnames(stations.id)<-"id"
# Join to one file with all data on the samples stations:
about.stations.df<-dplyr::left_join(stations.id, stations.det.df, by=c("id"="id") )
# Combine two databases:
for (i in 1:dim(stations.id)[1]) {
  tomerge.df<-dplyr::select(dplyr::filter(stations.data.df, id==about.stations.df[i,1]),
                            c("date", "tmpr"))
  cname<-paste(about.stations.df[i,1], sep="")
  colnames(tomerge.df)<-c("date", cname)
  stations.df<-left_join(stations.df, tomerge.df, by=c("date"="date"))
}
##################################################################


##################################################################
###       Find closest weather stations to power plants       ####
##################################################################
# Add lon lat to the list of outages power plants:
outaged.pp.df<-unique(outages.df[,c("production_RegisteredResource.mRID","plz","city","in_cap","stat","fuel")])
plztolat.df$plz<-as.character(plztolat.df$plz)
outaged.pp.df<-left_join(outaged.pp.df, plztolat.df, by=c("plz"="plz","city"="city"))

# Find closest weather stations:
stat.loc.df<-about.stations.df[,c("lat","lon","id")]
lonlat.ls<-list()

for (i in 1:dim(outaged.pp.df)[1]) {
  loc.df<-outaged.pp.df[i,]
  loc.mat<-as.matrix(loc.df[,c("lat", "lon")])
  stat.loc.df$loc.lat<-loc.df$lat
  stat.loc.df$loc.lon<-loc.df$lon
  stat.loc.df$euc<-sqrt(apply( (( as.matrix(stat.loc.df[,c("lat", "lon")]) - as.matrix(stat.loc.df[,c("loc.lat", "loc.lon")]) ) ^ 2),1, sum) )
  lonlat.df<-data.frame(eic=loc.df$production_RegisteredResource.mRID,
                        plz=loc.df$plz,
                        city=loc.df$city,
                        region=loc.df$region,
                        in_cap=loc.df$in_cap,
                        stat=loc.df$stat,
                        fuel=loc.df$fuel,
                        lat=stat.loc.df[which.min(stat.loc.df$euc),"lat"],
                        lon=stat.loc.df[which.min(stat.loc.df$euc),"lon"],
                        euc=min(stat.loc.df[,"euc"], na.rm = T),
                        id.st=stat.loc.df[which.min(stat.loc.df$euc),"id"])
  lonlat.ls[[i]]<-lonlat.df
}
#sapply(plz.lonlat.ls[[1]], class)
loc.stat.df<- dplyr::bind_rows(lonlat.ls[1:length(lonlat.ls)])
sum(is.na(loc.stat.df$lon)) # has to be 0
sum(is.na(loc.stat.df$lat)) # has to be 0
##################################################################


##################################################################
#                 ANALYSE TEMP vs OUTAGES 
#                         MONTHLY
##################################################################
# For the test stations.df must have a monthly time resolution
mon.stations.df<-group_by(stations.df, month=floor_date(date,"month"))
agg.m<-colnames(mon.stations.df[,2:(dim(mon.stations.df)[2]-1)])
mon.stations.df<-aggregate(mon.stations.df[,agg.m], list(mon.stations.df$month), mean)
colnames(mon.stations.df)<-c("date", agg.m)
# Filter temperatures data for the period in outages set data:
recent.tempr.df<-mon.stations.df %>% subset(date>="2015-01-01")
# Find max across all stations:
recent.tempr.df$max<-recent.tempr.df[,-1] %>% apply(1, max, na.rm=TRUE)
# Choose columns:
filter.tempr.df<-dplyr::select(recent.tempr.df, c("date","max"))
# Filter the necessary fuels and columns from the .df:
filter.outages.df.0<-dplyr::select(dplyr::filter(hourly.ts.out, 
                                               fuel %in% c("nuc" ,"coal","lignite","gas","ps")),  
                                 c("date.time","production_RegisteredResource.mRID","fuel"))
# Calculate all outages in a month:
filter.outages.df<-plyr::ddply(filter.outages.df.0, .(as.Date(date.time, "%Y-%m", tz="CET"), fuel), summarise, 
                               freq=length(unique(production_RegisteredResource.mRID)))
colnames(filter.outages.df)<-c("date","fuel","freq")
# all outages in a month. Don't account for fuel type:
filter.outages.dff<-plyr::ddply(filter.outages.df.0, .(as.Date(date.time, "%Y-%m", tz="CET")), summarise, 
                                freq=length(unique(production_RegisteredResource.mRID)))
colnames(filter.outages.dff)<-c("date","freq")
# All generation types on one graph:
scatter.data<-left_join(filter.tempr.df, filter.outages.df, by=c("date"="date"))
scatter.data[,c("month(date)","year(date)","production_RegisteredResource.mRID", "in_cap" )]<-NULL
colnames(scatter.data)<-c("date","temperature","fuel","freq")
scatter.data$fuel<-as.factor(scatter.data$fuel)
scatter.data$year<-as.factor(lubridate::year(scatter.data$date))
scatter.data.1f<-dplyr::filter(scatter.data, freq>=0 & temperature>=0)
formula <-y ~ x
ggplot(scatter.data.1f, aes(x=temperature, y=freq, color=fuel))+
  geom_point(alpha = 1, aes(shape=year))+
  geom_smooth(method = "lm",na.rm = T,formula = formula)+
  stat_poly_eq(aes(label = paste(..rr.label..)),
               label.x.npc = "right",
               formula = formula, parse = TRUE, 
               size = 3)+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = formula),
                  #geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value..), sep = "")),
                  size = 3)

# Without considering fuel types:
scatter.data<-left_join(filter.tempr.df, filter.outages.dff, by=c("date"="date"))
scatter.data[,c("month(date)","year(date)","production_RegisteredResource.mRID", "in_cap" )]<-NULL
colnames(scatter.data)<-c("date","temperature","freq")
scatter.data$year<-as.factor(lubridate::year(scatter.data$date))

scatter.data.1f<-dplyr::filter(scatter.data, freq>=0 & temperature>=0)
ggplot(scatter.data.1f, aes(x=temperature, y=freq))+
  geom_point(alpha = 1, aes(shape=year))+
  geom_smooth(method = "lm",na.rm = T,formula = formula)+
  stat_poly_eq(aes(label = paste(..rr.label..)),
               label.x.npc = "right",
               formula = formula, parse = TRUE, 
               size = 3)+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = formula),
                  #geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value..), sep = "")),
                  size = 3) +
  labs(x="Temperature [°C]", y= "Frequency of outages", shape="Year")
#ggsave("./Rplots/Scatterplot_lm_out_tmpr_mon.png")
# The p-value for each independent variable tests the null hypothesis that the variable 
# has no correlation with the dependent variable.
reg<-lm(freq ~ temperature, data=scatter.data)
summary(reg)
# Residuals:
# Min      1Q       Median      3Q      Max 
# -5.8862 -1.4612   -0.2065     1.8486  5.9151 
# When assessing how well the model fit the data, you should look for a symmetrical distribution 
# across these points on the mean value zero (0). We can see that the distribution 
# of the residuals appear to be symmetrical. 
# The t-statistic values are relatively far away from zero and are large relative to the standard 
# error, which could indicate a relationship exists.
# (<2e-16 ***, 0.0369 *) p-value < 0.05 for the intercept and the slope indicates that we can reject 
# the null hypothesis, which allows us to conclude that there is a relationship between freq and temperatures.
# Residual Standard Error is measure of the quality of a linear regression fit: 2.437
# The percentage error is (Res.st.error/Intercept) = (2.437/9.46349)*100 =25.75%
# The R-squared (R2) statistic provides a measure of how well the model is fitting the actual data.
# It always lies between 0 and 1: i.e.: a number near 0 represents a regression that does not explain the 
# variance in the response variable well and a number close to 1 does explain the observed variance in the response variable.
# R2 = 0.09125 (9%)
# F-statistic is a good indicator of whether there is a relationship between our predictor and the response variables. 
# The further the F-statistic is from 1 the better it is -> 4.619
# The p-value: 0.03691 < 0.05 *

# For normal distribution of the residuals: the p-value > 0.05 implies that the distribution of the data are not 
# significantly different from normal distribution. 
shapiro.test(reg$residuals)
# 0.9823 > 0.05 -> We can assume the normality.

##################################################################
#                 ANALYSE TEMP vs OUTAGES 
#                         HOURLY
##################################################################
# Merge hourly ts of temperatures and outages together:
# hourly.ts.out and stations.df
filter.tempr.hr.df<-stations.df %>% subset(date>="2015-01-01")
# Take maximum/min/mean temperatures from all the stations reports:
filter.tempr.hr.df$max<-filter.tempr.hr.df[,-1] %>% apply(1, mean, na.rm=TRUE)
# Take necessary columns:
filter.tempr.hr.df<-filter.tempr.hr.df[,c("date","max")]

filter.outages.hr.df0<-dplyr::select(dplyr::filter(hourly.ts.out, 
                                                    fuel %in% c("nuc","coal","lignite","gas","ps")),  
                                      c("date.time","production_RegisteredResource.mRID","fuel"))
# Calculate all outages in a month:
filter.outages.hr.df<-plyr::ddply(filter.outages.hr.df0, .(floor_date(filter.outages.hr.df0$date.time, unit="hour")), summarise, 
                               freq=length(unique(production_RegisteredResource.mRID)))
colnames(filter.outages.hr.df)<-c("date","freq")

scatter.data.4<-left_join(filter.tempr.hr.df,  filter.outages.hr.df, by=c("date"="date"))
str(scatter.data.4)

scatter.data.4.1<-scatter.data.4
scatter.data.4.1$logmax<-log(scatter.data.4.1$max)
scatter.data.4.1$logfreq<-log(scatter.data.4.1$freq)

scatter.data.4.1$lagmax<-dplyr::lag(scatter.data.4.1$max, 1)
scatter.data.4.1$lagfreq<-dplyr::lag(scatter.data.4.1$freq, 1)

scatter.data.4.1$loglagmax<-log(scatter.data.4.1$max/scatter.data.4.1$lagmax)
scatter.data.4.1$loglagfreq<-log(scatter.data.4.1$freq/scatter.data.4.1$lagfreq)

#scatter.data.4.1<-drop_na(scatter.data.4.1)
#ggplot(scatter.data.4.1, aes(date, loglagmax)) + geom_line()
# There are three outlines
scatter.data.4.1<-subset(scatter.data.4.1, loglagmax>=-35)

p1<-ggplot(scatter.data.4.1, aes(date, loglagmax)) + geom_line() +
    xlab("Year") + ylab(expression(paste("Ln(", T[n], "/", T[n-1],")")))
p2<-ggplot(scatter.data.4.1, aes(date, loglagfreq)) + geom_line() +
    xlab("Year") + ylab(expression(paste("Ln(", Out[n], "/", Out[n-1],")")))
ggarrange(p1, p2,labels = c("(a)", "(b)"), ncol = 1, nrow = 2)
#gsave("./Rplots/tseries_out_tmpr_ln.png")

reg.2<-lm(loglagfreq ~ loglagmax, data=scatter.data.4.1)
summary(reg.2)
# Residuals:
# Min      1Q        Median   3Q       Max 
# -0.69339 -0.00030  0.00004  0.00029  0.53932 
# When assessing how well the model fit the data, you should look for a symmetrical distribution 
# across these points on the mean value zero (0). We can see that the distribution 
# of the residuals appears to be symmetrical. 

# The t-statistic values are relatively far away from zero and are large relative to the standard 
# error, which could indicate a relationship exists.
# (0.99231, 0.00523 **) p-value < 0.05 for the slope indicates that we can reject 
# the null hypothesis, which allows us to conclude that there is a relationship between freq and temperatures.

# Residual Standard Error is measure of the quality of a linear regression fit: 0.07053
# The percentage error is mach larger than the intercept. 

# The R-squared (R2) statistic provides a measure of how well the model is fitting the actual data.
# It always lies between 0 and 1: i.e.: a number near 0 represents a regression that does not explain the 
# variance in the response variable well and a number close to 1 does explain the observed variance in the response variable.
# R2 = 0.0001958 (0.01%)

# F-statistic is a good indicator of whether there is a relationship between our predictor and the response variables. 
# The further the F-statistic is from 1 the better it is -> 4.619

# The p-value: 0.005226 < 0.05 **




