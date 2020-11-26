# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "plyr","tidyr","raster","ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "pastecs", "ggExtra",
           "CommEcol", "fBasics", "EnvStats", "geosphere", "ggpmisc","cowplot",
           "lubridate","tidyquant", "gridExtra", "matrixStats", "viridis")
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
#########                read files                    ###########
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
#              Expand to HOURLY outages time series              #
##################################################################
# Expand start and end dates to turn it to time series - showing the length of outages.
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
dim(hourly.ts.out) # 464637     17
# Find the quantity [MW] on outage - in the database "0" means "0" MW from the installed capacity was available. 
# Quantity shows AVAILABLE capacity from installed:
hourly.ts.out$quantity_out<-hourly.ts.out$production_RegisteredResource.pSRType.powerSystemResources.nominalP-hourly.ts.out$quantity
# Remove unavailable capacities below 10-50 MW:
hourly.ts.out<-dplyr::filter(hourly.ts.out, quantity_out>=10)
hourly.ts.out$date.time<-ymd_hms(hourly.ts.out$date.time)
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
stations.data.df$MESS_DATUM<-strptime(stations.data.df$MESS_DATUM,format = "%Y%m%d")
stations.data.df$MESS_DATUM<-as.POSIXct(stations.data.df$MESS_DATUM,
                                        origin=as.character(stations.data.df$MESS_DATUM[1]), 
                                        format = "%Y-%m-%d")
colnames(stations.data.df)<-c("id","date","tmpr")
class(stations.data.df$date)
stations.data.df$date<-as.Date(stations.data.df$date, tz="CET")
# Read data about weather stations:
stations.det.df<-as.data.frame(readxl::read_excel("./data_temp/list_stations.xlsx", 
                                                  sheet = "stat", col_names = T))
##################################################################

##################################################################
#########               Plot some outages              ###########
##################################################################
ggscatter(dplyr::filter(outages.df, fuel=="nuc"), x="st.datetime", y="duration_days")

# scatter plot of outages per months for the chosen capacity:
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
##################################################################


##################################################################
#########                  TEMPERATURES                ###########
##################################################################
# Filter for the dates for which we have the outages data:
stations.data.df<-subset(stations.data.df )
stations.df<-as.data.frame(unique(stations.data.df$date))
colnames(stations.df)<-"date"
# Take unique stations id:
length(unique(stations.data.df$id)) #20
stations.id<-as.data.frame(unique(stations.data.df$id))
colnames(stations.id)<-"id"
# Join to one file with all data on the samples stations:
about.stations.df<-dplyr::left_join(stations.id, stations.det.df, by=c("id"="id") )

for (i in 1:dim(stations.id)[1]) {
  tomerge.df<-dplyr::select(dplyr::filter(stations.data.df, id==about.stations.df[i,1]),
                            c("date", "tmpr"))
  cname<-paste(about.stations.df[i,1], sep="")
  colnames(tomerge.df)<-c("date", cname)
  stations.df<-left_join(stations.df, tomerge.df, by=c("date"="date"))
}
# For the johansen test stations.df must have a monthly time resolution
mon.stations.df<-group_by(stations.df, month=floor_date(date,"month"))
agg<-colnames(mon.stations.df[,2:(dim(mon.stations.df)[2]-1)])
mon.stations.df<-aggregate(mon.stations.df[,agg], list(mon.stations.df$month), mean)
colnames(mon.stations.df)<-c("date", agg)
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

# Filter temperatures data for the period in outages set data:
recent.tempr.df<-mon.stations.df %>% subset(date>="2015-01-01")
recent.tempr.df$max<-recent.tempr.df[,-1] %>% apply(1, max, na.rm=TRUE)

filter.tempr.df<-dplyr::select(recent.tempr.df, c("date","max"))

filter.outages.df<-dplyr::select(dplyr::filter(extract.out.1, 
                                               fuel %in% c("nuc" 
                                                           ,"coal"
                                                           ,"lignite"
                                                           ,"gas"
                                                           ,"ps"
                                               )),  
                                 c("date","freq","fuel"))

# Calculate all outages in a month:
filter.outages.df$date<-as.Date(filter.outages.df$date)
scatter.data<-left_join(filter.tempr.df,
                        filter.outages.df,
                        by=c("date"="date"))
scatter.data[,c("month(date)","year(date)","production_RegisteredResource.mRID", "in_cap" )]<-NULL
colnames(scatter.data)<-c("date","temperature","freq","fuel")
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

ggscatter(scatter.data.1f, x ="temperature", y = "freq",
          add = "reg.line",  # Add regressin line
          conf.int = TRUE,
          #size = 3, alpha = 0.6,
          ylab="Number of outages per month",
          rug = F ,
          ellipse = F,
          xlab="Monthly mean temperature",
          xlim=c(10,28)) +
  stat_cor(method = "pearson")


ggplot(scatter.data.1f)+
  geom_bar(aes(x=as.factor(temperature), y=freq , fill=fuel), stat = "identity", position = "stack")



##################################################################
#                 ANALYSE TEMP vs OUTAGES 
#                         daily
##################################################################

take2<-c("st.datetime","end.datetime",
         "production_RegisteredResource.mRID",
         "freq", "ratio")
big.out.extr.df<-out.ts.df[,take2]
big.out.extr.df$out.start<-as.Date(big.out.extr.df$st.datetime, "%Y-%m-%d")
dim(big.out.extr.df) #9401

# add fuel types to the eics in outages
fuel.big.out.df<-dplyr::left_join(big.out.extr.df,
                                  unique(eic.fuel.df[,c("peic","fuel")]),
                                  by=c("production_RegisteredResource.mRID"="peic"))
dim(fuel.big.out.df) #9401

# calculate all outages per day (we have now also per hours  / minutes)
fuel.big.out.df<-subset(fuel.big.out.df, fuel %in% c("coal","lignite","gas","nuc"))
dim(fuel.big.out.df) # 8835
# sum per datetime for each fuel type
filter.outages.daily.df<- plyr::ddply(fuel.big.out.df, .(out.start, fuel), summarise, sum.freq=sum(freq, na.rm = T))
# or do not sum for density graph
filter.outages.daily.dens.df<-fuel.big.out.df[,c("out.start","fuel","freq")]

recent.tempr.daily<-stations.df %>% subset(date>="2015-01-01")
# take maximum/min/mean temperatuire from all the stations reports
recent.tempr.daily$max<-recent.tempr.daily[,-1] %>% apply(1, mean, na.rm=TRUE)

filter.tempr.daily<-recent.tempr.daily[,c("date","max")]

scatter.data.2<-left_join(filter.tempr.daily,
                          filter.outages.daily.df, 
                          by=c("date"="out.start"))
median(scatter.data.2$max)
stdev(scatter.data.2$max)
median(scatter.data.2$sum.freq,na.rm=T)

scatter.data.2f<-dplyr::filter(scatter.data.2, date >="2015-01-01", date <="2018-12-31")

ggscatter(scatter.data.2f, x ="max", y = "sum.freq",
          #add = "reg.line",  # Add regressin line
          ylab="Number of outages per day",
          xlab="Daily mean temperature",
          xlim=c(0,35),
          conf.int = F,
          #size = 3, alpha = 0.6
)+
  stat_cor(method = "pearson")


scatter.data.2f<-drop_na(scatter.data.2f)

capnames<-data.frame(cap=c("coal","gas","lignite","nuc"), names=c("Coal","Lignite","Nuclear"))

main<-ggplot2::ggplot(scatter.data.2f, aes( x =max, y = sum.freq) ) +
  geom_point(aes(color=as.factor(fuel)), alpha = 1)+
  scale_color_brewer(palette="Dark2", labels=capnames[,"names"])+
  theme_classic() + 
  labs(title="Frequency of outages per temperature range",
       subtitle = "(number of occurences per fuel type and time moment: data from 2015 to 2018)",
       x= paste("Temperature (Germany mean daily soil temperature in 5 cm depth °C)", sep=""), 
       y = "Frequency of outages",
       color="Fuel types")
#main

xdens<-axis_canvas(main, axis="x")+
  geom_density(data=scatter.data.2f,
               aes(x=max, fill=as.factor(fuel)),
               alpha=0.5, size=0.2) +
  scale_fill_brewer(palette="Dark2")
#xdens

#ydens<-axis_canvas(main, axis = "y", coord_flip = TRUE)+
#  geom_density(data = scatter.data.2f, 
#               aes(x = sum.freq, fill = as.factor(fuel)),
#               alpha = 0.5, size = 0.2)+
#  coord_flip()+
#  ggpubr::fill_palette("jco")

p1 <- insert_xaxis_grob(main, xdens, grid::unit(.2, "null"), position = "top")
#p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p1)


##################################################################
#                   OUTAGES PER TEMPERATURE STEP                 #
##################################################################
# calculate outages per temperature step
step<-5
tmp.step<-seq(0,35,by=step)
temp.out.step.ls<-list()

# if neede filter out the years or dates
scatter.data.3f<-dplyr::filter(scatter.data.2, date >="2018-01-01", date <="2018-12-31")

for (i in 1:(length(tmp.step)-1)) {
  temp.out.1<-dplyr::filter(scatter.data.3f, scatter.data.3f$max>=tmp.step[i] & scatter.data.3f$max<=tmp.step[i+1])
  temp.out.1$step<-tmp.step[i]
  temp.out.step.ls[[i]]<-plyr::ddply(temp.out.1, .(step, fuel), summarise, sum.freq=sum(sum.freq, na.rm = T))
}
temp.out.step.df<- dplyr::bind_rows(temp.out.step.ls[1:length(temp.out.step.ls)])
temp.out.step.df<-drop_na(temp.out.step.df)

ggplot2::ggplot(temp.out.step.df) +
  geom_bar(aes(x=as.factor(step), y=sum.freq, fill=as.factor(fuel)), stat="identity") + 
  theme_classic() + 
  scale_fill_brewer(palette="PRGn") + 
  labs(title="Frequency of outages per temperature range",
       subtitle = "(number of occurences per fuel type and time moment: data for 2018)",
       x= paste("Temperature (Germany mean daily soil temperature in 5 cm depth °C)", sep=""), 
       y = "Frequency of outages",
       fill="Fuel")

