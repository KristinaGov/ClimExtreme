rm(list=ls(all=TRUE))


library(readxl)
library(dplyr)
library(plyr)
library(tidyr)
library(raster)
library(CommEcol)
library(fBasics)
library(ggplot2)
library(ggpubr)
library(ggnewscale) #add new scale to ggplot
library(RColorBrewer)
library(lubridate)
library(ggExtra)
library(ggpmisc)
library(matrixStats)

library(cowplot)

mydir<-getwd()
setwd(mydir)

Sys.setenv(LANGUAGE="en")

##################################################################
#########                read files                    ###########
##################################################################

# READ BIG OUT DATA WITH DAILY OUTAGES
# all outages (per generation and priduction unit)
big.out.df<-read.csv("./data_outage/big_out_data_2019.csv", sep=",")
big.out.df$X<-NULL
# all outages summed if same production units have multiple outages of their generation units at the same time
# 1 READ DATA ABOUT OUTAGES GENERATED FROM xml WITH BASIC STATS
out.ts.df<-read.csv("./data_outage/out_timeseries_2019.csv")
out.ts.df$X<-NULL
# 2 read file linking plz to lon/lat
# http://download.geonames.org/export/zip/
plztolat.df<-as.data.frame(read_excel("./data_plz_util/plztolat.xlsx", 
                                      sheet = "plztolat_upd", col_names = T))
# 3 read data about EICs. Careful - PLZ data there in not a PP location, but headquaters location
eic.info.df<- read.csv("./data_outage/EIC_codes.csv", sep=";")
# delete unnecessary columns
eic.info.df[,c("UID","Strasse","PLZ","city","Website","EIC_Typ","EIC_Display_Name","International")]<-NULL

# 4 read file about the fuel types and eic codes 
eic_bcode.df<-as.data.frame(readxl::read_excel("./data_outage/eic_units_type.xlsx", 
                                               sheet = "eic_punits_type", col_names = T))
dim(eic_bcode.df) # 694
# read the table that links fuel type to bcodes in the eic_bcode.df
bcode.df<-as.data.frame(readxl::read_excel("./data_outage/eic_units_type.xlsx", 
                                           sheet = "bcode", col_names = T))
#create time stamp
timedate.df<-data.frame(date=seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months"))
timedate.df$period<-seq(1,dim(timedate.df)[1], by=1)

#########               read the ncdf                  ###########
# open nc file with drought adta
nc_ufz<-ncdf4::nc_open("./data_ufz/1951-2018.nc")
# use nc_open to read the data into a data structure I called nc_data. 
# Print the metadata about this file to a text file.
{
  sink("./data_ufz/1951-2013_metadata.txt")
  print(nc_ufz)
  sink()
}
lon<-ncdf4::ncvar_get(nc_ufz, "lon")
lat<-ncdf4::ncvar_get(nc_ufz, "lat")
smi<-ncdf4::ncvar_get(nc_ufz, "SMI")
#dim(smi)
# other pertinent information about the NDVI variable: Lets’s see what fill 
# value was used for missing data.
fillvalue <- ncdf4::ncatt_get(nc_ufz, "SMI", "missing_value")
#fillvalue
# when done reading in the data - close the netCDF file
ncdf4::nc_close(nc_ufz)
##################################################################

##################################################################
#########             read DAILY temperatures          ###########
##################################################################
files.ls<-as.list(list.files(path = "./data_temp/stations/tempfiles/", 
                             all.files = T, 
                             full.names = T,
                             pattern = "produkt_erdbo_tag"
) )
length(files.ls) # 16
# check that thay exist
exist.list1<-list()
for (i in 1:length(files.ls)) {
  exist.list1[[i]]<-file.exists(files.ls[[i]])
}

take<-c("STATIONS_ID","MESS_DATUM","V_TE005M")
stations.data.ls<-list()
for (i in 1:length(files.ls)) {
  temp.stations.tb<-read.table(files.ls[[i]], header = TRUE, sep = ";")
  stations.data.ls[[i]]<-temp.stations.tb[take]
}
stations.data.df<-as.data.frame(dplyr::bind_rows(stations.data.ls[1:length(stations.data.ls)]))
# -999 is default NA value in the CDC data. remove it for defaul R designation NA
stations.data.df$V_TE005M[stations.data.df$V_TE005M == "-999"]<-NA

#create date stamps
stations.data.df$MESS_DATUM<-strptime(stations.data.df$MESS_DATUM,format = "%Y%m%d")
stations.data.df$MESS_DATUM<-as.POSIXct(stations.data.df$MESS_DATUM,
                                        origin=as.character(stations.data.df$MESS_DATUM[1]), 
                                        format = "%Y-%m-%d")
colnames(stations.data.df)<-c("id","date","tmpr")
class(stations.data.df$date)
stations.data.df$date<-as.Date(stations.data.df$date, tz="CET")
# read data about weather stations
stations.det.df<-as.data.frame(readxl::read_excel("./data_temp/list_stations.xlsx", 
                                                  sheet = "stat", col_names = T))
##################################################################

##################################################################
#########                CHOOSE YEAR                   ###########
##################################################################
                    FromYear<-"1951-01-01"
                    ToYear<-as.Date("2018-12-31")
##################################################################

##################################################################
#########               SMI brick raster               ###########
##################################################################

# raster brick, contrary to just raster, gets timeseries at locations
# replace all those fill values with the R-standard ‘NA’.
smi[smi==fillvalue$value]<-NA
dim(smi)

# choose which time slice to work with
time.st<-dplyr::select(dplyr::filter(timedate.df, date >= FromYear), c("period"))
time.vec<-as.vector(time.st[,1])

# slice the smi file to have the number of periods on your demand FromYear
smi.slice<-smi[, , time = time.vec]
dim(smi.slice) # 175 225 816 [ dim lat, dim lon, dim time periods ]

# convert the entire 3d array of data to a raster brick
brick.raster<-brick(smi.slice,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(brick.raster) #175 225 816
# transpose (the t() function) and flip() before the data are oriented correctly
brick.raster<-flip(t(brick.raster), direction = "y")
#res(brick.raster) # 0.06224887 0.03650986
##################################################################

##################################################################
#########                PREPARE  OUTAGES              ###########
##################################################################
dim(out.ts.df) # 9401
# check if there are duplicated entries due to the index of days
out.ts.df.check<-out.ts.df[!duplicated(out.ts.df),]
dim(out.ts.df.check) # 9401
length(unique(out.ts.df$production_RegisteredResource.mRID)) # check: 106 unique eics
# merge two files to have fuel types and eic codes in one data frame
eic.fuel.df<-inner_join(eic_bcode.df, bcode.df, by=c("bcode"="bcode") )
length(unique(eic.fuel.df$peic)) # check: 195 unique peics

# the eic_bcode.df has data about last updates that includes additions of 
# capacities to the powerplants
# that is why first extract rows with the latest updates as of 2018-2019
filter.unq<-unique(eic.fuel.df$peic)
extract.eic.fuel.ls<-list()
for (i in 1:length(filter.unq)) {
  filer1.eic.df<-eic.fuel.df[eic.fuel.df$peic==filter.unq[i],]
  filer2.eic.df<-dplyr::filter(eic.fuel.df, eic.fuel.df$val_from==max(filer1.eic.df$val_from) 
                               & eic.fuel.df$peic==filter.unq[i])
  extract.eic.fuel.ls[[i]]<-filer2.eic.df[!duplicated(filer2.eic.df$peic), ]
}
extract.eic.fuel.df<- dplyr::bind_rows(extract.eic.fuel.ls[1:length(extract.eic.fuel.ls)])

# remove "unnecessary" cols
extract.eic.fuel.df[, c ("bcode","name", "zone","val_from", "val_to","voltage",
                         "control_area","bid_zone","upd")]<- NULL

# merge two databases abouzt outages and fuel, installed capacities.
outages.df<-dplyr::left_join(out.ts.df, extract.eic.fuel.df, by=c("production_RegisteredResource.mRID"="peic"))


# add "day"
outages.df<-tidyr::unite(outages.df,"date",c("st.d","st.mon","year"), sep="/", remove=T)
outages.df$date<-as.Date(dmy(outages.df$date))
outaged.pp.df<-unique(outages.df[,c("production_RegisteredResource.mRID","plz","city","in_cap","stat","fuel")])
dim(outaged.pp.df) # 106

# add fuel and detailed data to big.out.df
dim(big.out.df) # 9609
big.fuel.out.df<-dplyr::left_join(big.out.df, extract.eic.fuel.df,  by=c("production_RegisteredResource.mRID"="peic"))
dim(big.fuel.out.df) #9609
big.fuel.out.df$freq<-1

# duration of the outages per months
ggscatter(dplyr::filter(big.fuel.out.df, fuel=="nuc"), x="st.mon", y="duration_days")

# scatter plot of outages
main.plot<- ggplot2::ggplot( dplyr::filter(big.fuel.out.df,  fuel %in% c("nuc"), duration_days<100 ), 
                       aes(st.mon, duration_days, color=fuel)) + 
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
  geom_density(data=dplyr::filter(big.fuel.out.df,  fuel %in% c("nuc")),
               aes(x=st.mon, fill=as.factor(fuel)),
               alpha=0.3, size=0.2) +
  ggpubr::fill_palette("jco")
xdens.plot

p.main <- insert_xaxis_grob(main.plot, xdens.plot, grid::unit(.2, "null"), position = "top")
ggdraw(p.main)

capnames<-data.frame(cap=c("coal","gas","lignite","nuc"), names=c("Coal","Gas","Lignite","Nuclear"))

# density plot of outages
plt<-ggplot(data=dplyr::filter(big.fuel.out.df , year  %in% c("2015", "2016","2017", "2018","2019")  , 
                               fuel %in% c("coal", "nuc","gas", "lignite")),
                               aes(x=st.mon, fill=as.factor(fuel))) +
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
#length(unique(stations.data.df$MESS_DATUM)) #24830
stations.df<-as.data.frame(unique(stations.data.df$date))
colnames(stations.df)<-"date"

#take unique stations id
length(unique(stations.data.df$id)) #20
stations.id<-as.data.frame(unique(stations.data.df$id))
colnames(stations.id)<-"id"
# join to one file with all data on the samples stations
about.stations.df<-dplyr::left_join(stations.id, stations.det.df, by=c("id"="id") )

for (i in 1:dim(stations.id)[1]) {
  tomerge.df<-dplyr::select(dplyr::filter(stations.data.df, id==about.stations.df[i,1]),
                            c("date", "tmpr"))
  cname<-paste(about.stations.df[i,1], sep="")
  colnames(tomerge.df)<-c("date", cname)
  stations.df<-left_join(stations.df, tomerge.df, by=c("date"="date"))
}

# for the johansen test stations.df must have a monthly time resolution
mon.stations.df<-group_by(stations.df, month=floor_date(date,"month"))
agg<-colnames(mon.stations.df[,2:(dim(mon.stations.df)[2]-1)])
mon.stations.df<-aggregate(mon.stations.df[,agg], list(mon.stations.df$month), mean)
colnames(mon.stations.df)<-c("date", agg)
##################################################################


##################################################################
###       find closest weather stations to power plants       ####
##################################################################
# add lon lat to the list of outages power plants
outaged.pp.df<-left_join(outaged.pp.df, plztolat.df, by=c("plz"="plz","city"="city"))

# find closest weather stations

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
#########           extract smi time series            ###########
##################################################################
# extract smi time series where the outages are

# function to extract available smi for lat/lon coordinates lying in a defined radius
radiusextract<-function(lon,lat,gridshp,radius=0.5){
  coor<-coordinates(gridshp)
  lonpar<-coor[,1][which(abs(coor[,1]-lon)%in%min(abs(coor[,1]-lon)) )]
  latpar<-coor[,2][which(abs(coor[,2]-lat)%in%min(abs(coor[,2]-lat)) )]
  data<-data.frame(x=coor[,1], y=coor[,2], val=values(gridshp))
  return(CommEcol::select.window(xf=lonpar[1], yf=latpar[1], radius=radius, xydata=data))}

myrad<-0.07
radsmi.ls<-list()
brick.ls<-list()
for (i in 1:dim(loc.stat.df)[1]) {
  x<-loc.stat.df[i,"lon"]
  y<-loc.stat.df[i,"lat"]
  rad.loc<-radiusextract(lon=x, lat=y, gridshp = brick.raster, radius=myrad)
  temp.radsmi.df<-data.frame(smi=t(rad.loc[,-(1:2)]))
  temp.mean.df<-data.frame(means=rowMeans(temp.radsmi.df))
  colnames(temp.mean.df)<-loc.stat.df[i,c("eic")]
  brick.ls[[i]]<- temp.mean.df
}
#length(brick.ls) # 20
brick.all.df<- dplyr::bind_cols(brick.ls[1:length(brick.ls)])
brick.all.df$date<-timedate.df$date
# check if there are NA values for smi:
sum(is.na(brick.all.df))/length(time.vec)
##################################################################


##################################################################
#########                    PLOT                      ###########
##################################################################

#take names of weather stations
weather.stat.name<-unique(loc.stat.df$id.st)
n<-which(weather.stat.name == 1303)
dplyr::filter(about.stations.df, id==weather.stat.name[n])

# OUTAGES MUCT BE PER MONTH!!!! NOT PER DAY

extract.pp.1<-unlist(dplyr::filter(loc.stat.df, id.st==weather.stat.name[n])["eic"])
extraxct.smi.1<-brick.all.df[,c(extract.pp.1,"date")]
extraxct.smi.1$means<-rowMeans(extraxct.smi.1[,extract.pp.1]) 
# tund the date stamp to year-mon format
#extraxct.smi.1$date<-format(extraxct.smi.1$date, format="%Y-%m")
#extraxct.smi.1$means<-extraxct.smi.1[,extract.pp.1]
extract.tmp.1<-mon.stations.df[,c("date",weather.stat.name[n])]
extract.out.1<-dplyr::filter(outages.df, production_RegisteredResource.mRID %in% extract.pp.1)

# sum outages by month
extract.out.1<-plyr::ddply(dplyr::select(extract.out.1, c("date",
                                            "production_RegisteredResource.mRID",
                                            "freq",
                                            "in_cap",
                                            "fuel")), 
             .( month(date), year(date), production_RegisteredResource.mRID, fuel, in_cap), summarise, freq=sum(freq))
# create year-month time stamp again (all outages per month and assigned to the first day of month)
extract.out.1$date<-as.Date(paste(extract.out.1$`year(date)`,extract.out.1$`month(date)`,"1" , sep="-"), format="%Y-%m-%d")
extract.out.1<-dplyr::filter(extract.out.1, date<="2018-12-31")

# take time series only when the outages data is available and merge
extract.all.1<-full_join(dplyr::select(dplyr::filter(extraxct.smi.1, date>=as.Date("2015-01-01")), c("date","means")),
                         extract.out.1[,c("date",
                                          "production_RegisteredResource.mRID",
                                          "freq",
                                          "in_cap",
                                          "fuel")], 
                         by=c("date"="date")) %>% left_join(extract.tmp.1, by=c("date"="date"))


station<-as.character(dplyr::filter(about.stations.df, id==weather.stat.name[n])[1])

ggline.1<-ggplot(extract.all.1, aes(x=date))+
  geom_line(aes(y=extract.all.1[,station]/25, col="black"),  cex=1)+
  scale_y_continuous(sec.axis = sec_axis(~.*25, name="Temperature °C"), 
                     labels = scales::number_format(accuracy = 0.01))+
  geom_line(aes(y=means, col="blue"), cex=1) +
  labs(title=paste("Temperatures and SMI drought index", sep = ""),
       subtitle = paste("in ",dplyr::filter(about.stations.df, id==weather.stat.name[n])[7],", ",
                        dplyr::filter(about.stations.df, id==weather.stat.name[n])[8], sep = "") ,
       y="SMI", x="Date") +
  scale_color_manual(name = "",
                     values=c("black", "blue"),
                     labels=c("°C", "SMI"))+
  scale_x_date(date_labels = "%Y (%b)") +
  #theme_minimal() +
  theme(legend.position = "top")
#ggline.1

ggbar.1<-ggplot(extract.all.1, aes(x=date))+
  geom_bar(aes(y=freq , fill=fuel), stat = "identity", position = "stack") +
  scale_y_continuous(sec.axis = sec_axis(~., name="Outages", 
                                         labels = scales::number_format(accuracy = 1)),
                     labels = scales::number_format(accuracy = 1))+
  scale_fill_brewer(palette="Spectral")+
  labs(title="Unplanned outages per type of power plant by fuel",
       y="Outages", x="Date", fill="Fuel type:")+
  scale_x_date(date_labels = "%Y (%b)") +
  #theme_minimal() +
  theme(legend.position = "top")
ggbar.1
#
gridExtra::grid.arrange(ggline.1, ggbar.1, ncol=1, nrow=2)

##################################################################
#########            PLOT STATIONs' POSITIONS          ###########
##################################################################
# plot where these stations are
# for this we need a slice raster file
FromYear<-as.Date("2018-04-01")
ToYear<-as.Date("2018-11-01")
##################################################################
plot.stat<-dplyr::filter(about.stations.df, id %in% weather.stat.name )

time.st<-dplyr::select(dplyr::filter(timedate.df, date >= FromYear & date <=ToYear ), c("period"))
#dim(smi)
# choose which time slice to graph
smi.raster<-smi[,,time.st$period]
dim(smi.raster)
smi.raster<-as.array(smi.raster)
# TAKE MEANS AT EACH LOCATION FOR smi SINCE FromYear
smi.raster.mean<-as.array(rowMeans(smi.raster, dim=2))
dim(smi.raster.mean)
slice.raster <- raster(t(smi.raster.mean), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster <- flip(slice.raster, direction='y')

r.spdf <- as(slice.raster, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
#head(r.df)

#display.brewer.all()
pal<-colorRampPalette(brewer.pal(10, 'Paired'))(dim(r.df)[1])
palSz <- dim(r.df)[1] 

#file.name<-paste("./plots/", "data_tmp_smi_out" , FromYear, "-", ToYear, ".svg", sep="")
#svg(file.name,  width = 500, height = 500)
ggplot(r.df, aes(x=x, y=y)) + 
  geom_tile(aes(fill = layer)) + 
  scale_fill_gradient2( name=paste("SMI index", sep=""),
                        low="cadetblue1",
                        mid = pal[palSz/2],
                        high = pal[which(r.df[,"layer"]==min(r.df[,"layer"], na.rm = T))],
                        midpoint = ((max(r.df[,"layer"], na.rm = T) + min(r.df[,"layer"], na.rm = T)) / 2)
                        )+
  # add points with weather staions
  new_scale("fill") +
  geom_point(data=plot.stat, aes(x=lon, y=lat), pch=0.5)+
  geom_label(data=plot.stat,aes(x=lon, y=lat), label=plot.stat$city,
             nudge_x=0.1, nudge_y=0.2)+
  labs(x="", y="", title=paste("position of weather stations"),
       subtitle = paste("SMI is monthly average between ",FromYear,"-",ToYear, sep="")) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") 
#dev.off()
##################################################################


##################################################################
#########    PLOT SMI comparison 1951-2014 2015-2018   ###########
##################################################################
# plot where these stations are
# for this we need a slice raster file
From51<-as.Date("1951-01-01")
To14<-as.Date("2014-12-01")

From15<-as.Date("2015-01-01")
To18<-as.Date("2018-12-01")
##################################################################

time51_14.st<-dplyr::select(dplyr::filter(timedate.df, date >= From51 & date <=To14 ), c("period"))

time15_18.st<-dplyr::select(dplyr::filter(timedate.df, date >= From15 & date <=To18 ), c("period"))

# choose which time slice to graph
smi.raster51_14<-smi[,,time51_14.st$period]
dim(smi.raster51_14)
smi.raster15_18<-smi[,,time15_18.st$period]
dim(smi.raster15_18)

smi.raster51_14<-as.array(smi.raster51_14)
smi.raster15_18<-as.array(smi.raster15_18)

smi.raster.mean51_14<-as.array(rowMeans(smi.raster51_14, dim=2))
smi.raster.mean15_18<-as.array(rowMeans(smi.raster15_18, dim=2))

smi.raster.min51_14<-as.array(apply(smi.raster51_14, c(1,2), min, na.rm=F))
smi.raster.min15_18<-as.array(apply(smi.raster15_18, c(1,2), min, na.rm=F))

dim(smi.raster.min51_14)
dim(smi.raster.min15_18)

# if mean 51-14 is greater than mean 15-18 (less dry), then show 15-18, esle - 0 (these pleces became more precipitated)
smi.raster.compared<-ifelse(smi.raster.mean51_14>=smi.raster.mean15_18, smi.raster.mean15_18, 0 )
# if min 51-14 is greater than minimum 15-18 (less dry), then show 15-18, esle - 0 (these pleces were more precipitated)
smi.raster.comp.min<-ifelse(smi.raster.min51_14>=smi.raster.min15_18, smi.raster.min15_18, 0 )

identical(smi.raster.compared, smi.raster.comp.max)

# TAKE MEANS AT EACH LOCATION FOR smi SINCE FromYear
slice.raster.compared <- raster(t(smi.raster.compared), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.compared <- flip(slice.raster.compared, direction='y')

slice.raster.comp.min <- raster(t(smi.raster.comp.min), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.comp.min <- flip(slice.raster.comp.min, direction='y')

r.spdf.c <- as(slice.raster.compared, "SpatialPixelsDataFrame")
r.df.c <- as.data.frame(r.spdf.c)
#head(r.df)
r.spdf.m <- as(slice.raster.comp.min, "SpatialPixelsDataFrame")
r.df.m <- as.data.frame(r.spdf.m)

#file.name<-paste("./plots/", "data_tmp_smi_out" , FromYear, "-", ToYear, ".svg", sep="")
#svg(file.name,  width = 500, height = 500)
ggplot(r.df.c, aes(x=x, y=y)) + 
  geom_tile(aes(fill = layer)) + 
  scale_fill_gradientn( name=paste("SMI index", sep=""),
                        colours = c("grey", "darkorange3", "darkolivegreen2"), 
                        values = c(0,0.0001,1)  )+
  labs(x="", y="", title=paste("position of weather stations"),
       subtitle = paste("SMI is monthly average between ",FromYear,"-",ToYear, sep="")) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") 
#dev.off()
##################################################################


##################################################################
#                 ANALYSE TEMP vs OUTAGES 
#                         MONTHLY
##################################################################

# filter temperatures data for the period in outages set data
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

# calculate all outages in a month
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

