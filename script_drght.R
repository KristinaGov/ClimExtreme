# Script...

# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "plyr","raster","ncdf4","ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "pastecs",
           "CommEcol", "fBasics", "EnvStats", "geosphere", "ggpmisc",
           "lubridate","tidyquant", "gridExtra", "tidyverse", "viridis",
           "ts", "strucchange", "fxregime", "zoo", "fitdistrplus")
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

#########                read files                    ###########
# 1 READ DATA ABOUT OUTAGES GENERATED FROM xml WITH BASIC STATS
# out.ts.df<-read.csv("./data_outage/out_timeseries_2019.csv")
out.ts.df<-read.csv("./data_outage/big_out_data_2019.csv")
dim(out.ts.df) # 9636
dim(out.ts.df[!duplicated(out.ts.df$production_RegisteredResource.mRID),]) # 106 number of unique EICs

# 2 read file linking plz to lon/lat
# http://download.geonames.org/export/zip/
plztolat.df<-as.data.frame(read_excel("./data_plz_util/plztolat.xlsx", 
                                      sheet = "plztolat_upd", col_names = T))

# 3 read data about EICs. Careful - PLZ data there in not a PP location, but headquaters location
eic.info.df<- read.csv("./data_outage/EIC_codes.csv", sep=";")

# delete unnecessary columns
eic.info.df[,c("UID","Strasse","PLZ","city","Website","EIC_Typ","EIC_Display_Name","International")]<-NULL

eic.df<-as.data.frame(read_excel("./data_outage/eic_bna_plz_kwk.xlsx", 
                                 sheet = "eic_plz", col_names = T))

# 4 read file about the fuel types and eic codes:
eic_bcode.df<-as.data.frame(readxl::read_excel("./data_outage/eic_units_type.xlsx", 
                                               sheet = "eic_punits_type", col_names = T))
dim(eic_bcode.df) # 694 12
# read the table that links fuel type to bcodes in the eic_bcode.df:
bcode.df<-as.data.frame(readxl::read_excel("./data_outage/eic_units_type.xlsx", 
                                           sheet = "bcode", col_names = T))
# merge two files to have fuel types and eic codes in one data frame:
eic.fuel.df<-inner_join(eic_bcode.df, bcode.df, by=c("bcode"="bcode") )
length(unique(eic.fuel.df$peic)) # check: 195 unique peics

# The eic_bcode.df has data about last updates that includes additions of capacities to the power plants,
# that is why we need to first extract rows with the latest updates as of 2018-2019:
filter.unq<-unique(eic.fuel.df$peic)
extract.eic.fuel.ls<-list()
for (i in 1:length(filter.unq)) {
  filer1.eic.df<-eic.fuel.df[eic.fuel.df$peic==filter.unq[i],]
  filer2.eic.df<-dplyr::filter(eic.fuel.df, eic.fuel.df$val_from==max(filer1.eic.df$val_from) & eic.fuel.df$peic==filter.unq[i])
  extract.eic.fuel.ls[[i]]<-filer2.eic.df[!duplicated(filer2.eic.df$peic), ]
}
extract.eic.fuel.df<- dplyr::bind_rows(extract.eic.fuel.ls[1:length(extract.eic.fuel.ls)])
length(unique(extract.eic.fuel.df$peic)) # check: 195 unique peics of power plants with defined fuel types
sum(is.na(extract.eic.fuel.df$fuel)) # check: have to be 0
# Remove "unnecessary" cols
extract.eic.fuel.df[, c ("bcode","name", "zone","val_from", "val_to","stat","in_cap","voltage","control_area","bid_zone","upd")]<- NULL

# 5 make the sequence of time stamps from scratch:
years<-as.data.frame(stringr::str_split_fixed(seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months"), "-", 3))
years$V1<-as.numeric(as.character(years$V1))
years$V2<-as.numeric(as.character(years$V2))
years$V3<-as.numeric(as.character(years$V3))
#sapply(years, class)
timedate.df<-data.frame(period=seq(1,816,1), year=years[,1], mon=years[,2], date=seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months") )
#################################################################

#########       add location data to powerplnts       ###########
out_eic_plz.df<- dplyr::left_join(out.ts.df, eic.df, by=c("production_RegisteredResource.mRID"="production_RegisteredResource.mRID"))
dim(out_eic_plz.df) #9636 ; for 2019 9636
dim(out_eic_plz.df[!duplicated(out_eic_plz.df$production_RegisteredResource.mRID),]) #  should be 106 

# Add location data:
plztolat.df$plz<-as.character(plztolat.df$plz)
out.ts.loc.df<-inner_join(out_eic_plz.df, plztolat.df, by=c("plz"="plz", "city"="city"))
dim(out.ts.loc.df)    # 9636
dim(out.ts.loc.df[!duplicated(out.ts.loc.df$production_RegisteredResource.mRID),]) # 106 number of unique EICs
dim(out.ts.loc.df[!duplicated(out.ts.loc.df$plz),])  # 81 unique plz's
##################################################################

#########               read the ncdf                  ###########
# Open nc file with drought adta
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
#########                CHOOSE YEAR                   ###########
# 2015 # year when outage data starts
FromYear<-as.Date("1951-01-01") # year when smi data starts
##################################################################

#########               brick raster                   ###########
# raster brick, contrary to just raster, gets timeseries at locations
# replace all those fill values with the R-standard ‘NA’.
smi[smi==fillvalue$value]<-NA
dim(smi) # 175 225 816
# choose which time slice to work with
time.st<-dplyr::select(dplyr::filter(timedate.df, year >= year(FromYear)), c("period"))
time.vec<-as.vector(time.st[,1])
#length(time.vec)
# Slice the smi file to have the number of periods on your demand FromYear:
smi.slice<-smi[, , time = time.vec]
dim(smi.slice) # [ dim lat, dim lon, dim time periods ]

# convert the entire 3d array of data to a raster brick
brick.raster<-brick(smi.slice,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(brick.raster)
# transpose (the t() function) and flip() before the data are oriented correctly
brick.raster<-flip(t(brick.raster), direction = "y")
#res(brick.raster)
##################################################################

#########        brick raster for reference smi        ###########
##################################################################
#########                CHOOSE YEAR                   ###########
FromYear.smi<-1951  # year from which we have smi data
ToYear.smi<-2018    # any reference year we need
##################################################################

# choose which time slice to work with
time.st.smiref<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear.smi & year <= ToYear.smi), c("period"))
time.vec.smiref<-as.vector(time.st.smiref[,1])

# slice the smi file to have the number of periods on your demand FromYear
smiref.slice.<-smi[, , time = time.vec.smiref]
dim(smiref.slice.) # [ dim lat, dim lon, dim time periods ]

# Кaster brick, contrary to just raster, gets timeseries at locationsю
# Сonvert the entire 3d array of data to a raster brickЮ
brick.raster.smiref<-brick(smiref.slice.,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(brick.raster.smiref) # 175 225 540
# Transpose (the t() function) and flip() before the data are oriented correctly
brick.raster.smiref<-flip(t(brick.raster.smiref), direction = "y")
#res(brick.raster)
##################################################################

# filter just unique eic keys and location info + outages of these eic'S
unq.out.ts.loc.df<-dplyr::select(out.ts.loc.df[!duplicated(out.ts.loc.df$production_RegisteredResource.mRID),], c(production_RegisteredResource.mRID,plz,city,region,lat,lon))
dim(unq.out.ts.loc.df)   # 106 number of unique EICs

##################################################################
#########           get (new) location data            ###########
#########                for every eic                 ###########
# NECESSARY TO EXTRACT BRICK SERIES, SINCE RESOLUTION OF LAT/LON VALUES
# IS NOT THE SAME IN PLZTOLAT AND RASTER DATA
xyz<-rasterToPoints(brick.raster$layer.1)
mat.loc.df<-data.frame(lat=xyz[,2], lon=xyz[,1] )
plz.lonlat.ls<-list()
for (i in 1:dim(unq.out.ts.loc.df)[1]) {
  loc.df<-unq.out.ts.loc.df[i,]
  my.loc.mat<-as.matrix(loc.df[,c("lat", "lon")])
  
  mat.loc.df$loc.lat<-loc.df$lat
  mat.loc.df$loc.lon<-loc.df$lon
  mat.loc.df$euc<-sqrt(apply( (( as.matrix(mat.loc.df[,c("lat", "lon")]) - as.matrix(mat.loc.df[,c("loc.lat", "loc.lon")]) ) ^ 2),1, sum) )
  
  temp.lonlat.df<-data.frame(eic=loc.df$production_RegisteredResource.mRID,
                             plz=loc.df$plz, 
                             city=loc.df$city,
                             region=loc.df$region,
                             near.lon=mat.loc.df[which.min(mat.loc.df$euc),"lon"], 
                             near.lat=mat.loc.df[which.min(mat.loc.df$euc),"lat"])
  plz.lonlat.ls[[i]]<-temp.lonlat.df
}
#sapply(plz.lonlat.ls[[1]], class)
plz.loc.df<- dplyr::bind_rows(plz.lonlat.ls[1:length(plz.lonlat.ls)])
dim(plz.loc.df) # check: 106 by the number of unique eics, 6
sum(is.na(plz.loc.df$near.lon))  # check: should be 0
sum(is.na(plz.loc.df$near.lat))  # check: should be 0
##################################################################

##################################################################
#      take mean smi values in the radius around each eic        #
##########          radius coord and mean smi          ###########
##################################################################
# function to extract available smi for lat/lon coordinates lying in a defined radius
radiusextract<-function(lon,lat,gridshp,radius=0.5){
  coor<-coordinates(gridshp)
  lonpar<-coor[,1][which(abs(coor[,1]-lon)%in%min(abs(coor[,1]-lon)) )]
  latpar<-coor[,2][which(abs(coor[,2]-lat)%in%min(abs(coor[,2]-lat)) )]
  data<-data.frame(x=coor[,1], y=coor[,2], val=values(gridshp))
  return(select.window(xf=lonpar[1], yf=latpar[1], radius=radius, xydata=data))}

# Define my desired radius and check with spDistsN1 below:
myrad<-0.04
radsmi.ls<-list()
# Check dimensions if needed:
#my.dim.ls<-list()
for (i in 1:length(plz.lonlat.ls)) {
  nr.lon<-plz.lonlat.ls[[i]][,5]
  nr.lat<-plz.lonlat.ls[[i]][,6]
  #plz.lonlat.ls[[255]][2]
  rad.loc<-radiusextract(lon=nr.lon, lat=nr.lat, gridshp = brick.raster, radius=myrad)
  #my.dim.ls[i]<-(dim(rad.loc))[2]
  temp.radsmi.df<-data.frame(period=time.st[,1] , smi=t(rad.loc[,-(1:2)]))
  temp.mean.df<-data.frame(period=temp.radsmi.df$period, 
                           smi.radmeans=rowMeans(temp.radsmi.df[,-1]), 
                           eic=plz.lonlat.ls[[i]][,1],
                           plz=plz.lonlat.ls[[i]][,2], 
                           city=plz.lonlat.ls[[i]][,3],
                           regio=plz.lonlat.ls[[i]][,4],
                           lat=nr.lat, 
                           lon=nr.lon)
  radsmi.ls[[i]]<-temp.mean.df
}
length(radsmi.ls) # 106 by each EIC code
radsmi.all.df<- dplyr::bind_rows(radsmi.ls[1:length(radsmi.ls)])
dim(radsmi.all.df) # 86496 
#check.my<-unlist(my.dim.ls)
#View(check.my)
# spDistsN1 returns a numeric vector of distances in   
# kilometers if longlat=TRUE. ufz SMI.ncdf data has a resolution 4 × 4 km2 
# then the radius of 0.04 will approx give us this distance between the points
##################################################################

##################################################################
#           Check SMI time series for structural break  
##################################################################
# aggregate the selected smi by taking average for region
aggregate.smi<-ddply(radsmi.all.df, .(period, regio), summarize, regio.mean=mean(smi.radmeans) )
aggregate.smi<-left_join(aggregate.smi, timedate.df, by=c("period"="period"))
aggregate.smi.range<-ddply(radsmi.all.df, .(period), summarize, max=max(smi.radmeans), min=min(smi.radmeans), mean=mean(smi.radmeans))
aggregate.smi.range<-left_join(aggregate.smi.range, timedate.df, by=c("period"="period"))

start.y<-min(aggregate.smi.range$date)
end.y<-max(aggregate.smi.range$date)

aggregate.smi.ranges<-subset(aggregate.smi.range, year >= 1951 & year <= 2017)

aggregate.smi.ts<-ts(aggregate.smi.ranges$mean, start=c(1951,01,01),frequency=12)
#aggregate.smi.ts<-zoo(aggregate.smi.range$mean, aggregate.smi.range$date)
glimpse(aggregate.smi.ts)
#plot(aggregate.smi.ts)

d <- ts.intersect(y = log(aggregate.smi.ts), y.diff = lag(log(aggregate.smi.ts), -1))
#plot.ts(d)

sc <- efp(y ~ y.diff, data = d, type = "Score-CUSUM")
plot(sc, functional = NULL)

bd <- fxregimes(y ~ y.diff, data = d, breaks = 5, ic = "BIC")
plot(bd) #BIC indicating optimal number of breakpoints is 2: 3 periods: 1977, 2006
summary(bd)

bp <- breakpoints(y ~ y.diff, data = d,  breaks = 5, h=12)
coef(bp)
##################################################################

##################################################################
#             Plot SMI time series for locations  
#             And long-term smi trend unsing SMA
##################################################################
aggregate.smi.range.j<-subset(aggregate.smi.range, mon==6)
aggregate.smi.range.d<-subset(aggregate.smi.range, mon==12)

sma_shift<-12
aggregate.smi.range$minsma<-SMA(aggregate.smi.range$min, n = sma_shift)
aggregate.smi.range$maxsma<-SMA(aggregate.smi.range$max, n = sma_shift)
aggregate.smi.range$meansma<-SMA(aggregate.smi.range$mean, n = sma_shift)

aggregate.smi.range$regyear<-ifelse(aggregate.smi.range$year>=2006, "After 2006", 
                                    ifelse(aggregate.smi.range$year>=1977, "After 1977", "Before 1977")) 
aggregate.smi.range.long<-gather(aggregate.smi.range, mtype, mval, c("minsma","maxsma","meansma") )
my.formula <- y ~ x

ggplot(aggregate.smi.range.long, aes(x=date, y=mval, color=mtype, fill=regyear)) +
  geom_linerange( aes(ymin = min, ymax = max), col = brewer.pal(4, "Pastel2")[3]) +
  geom_smooth(method="lm") +
  #stat_poly_eq(formula = my.formula, aes(label = paste(..rr.label..)),   parse = TRUE) + 
  #stat_cor() +
  geom_line() +
  theme_minimal()+
  scale_x_date(labels = scales::date_format("%Y", tz="CET"), date_breaks = "5 years", expand = c(0,0) )+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top")+
  labs(y="SMI", x="", fill="Year", color = "SMI Measurment") +
  scale_color_brewer(palette = "Dark2", labels = c("min (SMA)", "max (SMA)", "mean (SMA)"))+
  scale_fill_brewer(palette = "Dark2")
#ggsave("./Rplots/SMI_timeseries.svg")

# Look at june and december values.
aggregate.smi.range.j$minsma<-SMA(aggregate.smi.range.j$min, n = sma_shift)
aggregate.smi.range.j$maxsma<-SMA(aggregate.smi.range.j$max, n = sma_shift)
aggregate.smi.range.j$meansma<-SMA(aggregate.smi.range.j$mean, n = sma_shift)
aggregate.smi.range.j$regyear<-ifelse(aggregate.smi.range.j$year>=2000, "After 2000", "Before 2000") 
aggregate.smi.range.jlong<-gather(aggregate.smi.range.j, mtype, mval, c("min","max","mean") )

aggregate.smi.range.d$minsma<-SMA(aggregate.smi.range.d$min, n = sma_shift)
aggregate.smi.range.d$maxsma<-SMA(aggregate.smi.range.d$max, n = sma_shift)
aggregate.smi.range.d$meansma<-SMA(aggregate.smi.range.d$mean, n = sma_shift)
aggregate.smi.range.d$regyear<-ifelse(aggregate.smi.range.d$year>=2000, "After 2000", "Before 2000") 
aggregate.smi.range.dlong<-gather(aggregate.smi.range.d, mtype, mval, c("min","max","mean") )

ggplot(aggregate.smi.range.dlong, aes(x=date, y=mval, color=mtype, fill=regyear)) +
  geom_smooth(method="lm") +
  geom_line() +
  theme_minimal()+
  scale_x_date(labels = scales::date_format("%Y", tz="CET"), date_breaks = "5 years", expand = c(0,0) )+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top")+
  labs(y="SMI", x="", fill="Year", color = "SMI Measurment") +
  scale_color_brewer(palette = "Dark2", labels = c("min", "max", "mean"))+
  scale_fill_brewer(palette = "Dark2")
##################################################################

#########              analyse series                  ###########
# Log-Normal Distribution: https://stat.ethz.ch/~stahel/lognormal/
# https://uk.mathworks.com/help/stats/lognstat.html

stat.ls<-list()
for (i in 1:length(radsmi.ls)) {
  
  temp.stat.df<-as.data.frame(t(round(pastecs::stat.desc(radsmi.ls[[i]][2]), 6)))
  temp.stat.df[, c("SE.mean", "CI.mean.0.95", "sum", "range")]<-NULL
  
  log.vector<-log(radsmi.ls[[i]][,2])
  #plot(sort((log.vector)))
  meanlog.x<-mean(log.vector)
  stdlog.x<-stdev(log.vector)
  varlog.x<-var(log.vector)
  #stat.lrnorm<-EnvStats::elnorm(radsmi.ls[[i]][,2])
  
  # sigma and mean are sigma and mean of the log(X) vector
  meanlog<-exp(meanlog.x)*exp((stdlog.x^2)/2)
  medlog<-exp(meanlog.x)
  varlog<-(exp(stdlog.x^2)-1)*exp(stdlog.x^2)*exp(2*meanlog.x)
  stdlog<-sqrt(varlog)
  skewlog<-(exp(stdlog.x^2)+2)*sqrt(-1+exp(stdlog.x^2))
  kurtlog<-exp(4*stdlog.x^2)+2*exp(3*stdlog.x^2)+3*exp(2*stdlog.x^2)-3
  
  #mu<-log(meanlog/sqrt(1+varlog/(meanlog^2)))
  #sigm<-sqrt(log(1+varlog/meanlog^2))
  
  temp.stat.df$smi.mean_logX<-meanlog.x
  temp.stat.df$smi.stdev_logX<-stdlog.x
  temp.stat.df$smi.var_logX<-varlog.x
  
  temp.stat.df$smi.mean_log<-meanlog
  temp.stat.df$smi.median_log<-medlog
  temp.stat.df$smi.var_log<-varlog
  temp.stat.df$smi.stdev_log<-stdlog
  temp.stat.df$smi.skew_log<-skewlog
  temp.stat.df$smi.krtsis_log<-kurtlog
  
  temp.stat.df$eic<-unique(radsmi.ls[[i]][,3])
  temp.stat.df$plz<-unique(radsmi.ls[[i]][,4])
  temp.stat.df$lat<-unique(radsmi.ls[[i]][,7])
  temp.stat.df$lon<-unique(radsmi.ls[[i]][,8])
  temp.stat.df$city<-unique(radsmi.ls[[i]][,5])
  temp.stat.df$region<-unique(radsmi.ls[[i]][,6])
  stat.ls[[i]]<-temp.stat.df
}
# DATAFRAME with statistics about smi valies at location of power plants with outages
stat.all.df<- dplyr::bind_rows(stat.ls[1:length(stat.ls)])
dim(stat.all.df) # check: 106
#sapply(stat.ls[[1]], class)
##################################################################

# Check fitness of the lognormal disrtibution
#fitdist(radsmi.ls[[1]][,2], "lnorm" )

##################################################################
#########             joining databases I              ###########
##################################################################
# Add years and months to smi time series additionally to just periods
dim(radsmi.all.df) # check: 86496
smi.ts.df<-inner_join(timedate.df, radsmi.all.df, by=c("period"="period"))
dim(smi.ts.df) # check:86496
length(unique(smi.ts.df$plz)) #81
length(unique(smi.ts.df$eic))  # check: 106 unique eics
# EICs give us an information and tell which power plant was chosen 
# for the center of the radius SMI around.

##################################################################
#########     buld distribution and frequency of smi   ###########
#########          1951-1995   and   1995-2018         ###########
##################################################################
length(unique(smi.ts.df$plz)) #81

# Add a period marker before and after 2000
smi.ts.df$marker<- ifelse(smi.ts.df$year <= 2000, "Before 2000", "After 2000")

values.1<-dplyr::filter(smi.ts.df, marker=="Before 2000")
values.2<-dplyr::filter(smi.ts.df, marker=="After 2000")

breaks <- seq(0, 1, by=0.025)

bs<-data.frame(x1=breaks, x2=seq(0.025, 1.025, by=0.025))
bs$labels<-paste(bs$x1, bs$x2, sep="-")
bs<-bs[1:(length(bs$x1)-1),]
bs$'Before 2000'<-probs.1[,2]
bs$'After 2000'<-probs.2[,2]

colors<-c("Before 2000" = "red", "After 2000" = "green3", "Intercept" = "black")

ggplot(smi.ts.df, aes(x=smi.radmeans, fill=marker, color=marker), aplha=0.3)+
  geom_histogram(bins = 50, alpha=0.3,
                 position = "identity"
                 , aes(y = ..density..) )+
  scale_y_continuous("Probability Density")   +
  scale_fill_manual(name="", values = colors, labels=c("After 2000" ,"Before 2000", "Intercept")) +
  scale_color_manual(name="",values = colors, labels=c("After 2000" ,"Before 2000", "Intercept")) +
  labs(x="SMI", fill="") +
  theme_minimal()+
  theme(legend.position = c(0.1, 0.95))
#################################################################


##################################################################
#########             joining databases II             ###########
##################################################################
dim(out.ts.loc.df) # check: 9636
dim(smi.ts.df) # check: 86496

smi.ts.df.15<-subset(smi.ts.df, year>=2015)
length(unique(out.ts.loc.df$production_RegisteredResource.mRID)) # 106

# Join rad.smi data for each power plant location and outage data by location:
# Note that the power plant EICs in the rad.smi data is for power plants with outages:
smi.out.stat.df<-left_join(smi.ts.df.15, dplyr::select(out.ts.loc.df, c("production_RegisteredResource.mRID","year","st.mon","city","plz","duration_days"
                                                                        ,"st.datetime","end.datetime" )), 
                           by=c("eic" = "production_RegisteredResource.mRID", "year"="year", "mon"="st.mon", "city"="city", "plz"="plz" ))
smi.out.stat.df<-unique(smi.out.stat.df)
dim(smi.out.stat.df) # check: 10973
# Add fuel types
fuel.smi.out.stat.df<-left_join(smi.out.stat.df, extract.eic.fuel.df, by=c("eic"="peic"))
dim(fuel.smi.out.stat.df) # 10973
sum(is.na(fuel.smi.out.stat.df$fuel)) # should be 0
unique(fuel.smi.out.stat.df$fuel)
##################################################################
file.name.1<-paste("./data_outage/smi_out_stat_ts_2019.csv" )
#write.csv(fuel.smi.out.stat.df, file=file.name.1)
##################################################################


##################################################################
#########               inspect outages                ###########
##################################################################
#########                 INSPECTION 1                 ###########
agg.out.smi.df<-ddply(fuel.smi.out.stat.df, .(period, year,mon,date,smi.radmeans,
                                              plz,city,regio,lat,lon), summarize, freq=length(eic) )

# make a simple cor test
inspect.1<-dplyr::filter(agg.out.smi.df, !is.na(agg.out.smi.df$freq) )
cor.test.1<-cor.test(inspect.1$smi.radmeans, inspect.1$freq, method = c("pearson"))


# Plot the correlation
ggscat<-ggpubr::ggscatter(inspect.1, x = "smi.radmeans", y = "freq", 
                          color = "mon", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "monthly smi value", ylab = "relative index of outages: ln(frequency × mean of outages per month) ") +
  theme(legend.position="right") +
  scale_color_gradient2(low = "green4", mid = "white",
                        high = "purple4", midpoint = 7) +
  labs(color="months")
ggExtra::ggMarginal(ggscat, type = "histogram", margins = "x")
#ggsave("./Rplots/outage_scatter.svg")

##################################################################

##################################################################
#########                 INSPECTION 2.1               ###########
##################################################################
# get reference smi to filter out every smi below average for that month over 1951-1995

#########        brick raster for reference smi        ###########
# Raster brick, contrary to just raster, gets timeseries at locations
FromYear.smi<-1951
ToYear.smi<-2000
time.st.smiref<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear.smi & year <= ToYear.smi), c("period"))
time.vec.smiref<-as.vector(time.st.smiref[,1])
smiref.slice.<-smi[, , time = time.vec.smiref]
dim(smiref.slice.) # [ dim lat, dim lon, dim time periods ]
brick.raster.smiref<-brick(smiref.slice.,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
brick.raster.smiref<-flip(t(brick.raster.smiref), direction = "y")
# Define my desired radius and check with spDistsN1 below
myrad<-0.04
radsmiref.ls<-list()
# to check dimensions if needed
#my.dim.ls<-list()
for (i in 1:length(plz.lonlat.ls)) {
  nr.lon<-plz.lonlat.ls[[i]][,"near.lon"]
  nr.lat<-plz.lonlat.ls[[i]][,"near.lat"]
  #plz.lonlat.ls[[255]][2]
  rad.loc<-radiusextract(lon=nr.lon, lat=nr.lat, gridshp = brick.raster.smiref, radius=myrad)
  #my.dim.ls[i]<-(dim(rad.loc))[2]
  temp.radsmi.df<-data.frame(period=time.st.smiref[,1] , smi=t(rad.loc[,-(1:2)]))
  temp.mean.df<-data.frame(period=temp.radsmi.df$period, 
                           smi.radmeans=rowMeans(temp.radsmi.df[,-1]), 
                           eic=plz.lonlat.ls[[i]][,1],
                           plz=plz.lonlat.ls[[i]][,2], 
                           city=plz.lonlat.ls[[i]][,3],
                           regio=plz.lonlat.ls[[i]][,4],
                           lat=nr.lat, 
                           lon=nr.lon)
  radsmiref.ls[[i]]<-temp.mean.df
}
length(radsmiref.ls) # 106 by each EIC code 
radsmiref.df<- dplyr::bind_rows(radsmiref.ls[1:length(radsmiref.ls)])
dim(radsmiref.df) # check: 63600

# Add time periods to the df:
radsmiref.df<-left_join(radsmiref.df, timedate.df, by=c("period"="period"))
# Aggregate smi means for each location for each month - receive average monthly
# values for each location between the given time interval
smiref.stat.df<-plyr::ddply(radsmiref.df, .(mon,eic,plz,city,lat,lon), summarise, smi.monmeans=mean(smi.radmeans, na.rm = T))

inspect.2.1<-left_join(fuel.smi.out.stat.df, smiref.stat.df, by=c("mon"="mon","eic"="eic","city"="city","plz"="plz","lat"="lat","lon"="lon"))
#dim(inspect.2.1) #1764
inspect.2.1<-dplyr::filter(inspect.2.1, smi.radmeans <= smi.monmeans)
#dim(inspect.2.1) #1385

#http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software
#http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/78-perfect-scatter-plots-with-correlation-and-marginal-histograms/

##################################################################
#########                  AGGREGATE                   ###########
##################################################################

# Calculate the occurrence of outages per smi step:
step<-0.05
smi.step<-seq(0,1,by=step)

temp.out.smi.ls.1<-list()
for (i in 1:(length(smi.step)-1)) {
  temp.out.1<-dplyr::filter(inspect.1, inspect.1$smi.radmeans>=smi.step[i] & inspect.1$smi.radmeans<=smi.step[i+1])
  temp.out.1$step<-smi.step[i]
  temp.out.smi.ls.1[[i]]<-plyr::ddply(temp.out.1, .(step), summarise, sum.freq=sum(freq, na.rm = T))
}
temp.out.smi.df.1<- dplyr::bind_rows(temp.out.smi.ls.1[1:length(temp.out.smi.ls.1)])

# Calculate the occurrence of outages per smi step and per months:
temp.out.smi.ls.2<-list()
for (i in 1:(length(smi.step)-1)) {
  temp.out.2<-dplyr::filter(inspect.1, inspect.1$smi.radmeans>=smi.step[i] & inspect.1$smi.radmeans<=smi.step[i+1])
  temp.out.2$step<-smi.step[i]
  temp.out.smi.ls.2[[i]]<-ddply(temp.out.2, .(step,mon), summarize, sum.freq=sum(freq, na.rm = T))
}
temp.out.smi.df.2<- dplyr::bind_rows(temp.out.smi.ls.2[1:length(temp.out.smi.ls.2)])

##################################################################
#write.csv(temp.out.smi.df.2, file="./data_outage/outages_per_month_and_smi_2019.csv")
##################################################################

# Calculate the occurrence of outages per smi step and per months, comparing to
# smi values for this location from 1951 to 2000 (if it is below ref smi for this months)
temp.out.smi.ls.3<-list()
for (i in 1:12) {
  temp.out.3<-dplyr::filter(inspect.2.1, inspect.2.1$smi.radmeans>=smi.step[i] & inspect.2.1$smi.radmeans<=smi.step[i+1])
  temp.out.3$step<-smi.step[i]
  temp.out.smi.ls.3[[i]]<-plyr::ddply(temp.out.3, .(step,mon), summarize, sum.freq=length(eic))
}
temp.out.smi.df.3<- dplyr::bind_rows(temp.out.smi.ls.3[1:length(temp.out.smi.ls.3)])
temp.out.smi.df.3<-temp.out.smi.df.3[,c("step","mon","sum.freq")]

##################################################################
#write.csv(temp.out.smi.df.3, file="./data_outage/outages_per_month_and_smi_REF_2019.csv")
##################################################################

##################################################################
#########                    PLOTS                     ###########
##################################################################

#svg("./plots/freq_out_line.svg")
ggplot2::ggplot(temp.out.smi.df.1, aes(step, sum.freq)) +
  geom_line() + theme_classic() + 
  labs(title="Frequency of outages per smi range",
       x="smi range", y = "Frequency of outages (number of occurences)")
#dev.off()

# Bar plot of outages per smi value
#svg("./plots/freq_out_bar.svg")
ggplot2::ggplot(temp.out.smi.df.2) +
  geom_bar(aes(x=as.factor(step), y=sum.freq, fill=as.factor(mon)), stat="identity") + 
  theme_classic() + 
  scale_fill_brewer(palette="PRGn") + 
  labs(title="Frequency of outages per smi range",
       x= paste("smi range with step", step, sep = " "), 
       y = "Frequency of outages (number of occurences)",
       fill="months")
#dev.off()

ggplot2::ggplot(temp.out.smi.df.3, 
                aes(x=as.factor(step), y=sum.freq, 
                    fill=as.factor(mon))) +
  geom_bar(stat="identity") + 
  theme_classic() + 
  scale_fill_brewer(palette="PRGn") + 
  labs(title="Frequency of outages per smi range",
       x= paste("smi range with step", step, sep = " "), 
       y = "Frequency of outages (number of occurences)",
       fill="months")

##################################################################
#########                 INSPECTION 3                 ###########
##################################################################
inspect.3.0<-fuel.smi.out.stat.df
dim(inspect.3.0) # check: 10973
# sum frequency of outages per plz
inspect.3.1<-ddply(inspect.3.0, .( year, mon, smi.radmeans, plz, city, regio, lat, lon), 
                   summarize, sum.freq=length(eic))

length(unique(inspect.3.1$plz)) # check: 81

key.inspect.3<-data.frame(plz=inspect.3.1[!duplicated(inspect.3.1$plz),"plz"])
dim(key.inspect.3)  # check: 81
# these PLZs have least number of data incidents. Not enough for correlation (if any).
key.inspect.3<-subset(key.inspect.3, !plz %in% c("0"))
dim(key.inspect.3)  # 81

# In the Pearson correlation p-value us the significance level
# it has to be below the 0.05 to conclude the correlation 
# coefficient "c" is significant.

cor.inspect.3.ls<-list()
for (i in 1:dim(key.inspect.3)[1]) {
  inspect.3.3<-dplyr::filter(inspect.3.1, plz==key.inspect.3[i,])
  inspect.3.4<-dplyr::select(inspect.3.3[1,], c("plz","city","regio","lat","lon"))
  
  inspect.3.4$pearson.c<-as.numeric(cor.test(inspect.3.3$mon, inspect.3.3$sum.freq, method = c("pearson"))[4])
  inspect.3.4$pearson.p<-as.numeric(cor.test(inspect.3.3$mon, inspect.3.3$sum.freq, method = c("pearson"))[3])
  cor.inspect.3.ls[[i]]<-inspect.3.4
}
cor.inspect.3.df<- dplyr::bind_rows(cor.inspect.3.ls[1:length(cor.inspect.3.ls)])
# no significant correlations in this mode
##################################################################

##################################################################
#file.name.2<-paste("./data_outage/cor_inspect_2019.csv", sep="")
#write.csv(cor.inspect.3.df, file=file.name.2)
##################################################################

##################################################################
test.plz<-85406
test.pp<-dplyr::filter(fuel.smi.out.stat.df, fuel.smi.out.stat.df$plz == test.plz)
inspect.3.3<-dplyr::filter(inspect.3.1, plz==test.plz)
cor.test(inspect.3.3$mon, inspect.3.3$sum.freq, method = c("pearson"))
cor.test(inspect.3.3$mon, inspect.3.3$relat, method = c("pearson"))

# plot the correlation
ggpubr::ggscatter(inspect.3.3, x = "mon", y = "sum.freq", 
                  color = "mon", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Month", ylab = "Number of outages") 
#png(paste("./plots/corr_out_smi_",test.plz,".png", sep=""))

##################################################################


##################################################################
# SUM UP OUTAGES IN REGIONS PER LAT/LON WITHIN THE REQUIRED DIAMETER
##################################################################
# sum up the closest locations to increase the number of samples
# radius = 2*pi*radius
# ( x − m ) 2 + ( y − n ) 2 = r 2     circle with centre S=(m,n) 

coor<-unique(inspect.3.1[,c("lat","lon")])
lon<-inspect.3.1[23,c("lon")]
lat<-inspect.3.1[23,c("lat")]
rad<-1
# function that takes the lat/lon coordinates in the given radius
radiusextractloc<-function(lon,lat,coor,rad){
  coor$dist<-sqrt( sqrt(abs(coor[,1] - lat)) + sqrt(abs(coor[,2]-lon)) )
  coorpar<-dplyr::filter(coor, dist<rad)
  return(xf=coorpar[c(1,2)])
}
xxxx<-radiusextractloc(lon=lon,lat=lat,coor=coor,rad=rad)
xxxx
# distVincentyEllipsoid gives distance in meters between two lat/lon points
distVincentyEllipsoid(xxxx[1,c(1,2)], xxxx[2,c(1,2)])/1000
inspect.3.5<-dplyr::filter(inspect.3.1, lat %in% xxxx[,1] &  lon %in% xxxx[,2] )
# plot scatter
ggpubr::ggscatter(inspect.3.5, x = ("mon"), y = "sum.freq", 
                  #color = "loc.mean", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson"     ) 
##################################################################


##################################################################

##########     stats per fuel type of power plant       ###########
# make a dataframe extract on demand
extract.pp.df<-fuel.smi.out.stat.df

# create dataframes for fuel types
coal.out.df<-extract.pp.df[extract.pp.df$fuel=="coal",]
dim(coal.out.df) # 1062
coal.out.df<-left_join(coal.out.df, eic.fuel.df[,c("peic","in_cap")], by=c("eic"="peic"))
length(unique(coal.out.df$eic)) # 44 coal power plants with outages
coal.unq<-coal.out.df[!duplicated(coal.out.df$eic), c("eic", "lon", "lat", "fuel","in_cap")]
dim(coal.unq) #44

gas.out.df<-extract.pp.df[extract.pp.df$fuel=="gas",]
dim(gas.out.df) # 436
gas.out.df<-left_join(gas.out.df, eic.fuel.df[,c("peic","in_cap")], by=c("eic"="peic"))
length(unique(gas.out.df$eic)) # 28 gas power plants with outages
gas.unq<-gas.out.df[!duplicated(gas.out.df$eic), c("eic", "lon", "lat", "fuel","in_cap")]
dim(gas.unq) #28

lignite.out.df<-extract.pp.df[extract.pp.df$fuel=="lignite",]
dim(lignite.out.df) # 281
lignite.out.df<-left_join(lignite.out.df, eic.fuel.df[,c("peic","in_cap")], by=c("eic"="peic"))
length(unique(lignite.out.df$eic)) # 11 lignite power plants with outages
lignite.unq<-lignite.out.df[!duplicated(lignite.out.df$eic), c("eic", "lon", "lat", "fuel")]
dim(lignite.unq) #11

nuc.out.df<-extract.pp.df[extract.pp.df$fuel=="nuc",]
dim(nuc.out.df) # 79
nuc.out.df<-left_join(nuc.out.df, eic.fuel.df[,c("peic","in_cap")], by=c("eic"="peic"))
length(unique(nuc.out.df$eic)) # 8 nuc power plants with outages
nuc.unq<-nuc.out.df[!duplicated(nuc.out.df$eic), c("eic", "lon", "lat", "fuel")]
dim(nuc.unq) #8

ps.out.df<-extract.pp.df[extract.pp.df$fuel=="ps",]
dim(ps.out.df) # 154
ps.out.df<-left_join(ps.out.df, eic.fuel.df[,c("peic","in_cap")], by=c("eic"="peic"))
length(unique(ps.out.df$eic)) # 10 pump storages power plants with outages
ps.unq<-ps.out.df[!duplicated(ps.out.df$eic), c("eic", "lon", "lat", "fuel")]
dim(ps.unq) #9

# create one for all

# create dataframes for fuel types
all.pp.df<-extract.pp.df[extract.pp.df$fuel==c("coal","lignite","gas","nuc","ps"),]
dim(all.pp.df) # 402 16
all.pp.df<-left_join(all.pp.df, eic.fuel.df[,c("peic","in_cap")], by=c("eic"="peic"))
length(unique(all.pp.df$eic)) # 87 ower plants with outages
all.unq<-all.pp.df[!duplicated(all.pp.df$eic), c("eic", "lon", "lat", "fuel","in_cap")]
dim(all.unq) #87
##################################################################


# COR TEST
# p-value of the test should be less than the significance level alpha = 0.05.
# Then they are significantly correlated with a correlation coefficient cor
# WE ARE INTERESTED IN NEGATIVE CORRELATION COEF- IF X INCREASES, Y - DECREASES AND VICE VERSA


# SHAPIRO TEST
# if p-values are greater than the significance level 0.05 - 
# the distribution of the data are not significantly different from normal distribution.


##################################################################
#########          Plot power plants outages           ###########
##################################################################

#########               make the raster                ###########
# build my power plants against the rater of smi (take averages for smi at each location?)
#########                CHOOSE YEAR                   ###########
FromYear<-2010
ToYear<-2018
Mon<-6
##################################################################
# Reload the initial values to lon/lat
lon<-ncdf4::ncvar_get(nc_ufz, "lon")
lat<-ncdf4::ncvar_get(nc_ufz, "lat")
smi<-ncdf4::ncvar_get(nc_ufz, "SMI")

time.st<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear & year <=ToYear & mon == Mon ), c("period"))
time.vec<-as.vector(time.st[,1])
#dim(smi)
# Choose which time slice to graph
smi.raster<-smi[,,time = time.vec]
dim(smi.raster)
smi.raster<-as.array(smi.raster)
# TAKE MEANS AT EACH LOCATION FOR smi SINCE FromYear
smi.raster.mean<-as.array(rowMeans(smi.raster, dim=2))
dim(smi.raster.mean)

slice.raster <- raster(t(smi.raster.mean), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster <- flip(slice.raster, direction='y')

##################################################################
# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
r.spdf <- as(slice.raster, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
head(r.df)

#display.brewer.all()
pal<-colorRampPalette(brewer.pal(10, 'Paired'))(dim(r.df)[1])
palSz <- dim(r.df)[1] 

#file.name.3<-paste("./plots/", "out_drght_" , FromYear, "-", ToYear, ".svg", sep="")
#svg(file.name.3,  width = 500, height = 500)
ggplot(r.df, aes(x=x, y=y)) + 
  geom_tile(aes(fill = layer)) + 
  scale_fill_gradient2( name=paste("SMI index", sep=""),
                        low=pal[which(r.df[,"layer"]==max(r.df[,"layer"], na.rm = T))],
                        mid = pal[palSz/2],
                        high = pal[which(r.df[,"layer"]==min(r.df[,"layer"], na.rm = T))],
                        midpoint = ((max(r.df[,"layer"], na.rm = T) + min(r.df[,"layer"], na.rm = T)) / 2)) +
  # add size (installes MW) and powe rplant type
  new_scale("fill") +
  geom_jitter(data=all.unq, aes(x=lon, y=lat, col=fuel, size=in_cap),width = .1 )+
  scale_color_manual(name=paste("Capacity type", sep=""),
                     values=c("coal"="brown4",
                              "lignite"="firebrick3",
                              "gas"="dodgerblue4",
                              "nuc"="goldenrod2",
                              "ps"="cadetblue1"))+
  labs(x="", y="", title=paste("Power plant phase-outs vs SMI index"),
       subtitle = paste("SMI is average for month #",Mon," between ",FromYear,"-",ToYear, sep="")) +
  guides(size=guide_legend(title=paste("Installed","\n","cap [MW]", sep="")),
         col = guide_legend(override.aes = list(linetype = 0, size=5)))+
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
To99<-as.Date("1999-12-01")

From00<-as.Date("2000-01-01")
To18<-as.Date("2018-12-01")

#create time stamp
timedate.df<-data.frame(date=seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months"))
timedate.df$period<-seq(1,dim(timedate.df)[1], by=1)
##################################################################

time51_99.st<-dplyr::select(dplyr::filter(timedate.df, date >= From51 & date <=To99 ), c("period"))
time00_18.st<-dplyr::select(dplyr::filter(timedate.df, date >= From00 & date <=To18 ), c("period"))

# choose which time slice to graph
smi.raster51_99<-smi[,,time51_99.st$period]
dim(smi.raster51_99)
smi.raster00_18<-smi[,,time00_18.st$period]
dim(smi.raster00_18)

smi.raster51_99<-as.array(smi.raster51_99)
smi.raster00_18<-as.array(smi.raster00_18)

smi.raster.mean51_99<-as.array(rowMeans(smi.raster51_99, dim=2))
smi.raster.mean00_18<-as.array(rowMeans(smi.raster00_18, dim=2))

smi.raster.min51_99<-as.array(apply(smi.raster51_99, c(1,2), min, na.rm=F))
smi.raster.min00_18<-as.array(apply(smi.raster00_18, c(1,2), min, na.rm=F))

# Compare the mean of 00-18 to the mean 51-99 
smi.raster.ratio<-(smi.raster.mean00_18/smi.raster.mean51_99-1)*100
slice.raster.comp.rat <- raster(t(smi.raster.ratio), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.comp.rat <- flip(slice.raster.comp.rat, direction='y')
r.spdf.r <- as(slice.raster.comp.rat, "SpatialPixelsDataFrame")
r.df.r <- as.data.frame(r.spdf.r)

# if min 51-99 is greater than minimum 00-18 (less dry), then show 00-18, else - 0 (these places were more precipitated)
smi.raster.comp.min<-ifelse(smi.raster.min51_99>=smi.raster.min15_18, smi.raster.min00_18, 0 )
slice.raster.comp.min <- raster(t(smi.raster.comp.min), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.comp.min <- flip(slice.raster.comp.min, direction='y')
r.spdf.m <- as(slice.raster.comp.min, "SpatialPixelsDataFrame")
r.df.m <- as.data.frame(r.spdf.m)


# if mean 51-99 is greater than mean 00-18 (less dry), then show 00-18, else - 0 (these places became more precipitated).
smi.raster.compared<-ifelse(smi.raster.mean51_99>=smi.raster.mean00_18, smi.raster.mean00_18, 0 )
# TAKE MEANS AT EACH LOCATION FOR smi SINCE FromYear
slice.raster.compared <- raster(t(smi.raster.compared), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.compared <- flip(slice.raster.compared, direction='y')
r.spdf.c <- as(slice.raster.compared, "SpatialPixelsDataFrame")
r.df.c <- as.data.frame(r.spdf.c)

# Check that the rasters are different! Should be FALSE!
identical(smi.raster.compared, slice.raster.compared)


ggplot(r.df.r, aes(x=x, y=y)) + 
  geom_tile(aes(fill = layer)) + 
  scale_fill_gradient2( name=paste("SMI change [%]", sep=""),
                        low = "red",
                        mid = "antiquewhite1",
                        high = "green3",
                        midpoint = 0,
                        limits = c(-50, 50))+
  # add size (installes MW) and powe rplant type
  new_scale("fill") +
  geom_jitter(data=all.unq, aes(x=lon, y=lat, col=fuel, size=in_cap),width = .1 )+
  scale_color_manual(name=paste("Capacity type", sep=""),
                     values=c("coal"="brown4",
                              "lignite"="firebrick3",
                              "gas"="dodgerblue4",
                              "nuc"="orange3",
                              "ps"="lightslateblue"))+
  labs(x="", y="")+
  guides(size=guide_legend(title=paste("Installed","\n" ,"capacity [MW]", sep="")),
         col = guide_legend(override.aes = list(linetype = 0, size=5)))+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")
#file.name<-paste("./plots/", "data_tmp_smi_out" , FromYear, "-", ToYear, ".svg", sep="")
#ggsave(file.name,  width = 500, height = 500)
##################################################################
