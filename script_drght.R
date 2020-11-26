# Script...

# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "plyr","raster","ncdf4","ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "pastecs",
           "CommEcol", "fBasics", "EnvStats", "geosphere", "ggpmisc",
           "lubridate","tidyquant", "gridExtra", "tidyverse", "viridis",
           "ts", "strucchange", "fxregime", "zoo")
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

#########         Read the .ncdf with SMI              ###########
# Open .ncdf file with drought data:
nc_ufz<-ncdf4::nc_open("./data_ufz/1951-2018.nc")
# Use nc_open to read the data into a data structure I called nc_data. 
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
# Other pertinent information about the NDVI variable: Lets’s see what fill 
# value was used for missing data:
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
##################################################################
# Make the sequence of time stamps from scratch
years<-as.data.frame(stringr::str_split_fixed(seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months"), "-", 3))
years$V1<-as.numeric(as.character(years$V1))
years$V2<-as.numeric(as.character(years$V2))
years$V3<-as.numeric(as.character(years$V3))
sapply(years, class)
timedate.df<-data.frame(period=seq(1,816,1), year=years[,1], mon=years[,2], date=seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months") )
##################################################################

#########               Brick raster                   ###########
# Raster brick, contrary to just raster, gets timeseries at locations.
# Replace all those fill values with the R-standard ‘NA’.
smi[smi==fillvalue$value]<-NA
dim(smi) # 175 225 816
# Choose which time slice to work with:
time.st<-dplyr::select(dplyr::filter(timedate.df, year >= year(FromYear)), c("period"))
time.vec<-as.vector(time.st[,1])
#length(time.vec)
# slice the smi file to have the number of periods on your demand FromYear:
smi.slice<-smi[, , time = time.vec]
dim(smi.slice) # [ dim lat, dim lon, dim time periods ]

# Convert the entire 3d array of data to a raster brick:
brick.raster<-brick(smi.slice,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(brick.raster)
# Transpose (the t() function) and flip() before the data are oriented correctly:
brick.raster<-flip(t(brick.raster), direction = "y")
#res(brick.raster)
##################################################################

#########        Brick raster for reference smi        ###########
##################################################################
#########                CHOOSE YEAR                   ###########
FromYear.smi<-1951  # year from which we have smi data
ToYear.smi<-2018    # any reference year we need
##################################################################

# choose which time slice to work with
time.st.smiref<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear.smi & year <= ToYear.smi), c("period"))
time.vec.smiref<-as.vector(time.st.smiref[,1])
#length(time.vec)
# slice the smi file to have the number of periods on your demand FromYear
smiref.slice.<-smi[, , time = time.vec.smiref]
dim(smiref.slice.) # [ dim lat, dim lon, dim time periods ]

# raster brick, contrary to just raster, gets timeseries at locations
# convert the entire 3d array of data to a raster brick
brick.raster.smiref<-brick(smiref.slice.,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(brick.raster.smiref) # 175 225 540
# transpose (the t() function) and flip() before the data are oriented correctly
brick.raster.smiref<-flip(t(brick.raster.smiref), direction = "y")
#res(brick.raster)
##################################################################


##################################################################
#             Plot SMI time series for locations  
#             And long-term smi trend unsing SMA
##################################################################
# aggregate the selected smi by taking average for region
aggregate.smi<-ddply(radsmi.all.df, .(period, regio), summarize, regio.mean=mean(smi.radmeans) )
aggregate.smi<-left_join(aggregate.smi, timedate.df, by=c("period"="period"))

aggregate.smi.range<-ddply(radsmi.all.df, .(period), summarize, max=max(smi.radmeans), min=min(smi.radmeans), mean=mean(smi.radmeans))
aggregate.smi.range<-left_join(aggregate.smi.range, timedate.df, by=c("period"="period"))
aggregate.smi.range.j<-subset(aggregate.smi.range, mon==6)
aggregate.smi.range.d<-subset(aggregate.smi.range, mon==12)

sma_shift<-12
aggregate.smi.range$minsma<-SMA(aggregate.smi.range$min, n = sma_shift)
aggregate.smi.range$maxsma<-SMA(aggregate.smi.range$max, n = sma_shift)
aggregate.smi.range$meansma<-SMA(aggregate.smi.range$mean, n = sma_shift)

#aggregate.smi.range$regyear<-ifelse(aggregate.smi.range$year>=2006, "After 2006", "Before 2006") 
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
#ggsave("./plots/drought/SMI_timeseries.svg")

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

##################################################################
#           Check SMI time series for structural break  
#             
##################################################################
start.y<-min(aggregate.smi.range$date)
end.y<-max(aggregate.smi.range$date)

aggregate.smi.ranges<-subset(aggregate.smi.range, year >= 1951 & year <= 2018)

aggregate.smi.ts<-ts(aggregate.smi.ranges$mean, start=c(1951,01,01),frequency=12)
#aggregate.smi.ts<-zoo(aggregate.smi.range$mean, aggregate.smi.range$date)
glimpse(aggregate.smi.ts)
plot(aggregate.smi.ts)

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

#library("fitdistrplus")
#fitdist(radsmi.ls[[1]][,2], "lnorm" )

##################################################################
#########             joining databases I              ###########
##################################################################
# add years and months to smi time seried additionally to periods
dim(radsmi.all.df) # 
smi.ts.df<-inner_join(timedate.df, radsmi.all.df, by=c("period"="period"))
dim(smi.ts.df) # check:
length(unique(smi.ts.df$plz)) #81
length(unique(smi.ts.df$eic))  # check: 106 unique eics

dim(out.ts.df) # check:
length(unique(out.ts.df$production_RegisteredResource.mRID)) # 106
# join rad smi timeseries and outage time series
smi.out.stat.df<-left_join(smi.ts.df, out.ts.df, by=c("eic" = "production_RegisteredResource.mRID", "year"="year", "mon"="st.mon", "city"="city", "plz"="plz" ))
smi.out.stat.df$ratio<-NULL
dim(smi.out.stat.df) # check: 
##################################################################


##################################################################
#########             joining databases II             ###########
##################################################################
# add fuel types
fuel.smi.out.stat.df<-left_join(smi.out.stat.df, extract.eic.fuel.df, by=c("eic"="peic"))
dim(fuel.smi.out.stat.df) # 5088
sum(is.na(fuel.smi.out.stat.df$fuel)) # should be 0
unique(fuel.smi.out.stat.df$fuel)
##################################################################
file.name.1<-paste("./data_outage/smi_out_stat_ts_2019.csv" )
#write.csv(fuel.smi.out.stat.df, file=file.name.1)
##################################################################


##################################################################
#########     buld distribution and frequency of smi   ###########
#########          1951-1995   and   1995-2018         ###########
##################################################################
length(unique(fuel.smi.out.stat.df$plz)) #81
length(unique(smi.ts.df$plz)) #81

# add a period marker before and after 1995
smi.ts.df$marker<- ifelse(smi.ts.df$year <= 1995, "Before 1995", "After 1995")

pplt.1<- ggplot(dplyr::filter(smi.ts.df, marker=="Before 1995"), 
                aes(x=smi.radmeans))+
  geom_density(aes(y=..density..*(5150)), alpha=0.7, fill="grey", col="grey") +   
  geom_histogram(bins = 12, alpha=0.5, position = "identity", fill="firebrick1") + 
  scale_y_continuous("Frequency",  limits=c(0, 7000),
                     sec.axis = sec_axis(~ . / 5150, name = "Probability density", labels = scales::comma) )+
  theme_minimal() + labs( x="SMI")
#pplt.1
pplt.2<- ggplot( dplyr::filter(smi.ts.df, marker=="After 1995"), 
                 aes(x=smi.radmeans))+
  geom_density(aes(y=..density..*(2600)), alpha=0.5,  fill="grey", col="grey") +
  geom_histogram(bins = 12, alpha=0.5, position = "identity",  fill="darkolivegreen1" )+
  scale_y_continuous("",  limits=c(0, 7000),
                     sec.axis = sec_axis(~ . / 2600, name = "Probability density", labels = scales::comma) )+
  theme_minimal() + labs( x="SMI")
#pplt.2
grid.arrange(pplt.1, pplt.2, nrow = 1)




values.1<-dplyr::filter(smi.ts.df, marker=="Before 1995")
values.2<-dplyr::filter(smi.ts.df, marker=="After 1995")

breaks <- seq(0, 1, by=0.025)

bs.1 <- hist(values.1$smi.radmeans, breaks=breaks, plot=F)$breaks
probs.1 <- table(cut(values.1$smi.radmeans, bs.1)) / length(values.1$smi.radmeans)
probs.1<- as.data.frame(probs.1)
#barplot(probs.1, ylab="Probability", las=2)

bs.2 <- hist(values.2$smi.radmeans, breaks=breaks, plot=F)$breaks
probs.2 <- table(cut(values.2$smi.radmeans, bs.1)) / length(values.2$smi.radmeans)
probs.2<- as.data.frame(probs.2)
#barplot(probs.2, ylab="Probability", las=2)

bs<-data.frame(x1=breaks, x2=seq(0.025, 1.025, by=0.025))
bs$labels<-paste(bs$x1, bs$x2, sep="-")
bs<-bs[1:(length(bs$x1)-1),]
bs$'Before 1995'<-probs.1[,2]
bs$'After 1995'<-probs.2[,2]

colors<-c("Before 1995" = "red", "After 1995" = "green3", "Intercept" = "black")

ggplot(bs, aes(x=labels))+
  geom_bar( aes(y=`Before 1995`, fill="Before 1995"), alpha=0.5, 
            stat="identity") +
  geom_bar(aes(y=`After 1995`,  fill="After 1995"), alpha=0.5,
           stat="identity") +
  scale_x_discrete(labels=bs$labels)+
  theme_minimal_hgrid() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))+
  labs(y="Probability", x="SMI", fill = "") +
  scale_fill_manual(values = colors)


ggplot()+
  geom_bar(data=probs.1, fill="firebrick3", alpha=0.5, 
           aes(x=Var1,y=Freq),stat="identity") +
  geom_bar(data=probs.2, fill="darkolivegreen3", alpha=0.5,
           aes(x=Var1,y=Freq),stat="identity") +
  scale_x_discrete(labels=bs$labels)+
  theme_minimal_hgrid() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))+
  labs(y="Probability", x="SMI", fill = "Legend") +
  scale_fill_manual(values = colors)


ggplot(smi.ts.df, aes(x=smi.radmeans, fill=marker, color=marker), aplha=0.3)+
  geom_histogram(bins = 50, alpha=0.3,
                 position = "identity"
                 , aes(y = ..density..) )+
  scale_y_continuous("Probability Density")   +
  scale_fill_manual(name="", values = colors, labels=c("After 1995" ,"Before 1995", "Intercept")) +
  scale_color_manual(name="",values = colors, labels=c("After 1995" ,"Before 1995", "Intercept")) +
  labs(x="SMI", fill="") +
  theme_minimal()+
  theme(legend.position = c(0.1, 0.95))


###############################################################


##################################################################
#########               inspect outages                ###########
##################################################################
#########                 INSPECTION 1                 ###########
# make a simple cor test
inspect.1<-dplyr::filter(fuel.smi.out.stat.df, !is.na(fuel.smi.out.stat.df$freq) )
inspect.1$relative<-inspect.1$freq*inspect.1$max
cor.test.1<-cor.test(inspect.1$smi.radmeans, inspect.1$mean, method = c("pearson"))

# plot the correlation
ggpubr::ggscatter(inspect.1, x = "smi.radmeans", y = "freq", 
                  color = "fuel", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "smi", ylab = "mon") 

##################################################################
#########                 INSPECTION 2                 ###########
##################################################################
unq.plz<-unique(fuel.smi.out.stat.df$plz)
# 
inspect.2<-dplyr::filter(fuel.smi.out.stat.df, 
                         #!is.na(fuel.smi.out.stat.df$freq) &
                          fuel.smi.out.stat.df$year>=2015 &
                          fuel.smi.out.stat.df$smi.radmeans<=1  
                         #& fuel.smi.out.stat.df$plz==unq.plz[34]
                         )
# calculate relative value to stress both the frequency and the mean outage value
inspect.2$relative<-round(log(inspect.2$mean*inspect.2$freq), 5)
#duplitcated.relative<-unique(inspect.2[duplicated(inspect.2$relative), "relative"])
# removing frequencies of outages that seem to occur regularly
#inspect.2<-dplyr::filter(inspect.2, inspect.2$relative!=0.00000)
#inspect.2<-dplyr::filter(inspect.2, inspect.2$relative!=0.69315) 
#inspect.2<-dplyr::filter(inspect.2, inspect.2$relative!=1.60944)
cor.test.2<-cor.test(inspect.2$smi.radmeans, inspect.2$relative, method = c("pearson"), na.action="na.exclude")

# plot the correlation
#svg("./plots/outage_scatter.svg")
ggscat<-ggpubr::ggscatter(inspect.2, x = "smi.radmeans", y = "relative", 
                          color = "mon", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "monthly smi value", ylab = "relative index of outages: ln(frequency × mean of outages per month) ") +
  theme(legend.position="right") +
  scale_color_gradient2(low = "green4", mid = "white",
                        high = "purple4", midpoint = 7) +
  labs(color="months")
ggExtra::ggMarginal(ggscat, type = "histogram", margins = "x")

#dev.off()
##################################################################

##################################################################
#########                 INSPECTION 2.1               ###########
##################################################################
# get reference smi to filter out every smi below average for that month over 1951-1995

#########        brick raster for reference smi        ###########
# raster brick, contrary to just raster, gets timeseries at locations
FromYear.smi<-1951
ToYear.smi<-1995
time.st.smiref<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear.smi & year <= ToYear.smi), c("period"))
time.vec.smiref<-as.vector(time.st.smiref[,1])
smiref.slice.<-smi[, , time = time.vec.smiref]
dim(smiref.slice.) # [ dim lat, dim lon, dim time periods ]
brick.raster.smiref<-brick(smiref.slice.,xmn=min(lat),xmx=max(lat),ymn=min(lon),ymx=max(lon),
                         crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
brick.raster.smiref<-flip(t(brick.raster.smiref), direction = "y")
# define my desired radius and check with spDistsN1 below
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
dim(radsmiref.df) # check: 57240

# add time periods to the df
radsmiref.df<-left_join(radsmiref.df, timedate.df, by=c("period"="period"))
# aggregate smi means for each location for each month - receive average monthly 
# values for each location between the given time interval
smiref.stat.df<-plyr::ddply(radsmiref.df, .(mon,eic,plz,city,lat,lon), summarise, smi.monmeans=mean(smi.radmeans, na.rm = T))

inspect.2.1<-left_join(inspect.2, smiref.stat.df, by=c("mon"="mon","eic"="eic","city"="city","plz"="plz","lat"="lat","lon"="lon"))
#dim(inspect.2.1) #1764
inspect.2.1<-dplyr::filter(inspect.2.1, smi.radmeans <=smi.monmeans)
#dim(inspect.2.1) #1385
#http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software
#http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/78-perfect-scatter-plots-with-correlation-and-marginal-histograms/



##################################################################
#########                  AGGREGATE                   ###########
##################################################################

# calculate the occurence of outages per smi step
step<-0.05
smi.step<-seq(0,1,by=step)

temp.out.smi.ls.1<-list()
for (i in 1:(length(smi.step)-1)) {
  temp.out.1<-dplyr::filter(inspect.2, inspect.2$smi.radmeans>=smi.step[i] & inspect.2$smi.radmeans<=smi.step[i+1])
  temp.out.1$step<-smi.step[i]
  temp.out.smi.ls.1[[i]]<-plyr::ddply(temp.out.1, .(step), summarise, sum.freq=sum(freq, na.rm = T))
}
temp.out.smi.df.1<- dplyr::bind_rows(temp.out.smi.ls.1[1:length(temp.out.smi.ls.1)])

# calculate the occurence of outages per smi step and per months
temp.out.smi.ls.2<-list()
for (i in 1:(length(smi.step)-1)) {
  temp.out.2<-dplyr::filter(inspect.2, inspect.2$smi.radmeans>=smi.step[i] & inspect.2$smi.radmeans<=smi.step[i+1])
  temp.out.2$step<-smi.step[i]
  temp.out.smi.ls.2[[i]]<-ddply(temp.out.2, .(step,mon), summarize, sum.freq=sum(freq, na.rm = T))
}
temp.out.smi.df.2<- dplyr::bind_rows(temp.out.smi.ls.2[1:length(temp.out.smi.ls.2)])

##################################################################
#write.csv(temp.out.smi.df.2, file="./data_outage/outages_per_month_and_smi_2019.csv")
##################################################################

# calculate the occurence of outages per smi step and per months, comparing to
# smi values for this location from 1951 to 1995 (if it is below ref smi for this months)
temp.out.smi.ls.3<-list()
for (i in 1:12) {
  temp.out.3<-dplyr::filter(inspect.2.1, inspect.2.1$smi.radmeans>=smi.step[i] & inspect.2.1$smi.radmeans<=smi.step[i+1])
  temp.out.3$step<-smi.step[i]
  temp.out.smi.ls.3[[i]]<-plyr::ddply(temp.out.3, .(step,mon), summarize, sum.freq=sum(freq, na.rm = T))
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

# bar plot of outages per smi value
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
inspect.3.0<-dplyr::filter(fuel.smi.out.stat.df, !is.na(fuel.smi.out.stat.df$freq) )
dim(inspect.3.0) # check: 2036
# sum frequency of outages per plz
inspect.3.1<-ddply(inspect.3.0, .( year, mon, smi.radmeans, plz, city, regio, lat, lon), 
                   summarize, sum.freq=sum(freq, na.rm = T), loc.mean=mean(mean, na.rm=T))
# count relative value of 
inspect.3.1$relat=round(log(inspect.3.1$loc.mean*inspect.3.1$sum.freq), 5)

dim(inspect.3.1) # check: 2320

length(unique(inspect.3.1$plz)) # check: 80

key.inspect.3<-data.frame(plz=inspect.3.1[!duplicated(inspect.3.2$plz),"plz"])
dim(key.inspect.3)  #80
# these PLZs have least number of data incidents. Not enouth for correlation (if any).
key.inspect.3<-subset(key.inspect.3, !plz %in% c("0"))
# dim(key.inspect.3)  #75

# in the pearson correlatipn p-value us the significance level
# it has to be below the alpha (0.05) to conclude the correlation 
# coefficient "c" is significant.

cor.inspect.3.ls<-list()
for (i in 1:dim(key.inspect.3)[1]) {
  inspect.3.3<-dplyr::filter(inspect.3.1, plz==key.inspect.3[i,])
  inspect.3.4<-dplyr::select(inspect.3.3[1,], c("plz","city","regio","lat","lon"))
 
  inspect.3.4$pearson.c<-as.numeric(cor.test(inspect.3.3$mon, inspect.3.3$relat, method = c("pearson"))[4])
  inspect.3.4$pearson.p<-as.numeric(cor.test(inspect.3.3$mon, inspect.3.3$relat, method = c("pearson"))[3])
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
test.plz<-47179
test.pp<-dplyr::filter(fuel.smi.out.stat.df, fuel.smi.out.stat.df$plz == test.plz)
inspect.3.3<-dplyr::filter(inspect.3.2, plz==test.plz)
cor.test(inspect.3.3$mon, inspect.3.3$sum.freq, method = c("pearson"))
cor.test(inspect.3.3$mon, inspect.3.3$relat, method = c("pearson"))

# plot the correlation
ggpubr::ggscatter(inspect.3.3, x = "mon", y = "sum.freq", 
                  color = "mon", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "smi", ylab = "outages") 

#png(paste("./plots/corr_out_smi_",test.plz,".png", sep=""))
ggpubr::ggscatter(inspect.3.3, x = "mon", y = "relat", 
                  #color = "mon", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "monthly smi value", ylab = "relative index of outages") +
  theme(legend.position="right") 
#dev.off() 
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
                  color = "loc.mean", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson"     ) 
##################################################################


##################################################################

##########     stats per fuel type of power plant       ###########
# make a dataframe extract on demand
extract.pp.df<-dplyr::filter(fuel.smi.out.stat.df, fuel.smi.out.stat.df$max>=0)

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

#########      estimate corr  sts for each eic         ###########
# run correlation tests
cor.test.ls<-list()
i<-1
for (i in 1:dim(unq.out.ts.loc.df)[1]) {
  inspect<-dplyr::filter(smi.out.stat.df, eic==unq.out.ts.loc.df[i,1])
  temp.corstat.df<-inspect
  
  if (length(inspect$mean)>=0 & sum(unique(inspect$mean))>=0 ) {  #| sum(unique(inspect$ratio))>=5 
    correlat<-cor.test(inspect$smi.radmeans, inspect$mean, method = c("pearson"), na.action = "na.exclude")
    shapiro<-shapiro.test(inspect$mean) 
    #shapiro<-goft::lnorm_test(inspect$max) #lest if log-normal fitting
    #shapiro<-MASS::fitdistr(inspect$max, "log-normal" )
    temp.corstat.df$cor.p<-correlat$p.value
    temp.corstat.df$cor.c<-correlat$estimate
    temp.corstat.df$shp.p<-shapiro$p.value
    
  } else  { temp.corstat.df$cor.p<-0
  temp.corstat.df$cor.c<-0
  temp.corstat.df$shp.p<-0}
  
  cor.test.ls[[i]]<-temp.corstat.df 
}

out.cor.test.df<- dplyr::bind_rows(cor.test.ls[1:length(cor.test.ls)])
dim(out.cor.test.df) # 6460
dim(out.cor.test.df[unique(out.cor.test.df$eic),]) # 108 EICs

# FIND OUT WHAT POWER PLANTS ARE THESE IN CORRELATIONS:
out.cor.test.df<- left_join(out.cor.test.df, eic.info.df, by=c("eic"="EIC_Code") )

##################################################################
#file.name.2<-paste("./data_outage/corr.out.smi_2019.csv" )
#write.csv(out.cor.test.df, file=file.name.2)
##################################################################

# COR TEST
# p-value of the test should be less than the significance level alpha = 0.05.
# Then they are significantly correlated with a correlation coefficient cor
# WE ARE INTERESTED IN NEGATIVE CORRELATION COEF- IF X INCREASES, Y - DECREASES AND VICE VERSA


# SHAPIRO TEST
# if p-values are greater than the significance level 0.05 - 
# the distribution of the data are not significantly different from normal distribution.
##################################################################
#########           Plot correlation outages           ###########
##################################################################

# HOW MANY POWER PLANTS HAVE COR P-VAL < 0.05
cor.p.df<-out.cor.test.df[out.cor.test.df$cor.p >= 0.05,]
unq.cor.p.df<- cor.p.df[!duplicated(cor.p.df$eic),]
dim(unq.cor.p.df)   # 41 EICs with mean of outages
negative.cor<-unique(dplyr::select(dplyr::filter(cor.p.df, cor.c<=0), c("eic", "plz", "lat", "lon", "cor.c" )))
positive.cor<-unique(dplyr::select(dplyr::filter(cor.p.df, cor.c>=0), c("eic", "plz", "lat", "lon", "cor.c" )))
dim(negative.cor)

# HOW MANY of these POWER PLANTS HAVE COR P-VAL > 0.05 for SHAPIRO NORMALITY TEST
shp.cor.df<-unq.cor.p.df[unq.cor.p.df$shp.p >=0.05, ]
dim(shp.cor.df)  # 1 with mean of outages

# plot the correlation
ggscatter(inspect, x = "means", y = "max", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "means", ylab = "out.max")
##################################################################



##################################################################
#########          Plot power plants outages           ###########
##################################################################

#########               make the raster                ###########
# build my power plants against the rater of smi (take averages for smi at each location?)
#########                CHOOSE YEAR                   ###########
FromYear<-2010
ToYear<-2018
Mon<-9
##################################################################
#time.st<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear), c("period"))
#time.st<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear & year <=ToYear ), c("period"))
time.st<-dplyr::select(dplyr::filter(timedate.df, year >= FromYear & year <=ToYear & mon == Mon ), c("period"))
#dim(smi)
# choose which time slice to graph
smi.raster<-smi[,,time.st$period]
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
To14<-as.Date("1995-12-01")

From15<-as.Date("1995-01-01")
To18<-as.Date("2018-12-01")

#create time stamp
timedate.df<-data.frame(date=seq(as.Date("1951/1/1"), as.Date("2018/12/31"), by="months"))
timedate.df$period<-seq(1,dim(timedate.df)[1], by=1)
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

smi.raster.ratio<-(smi.raster.mean15_18/smi.raster.mean51_14-1)*100
slice.raster.comp.rat <- raster(t(smi.raster.ratio), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.comp.rat <- flip(slice.raster.comp.rat, direction='y')
r.spdf.r <- as(slice.raster.comp.rat, "SpatialPixelsDataFrame")
r.df.r <- as.data.frame(r.spdf.r)


# if min 51-14 is greater than minimum 15-18 (less dry), then show 15-18, esle - 0 (these pleces were more precipitated)
smi.raster.comp.min<-ifelse(smi.raster.min51_14>=smi.raster.min15_18, smi.raster.min15_18, 0 )
slice.raster.comp.min <- raster(t(smi.raster.comp.min), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.comp.min <- flip(slice.raster.comp.min, direction='y')
r.spdf.m <- as(slice.raster.comp.min, "SpatialPixelsDataFrame")
r.df.m <- as.data.frame(r.spdf.m)


# if mean 51-14 is greater than mean 15-18 (less dry), then show 15-18, esle - 0 (these pleces became more precipitated)
smi.raster.compared<-ifelse(smi.raster.mean51_14>=smi.raster.mean15_18, smi.raster.mean15_18, 0 )
# TAKE MEANS AT EACH LOCATION FOR smi SINCE FromYear
slice.raster.compared <- raster(t(smi.raster.compared), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
slice.raster.compared <- flip(slice.raster.compared, direction='y')
r.spdf.c <- as(slice.raster.compared, "SpatialPixelsDataFrame")
r.df.c <- as.data.frame(r.spdf.c)


identical(smi.raster.compared, slice.raster.compared)


#file.name<-paste("./plots/", "data_tmp_smi_out" , FromYear, "-", ToYear, ".svg", sep="")
#svg(file.name,  width = 500, height = 500)
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
#dev.off()
##################################################################
