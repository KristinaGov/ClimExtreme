# this file just reads outage data

rm(list=ls(all=TRUE))

library(readxl)
library(dplyr)
library(plyr)
library(tidyr)
library(readxl)
library(raster)

mydir<-getwd()
setwd(mydir)

#########################################################################
#                               read data                               #
#########################################################################

# read BIG DATA file generated from the script woith outages
#filename.1<-paste("./data_outage/big_out_data.csv", sep="")
filename.1<-paste("./data_outage/big_out_data_2019.csv", sep="")
big.out.df<-data.frame(read.csv(filename.1, sep=","))
dim(big.out.df)   # for 2019 - #9609
#check how many unique power plants id's are there
dim(big.out.df[!duplicated(big.out.df$production_RegisteredResource.mRID),]) # for 2019 - #108

# there if there are duplicated rows in the reporting data due to quiry overlaps
big.out.df[,"X"]<-NULL
dim(-unique(big.out.df)) # for 2019 - 9609

#########################################################################
#### extract to correct plz for the powerplants with outages ############
#big.out.philip<-big.out.df[!duplicated(big.out.df$production_RegisteredResource.mRID),]
#big.out.philip<-big.out.philip[!big.out.philip$production_RegisteredResource.mRID %in% foreign.eic, ]
#dim(big.out.philip)
#write.csv(big.out.philip, "./data_outage/big.out.philip.csv")
#########################################################################


#########################################################################
# remove these columns in the big out data
outlist.1=c( "biddingZone_Domain.mRID", "EIC_Display_Name", "EIC_Long_Name",
             "EIC_Function", "UID", "UstId","Website",
             "start_DateAndOrTime.time", "start_DateAndOrTime.date",
             "end_DateAndOrTime.time", "end_DateAndOrTime.date")
big.out.df[,outlist.1]<-NULL
dim(big.out.df)  # for 2019 - #9609
# check if we havn t deleted something meaningful with columns
dim(-unique(big.out.df)) # for 2019 - #9609
#unique.eic<-unique(big.out.df$production_RegisteredResource.mRID)
#see<-filter(big.out.df, production_RegisteredResource.mRID=="11WD8GOLD1H----6")
#########################################################################



#########################################################################
############   combine big_data with plz and eic codes    ###############
#########################################################################
# the file is based on the unique EIC codes of the big_out_data and plz/BNA data
eic.df<-as.data.frame(read_excel("./data_outage/big.out.philip.xlsx", 
                                 sheet = "eic_plz", col_names = T))
# change formats of the column values if needed
#eic.df$EIC_Code<-as.character(eic.df$EIC_Code)
big.out.df$production_RegisteredResource.mRID<-as.character(big.out.df$production_RegisteredResource.mRID)
#sapply(eic.df, class)

# JOIN data about outages by eic and eic with plz's
out_eic_plz.df<- dplyr::left_join(big.out.df, eic.df, by=c("production_RegisteredResource.mRID"="production_RegisteredResource.mRID"))
dim(out_eic_plz.df) #9580 ; for 2019 9609
dim(out_eic_plz.df[!duplicated(out_eic_plz.df$production_RegisteredResource.mRID),]) #  should be 108 ; for 2019 - #108 

sum(is.na(out_eic_plz.df$plz)) # has to be "0" after we removed the non-german PLZs

# if needed remove the rows wwith NA
out_eic_plz.df<-tidyr::drop_na(out_eic_plz.df, plz)
dim(out_eic_plz.df) # for 2019 - 9605
dim(out_eic_plz.df[!duplicated(out_eic_plz.df$production_RegisteredResource.mRID),]) #  should be 108 ; for 2019 - #106
##################################################################


##################################################################
# DATA CONTAINS ALL DATA ABOUT THE UNPLANNED OUTAGES WITH EIC POWER PLANT CODES and PLZ codes and CITIES
filename.2<-paste("./data_outage/out_eic_plz_2019.csv", sep="")
write.csv(out_eic_plz.df, file=filename.2)
#################################################################


########################################################################
# turn dates to dates and time
out_eic_plz.df$st.datetime<-as.POSIXct(out_eic_plz.df$st.datetime, format = "%Y-%m-%d %H:%M:%S")
out_eic_plz.df$end.datetime<-as.POSIXct(out_eic_plz.df$end.datetime, format = "%Y-%m-%d %H:%M:%S")
########################################################################


########################################################################
#########             estimate outage ratio            #################
########################################################################
# ratio of the MW outages from the installed capacity
out_eic_plz.df$quantity<-as.numeric(out_eic_plz.df$quantity)
out_eic_plz.df$ratio<-(out_eic_plz.df$production_RegisteredResource.pSRType.powerSystemResources.nominalP-out_eic_plz.df$quantity)/out_eic_plz.df$production_RegisteredResource.pSRType.powerSystemResources.nominalP

dim(out_eic_plz.df[!duplicated(out_eic_plz.df$production_RegisteredResource.mRID),]) #106
# check NAs
sum(is.na(out_eic_plz.df$plz))  # has to be "0"
##################################################################

##################################################################
# make time data series
#years<-as.data.frame(stringr::str_split_fixed(seq(as.Date("2015/1/1"), as.Date("2019/12/31"), by="months"), "-", 3))
#years$V1<-as.numeric(as.character(years$V1))
#years$V2<-as.numeric(as.character(years$V2))
#date.df<-data.frame(year=years[,1], mon=years[,2])
##################################################################

#########             estimate outage stats            ###########
# create a list of power plants that had outages from 2015 to 2018
# needed for loop
ind.out.kwk<-unique(out_eic_plz.df$production_RegisteredResource.mRID)
length(ind.out.kwk)             # for 2019 - 106

##################################################################
#########     estimate outage stats loop per month     ###########
##################################################################
out.stat.ls<-list()
out.ts.ls<-list()
takelist.1<- c("st.datetime","end.datetime", "year", "st.mon", "st.d", "plz", "city",  "ratio", 
               "production_RegisteredResource.mRID", "production_RegisteredResource.pSRType.powerSystemResources.mRID",
               "production_RegisteredResource.pSRType.powerSystemResources.name")
#i=2
for (i in 1:length(ind.out.kwk)) {
  f.extract<-dplyr::select(dplyr::filter(out_eic_plz.df, production_RegisteredResource.mRID == ind.out.kwk[i]), 
                         all_of(takelist.1))  
  f.extract$nmbr<-1
  # aggregate frequency at the same time stamp (e.g. different blocks of the same production unit)
  f.agg.1<-plyr::ddply(f.extract, .(st.datetime, end.datetime, year, st.mon, st.d, plz, city, 
                                    ratio,production_RegisteredResource.mRID), summarise, freq=sum(nmbr))
  out.ts.ls[[i]]<-f.agg.1
  
  # aggregate frequency at the same monthsc for generation units of one production unit
  f.agg.2<-plyr::ddply(f.extract, .(year, st.mon, plz, city, production_RegisteredResource.mRID), summarise, 
                       freq=sum(nmbr), ratio=mean(ratio), max=max(ratio), min=min(ratio), mean=mean(ratio), mod=modal(ratio) )
  out.stat.ls[[i]]<-f.agg.2
}

out.stat.df<- dplyr::bind_rows(out.stat.ls[1:length(out.stat.ls)])
dim(out.stat.df) # for 2019 - 2524 
dim(out.stat.df[unique(out.stat.df$production_RegisteredResource.mRID),])  #106

out.ts.df<- dplyr::bind_rows(out.ts.ls[1:length(out.ts.ls)])
dim(out.ts.df) # 12337
dim(out.ts.df[unique(out.ts.df$production_RegisteredResource.mRID),]) #106

##################################################################

#########                data output                   ###########
filename.3<-paste("./data_outage/out_timeseries_2019.csv" )
write.csv(out.ts.df, file=filename.3)

filename.4<-paste("./data_outage/out_stat_2019.csv" )
write.csv(out.stat.df, file=filename.4)
##################################################################
