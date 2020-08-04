# The ENTSO-e data is reported in .xml format. This script reads .xml files
# and picks the necessary data to collect it in the data frame.
# Many thanks for the following discussion at the stackoverflow:
# https://stackoverflow.com/questions/13579996/how-to-create-an-r-data-frame-from-a-xml-file

# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
library(dplyr)
library(flatxml)
###################################################################


#########             set working directory             ###########
mydir<-getwd()
setwd(mydir)
###################################################################


##################################################################
##################################################################
#                   Read files and glimpse                       #
##################################################################
##################################################################

#########         exclude unnecessary items            ###########
exclude<-c("mRID", "businessType", 
           "curveType",
           "production_RegisteredResource.location.name",
           "production_RegisteredResource.pSRType.psrType",
           "Available_Period",
           "Reason",
           "timeInterval"
)
##################################################################

#########       read all file names in the folder      ###########
folder<- as.list(c("50hz", "amp", "ten", "trans"))
files.ls<-list()
# Loop that reads fine names in the directory with xml files
for (i in 1:4) {
  filepath<-paste("./data_outage/", folder[[i]], sep="")
  files.ls[[i]]<-as.list(list.files(path = filepath, 
                                    all.files = T, full.names = T,
                                    pattern = "CHANGES_IN_ACTUAL"))
}
length(files.ls)
files.ls<-do.call(c, files.ls)
length(files.ls) #  12399
exist.list<-list()
for (i in 1:length(files.ls)) {
  exist.list[[i]]<-file.exists(files.ls[[i]])
}
all(exist.list)
##################################################################

#########         loop to read all .xlm data            ###########
data.list<-list()
for (i in 1:length(files.ls)) {

  doc <- fxml_importXMLFlat(files.ls[[i]])
  ts<-doc$elemid.[match("TimeSeries", doc$elem.)]
  tr<-doc$elemid.[match("Reason", doc$elem.)]
  pt<-doc$elemid.[match("Point", doc$elem.)]
  
  det.df<-fxml_toDataFrame(doc, siblings.of=ts, elem.or.attr="elem",
                           col.attr="elem", exclude.fields=exclude)
  reason.df<-fxml_toDataFrame(doc, siblings.of=tr, elem.or.attr="elem",
                              col.attr="elem", exclude.fields=exclude)
  point.df<-fxml_toDataFrame(doc, siblings.of=pt, elem.or.attr="elem",
                             col.attr="elem", exclude.fields=exclude)
  
  det.df$reas_c <- reason.df$code[1] 
  det.df$reas_t <- reason.df$text[1]
  det.df$quantity <- point.df$quantity[1]
  data.list[[i]]<-det.df
}
length(data.list) # 12399
# Combine to a single data frame
big_data.df <- dplyr::bind_rows(data.list[1:length(data.list)])
# Create a safe version of the data frame (to store data for a backup):
big_data.df.0<-big_data.df
big_data.df<-big_data.df.0
dim(big_data.df) # 12399 14
#sapply(big_data.df, class) # check the class of the data
##################################################################

##################################################################
file.name.1<-paste("./data_outage/xml_output_2019.csv" )
write.csv(big_data.df, file=file.name.1)
##################################################################

#########                 Clean data                   ###########
# Remove letters given at the end of time stamps:
big_data.df$start_DateAndOrTime.time<-gsub("[a-zA-Z ]", "", big_data.df$start_DateAndOrTime.time)
big_data.df$start_DateAndOrTime.date<-as.Date(big_data.df$start_DateAndOrTime.date)
big_data.df$end_DateAndOrTime.time<-gsub("[a-zA-Z ]", "", big_data.df$end_DateAndOrTime.time)
big_data.df$end_DateAndOrTime.date<-as.Date(big_data.df$end_DateAndOrTime.date)

# Add explicit time stamps:
start<-stringr::str_split_fixed(big_data.df$start_DateAndOrTime.date, "-", 3)
end<-stringr::str_split_fixed(big_data.df$end_DateAndOrTime.date, "-", 3)
startend<-cbind(start, end)
startend.df<-data.frame(year=startend[,1], st.mon=startend[,2], st.d=startend[,3], end.mon=startend[,5], end.d=startend[,6])
big_data.df<-cbind(big_data.df, startend.df)

dim(big_data.df) # 212399    19
big_data.df[,"X"]<-NULL
big_data.df<-unique(big_data.df)
dim(big_data.df) # 11480    19

# Filter out the codes B18 (failure) and B20 (shutdown) A95(failure - no reason) | B19 (foreseen maintenance)
# For information on the codes look here:
# https://transparency.entsoe.eu/content/static_content/Static%20content/knowledge%20base/SFTP-Transparency_Doc
see.codes<-unique(big_data.df$reas_c)
big_data.df<-dplyr::filter(big_data.df, reas_c!="B19")
dim(big_data.df) # 10209 19
dim(big_data.df[!duplicated(big_data.df$production_RegisteredResource.mRID),]) # 110  19

# Each power plant (as well as other units of the power system) has a unique energy identification code (EIC).
# see: https://www.entsoe.eu/data/energy-identification-codes-eic/
# Remove foreign EICs from the database:
# (EICs that belong to power plants not in Germany but used for reserve capacity in neighboring countries)
foreign.eic<-c("11WD72VIA2H-KW-0" , "11WD2SILT000157E", "11WD43VIWXHOILLM", "11WD2KUET000160J")
big_data.df<-big_data.df[!big_data.df$production_RegisteredResource.mRID %in% foreign.eic, ] 
dim(big_data.df) # for 2019 - 9636
dim(big_data.df[!duplicated(big_data.df$production_RegisteredResource.mRID),]) # 106

# Add date / time stamps:
big_data.df$st.datetime<-paste(big_data.df$start_DateAndOrTime.date, big_data.df$start_DateAndOrTime.time)
big_data.df$end.datetime<-paste(big_data.df$end_DateAndOrTime.date,  big_data.df$end_DateAndOrTime.time)
big_data.df$st.datetime<-as.POSIXct(big_data.df$st.datetime, format = "%Y-%m-%d %H:%M:%S")
big_data.df$end.datetime<-as.POSIXct(big_data.df$end.datetime, format = "%Y-%m-%d %H:%M:%S")

# Add duration of the outages in days:
big_data.df$duration_days<-difftime(big_data.df$end.datetime, big_data.df$st.datetime, units = c("days") )

##################################################################
# BIG DATA CONTAINS ALL DATA ABOUT THE UNPLANNED OUTAGES WITH EIC POWER PLANT CODES
filename<-paste("./data_outage/big_out_data_2019.csv", sep="")
write.csv(big_data.df, file=filename)
##################################################################



##################################################################
##################################################################
#            Check manually data for mistakes                    #
##################################################################
##################################################################

check.doc <- fxml_importXMLFlat(files[[1]])

match("TimeSeries", check.doc$elem.)
match("Reason", check.doc$elem.)

check.ts<-check.doc$elemid.[match("TimeSeries", check.doc$elem.)]
check.tr<-check.doc$elemid.[match("Reason", check.doc$elem.)]
check.pt<-check.doc$elemid.[match("Point", check.doc$elem.)]

check.det.df<-fxml_toDataFrame(check.doc, siblings.of=check.ts, elem.or.attr="elem",
                               col.attr="elem", exclude.fields=exclude)
check.reason.df<-fxml_toDataFrame(check.doc, siblings.of=check.tr, elem.or.attr="elem",
                                  col.attr="elem", exclude.fields=exclude)
check.point.df<-fxml_toDataFrame(check.doc, siblings.of=check.pt, elem.or.attr="elem",
                                 col.attr="elem", exclude.fields=exclude)
check.det.df$reas_c <- check.reason.df$code[1] 
check.det.df$reas_t <- check.reason.df$text[1]
check.det.df$reas_t <- check.point.df$quantity[1]