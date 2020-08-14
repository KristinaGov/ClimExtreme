# This script connects to ENTSO-e API and downloads the data about outages.

# About the ENTSO-e definitions of unavailability of generation units:
# https://transparency.entsoe.eu/content/static_content/Static%20content/knowledge%20base/data-views/outage-domain/Data-view%20Unavailability%20of%20Production%20and%20Generation%20Units.html

# My credits to github project for useful hints:
# https://github.com/krose/entsoeR/blob/master/R/outages_get.R

# Make sure the workspace is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
library(logging)
library(httr)
library(XML)
library(dplyr)
library(lubridate)
###################################################################


#########             set working directory             ###########
mydir<-getwd()
setwd(mydir)
###################################################################

###################################################################
#                 Presets: token, dates, codes                   #
###################################################################
# For ENTSO-E guides, please see to entso-e-transparency-xml-schema-use-1-0.pdf in the "./data_outage/"
# Insert your security token. What is a security token you can find out here:
# https://transparency.entsoe.eu/content/static_content/Static%20content/faq/FAQ.html 
securityToken<-"XXX"

# Add TSO names, codes (from the transparency database) and location of folders for the downloaded xml files:
tso<-data.frame(tname=c("50Hz","Amprion","TenneT","TransnetBW"), 
                tcode=c("10YDE-VE-------2","10YDE-RWENET---I","10YDE-EON------1","10YDE-ENBW-----N"),
                folder=c("./data_outage/50Hz", "./data_outage/amp", "./data_outage/ten", "./data_outage/trans"))

# entso-e api has a limit on data that can be downloaded. You can use resolution per months not to exceed the limit. 
# If you exceed the limit you will receive an error code "400" in the loop. Then change the period accordingly.

# Define the interested period (you can change it on need): for start of the months:
s.sdate = "2015-01-01"
s.edate = "2019-12-31"
# for end f the months:
e.sdate = "2015-01-15"
e.edate = "2019-12-31"
# Combine to one line
st.dateArray = paste(seq(floor_date(as.Date(s.sdate, tz = "Europe/Berlin"),"months"), floor_date(as.Date(s.edate, tz = "Europe/Berlin"),"months"), by = "15 days"), "0000", sep = "")
ed.dateArray = paste(seq(floor_date(as.Date(e.sdate, tz = "Europe/Berlin"),"days"), floor_date(as.Date(e.edate, tz = "Europe/Berlin"),"days"), by = "15 days"), "2300", sep = "")

###################################################################
#                 LOOP: get files, errors, joy                    #
###################################################################
# loop to download zip files and extract .xml
for(j in 1:dim(tso)[1]) {
  for(i in 1:length(st.dateArray)) {
  url = "https://transparency.entsoe.eu/api?" 
  request = paste(
    'securityToken=', securityToken, "&",
    'documentType=', "A80","&",
    'DocStatus=', "A05","&",
    'BusinessType=', "A54","&",
    'BiddingZone_Domain=', tso[j,2], "&",
    'periodStart=', stringr::str_remove_all(as.character(st.dateArray[i]), "[-]"), "&", 
    'periodEnd=', stringr::str_remove_all(as.character(ed.dateArray[i]), "[-]") ,
     sep = "");

    url = paste(url, request, sep = "")
  
    tempfile_path <- tempfile(pattern = "file", tmpdir = as.character(tso[j,3]) )
    tempdir_path <- as.character(tso[j,3])

    e_request <- httr::GET(url = url, httr::write_disk(tempfile_path, overwrite = TRUE))
    # Correct status is "200". Check if error:
    #httr::status_code(e_request)
  
    if(httr::http_type(e_request) == "application/zip"){
      zip_files <- unzip(tempfile_path, list = TRUE)
      zip_files$path <- paste0(tempdir_path, "/", zip_files$Name)
      unzip(zipfile = tempfile_path, exdir = tempdir_path)
      }
  }  
  # Delete the temp download files:
  lr<-list.files(as.character(tso[j,3]), full.names = TRUE , pattern = "file")
  if (all(file.exists(lr)))
      file.remove(lr)
} 


###################################################################
#      In case of ERRORS: get the following in the loop           #
###################################################################

# Put the following lines inside the loop if any errors suspected.
# Specific error codes are 400 and 500:
if(httr::status_code(e_request) == 400){
  stop(paste0(httr::http_status(e_request)$category, ". ",
              httr::http_status(e_request)$reason, ". ",
              httr::http_status(e_request)$message, ". ",
              e_request %>% httr::content(., encoding = "UTF-8") %>% 
                xml2::xml_child(., 8) %>% 
                xml2::xml_child(., 2) %>% 
                xml2::xml_text()),
       paste(st.dateArray[i],  tso[j,2], sep=" - "),
       call. = FALSE)
} else if(httr::status_code(e_request) == 500){
  stop(paste0(httr::http_status(e_request)$category, ". ",
              httr::http_status(e_request)$reason, ". ",
              httr::http_status(e_request)$message, ". ",
              e_request %>% httr::content(., encoding = "UTF-8") %>% 
                rvest::html_node("body") %>% 
                rvest::html_text()), 
       paste(st.dateArray[i],  tso[j,2], sep=" - "), 
       call. = FALSE)
}
