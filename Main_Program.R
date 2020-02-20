###############################################################################
###############################################################################
#
#           WQX Water Quality Summary Tool for Colorado Watersheds
#
# Retrieves and generates summary statistics for water quality data
# served by the EPA/USGS Water Quality Portal web services:
# (http://www.waterqualitydata.us/). Output formatting conforms to the Water
# Quality Report Card developed for Colorado. Statistical analysis conducted
# according to State of Colorado Regulations #31 and #33, as well as
# recommendations from EPA for water quality protection.
#
# Created by:
#                 Seth Mason
#                 Lotic Hydrological, LLC
#                 P.O. Box 1524
#                 Carbondale, CO 81623
#
# Last Modified 
#' Date: 1-28-2015: BH, Updated to include macroinvertebrate and nutrient sites and data
#' Date: 12-7-2017: BH, Run for 2017 ERW data
#' Date: 12-21-2017: BH,  for MCWC up to 2018
#' Date: 10-22-2019: BH, run for MCW SMP, 30 year extended run to improve dataset size
#' Date: 12-23-2019: BH, run for MCW SMP, re-run to include glenwood cn and exclude plateau ck'
#' Date: 02-18-2018: BH, run for MCW SMP, re-run with additional phosophorus data
#'
#'
###############################################################################

#;       INITIALIZE WORKING ENVIRONMENT

# Workind directory will already be set if you opened the project from its directory in windows explorer
setwd("G:/Shared drives/Projects/MCWC/Data/water_quality/WQ_report_card/Code/report_card_30yr")

# Initialize program and clear workspace
rm(list = ls())

# Load Functions
sapply(list.files(pattern="[.]R$", path="./Functions/", full.names=TRUE), source)

#############################################################################

#   LOAD PROGRAM FILES

#Load the monitoring stations file that was prepped in the GIS with 305(b) Segment ID information joined in
monitoringStations <- read.csv(file="./Program_Files/Monitoring_Stations_MCW_2018_30y.csv", 
                               header=TRUE, sep=",",stringsAsFactors=F, strip.white=T)

# WQPortal has been returning some datasets with duplicate rows, check for and remove these, they
# may not be apparent in the GIS prep if the point features were perfectly stacked on each other
monitoringStations <- monitoringStations %>% distinct()

# remove any other stations manually that you don't want
remove_stations <- c("","","")
monitoringStations <- monitoringStations[!(monitoringStations$MonitoringLocationID %in% remove_stations),]

# read in segment standards
wqStandards <- read.csv(file="./Program_Files/2018_Standards.csv", header=TRUE, stringsAsFactors=F)

# read in the gauge site key
dischargeStations <- read.csv('./Program_Files/discharge_stations_MCW.csv', 
                              colClasses=c(rep("character", 4)), stringsAsFactors=FALSE)

#' Define data query and assessment date ranges. Range should span 5 years to allow for trends 
#' assessment and to conform with WQCD listing methodology
StartDate <- ('01-01-1988') # MM-DD-YYYY This is the start date for the entire data set you want.
EndDate <- ('12-31-2018') # MM-DD-YYYY This is the end date for the entire data set you want.
HUC8 <- "14010005" #COLORADO HEADWATERS-PLATEAU


####################################################################################
#'
#'  MAKE THE DATABASE CALL
#'
#'  Only run 'getData' once, when initially downloading data, if you manually specified download
#'  parameters for a dataset on the Water Quality Portal, you don't need to run the getDataByHUC8 function

#retrieve the full data set for the period of interest and create a datasheet
#getDataByHUC8(HUC8,StartDate, EndDate) 


##################################################################################
#
#'  PROGRAM FILEs PREPARATION

#'   Only need to run/rerun from here once data has been downloaded the first time


# Load the dataset
watershedData <- read.csv(file="./Data/Combined_14010001_14010005_dataSheet.2019-12-23.csv",header=T,stringsAsFactors=F,strip.white=T) #retrieve the datasheet

#' Format dates
watershedData$ActivityStartDate <- as.Date(watershedData$ActivityStartDate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")) #format as dates

# Retrieve all stations where data was actuall collected during your specified time period in the database extract
Monitoring_Station_List <- unique(as.list(watershedData$MonitoringLocationIdentifier)) 

#' Note, for MCW, many of these stations extracted from the database call are outside of the 
#' project area, normally this list will be shorter than your the number of unique monitoring 
#' sites downloaded in the WQP 'stations' file.

#' Filter the dataset for only those stations we want to use;
#' data will not be present at all sites initially downloaded from the WQP for the watershed, so pare the site list
#' down to just those sites present in the dataset
monitoringStations <- monitoringStations[monitoringStations$MonitoringLocationID %in% Monitoring_Station_List,]

 # filter dataset to only most recent 5 years, Jan 1, 2013 - Dec 31, 2017 if doing for currentstandards analysis
 #watershedData <- watershedData[year(watershedData$ActivityStartDate) >= 2013,]

#check range of dates present in the dataset
range(watershedData$ActivityStartDate, na.rm=T) 
 
# remove rows with missing dates, this data can't be used
watershedData <- watershedData[!is.na(watershedData$ActivityStartDate),]

# write a finalized list of stations used to file, if desired, for later use in GIS visuals
#write.csv(monitoringStations,"./Program_Files/RC_Site_List_30yr.csv", row.names=F)  


# Create a segment list to use for looping
segmentIDs <- unique(as.vector(monitoringStations$AUID))
print(paste("Total number of segments: ", length(segmentIDs), sep=''))

# Write a file to use to cross-check the 'SEGMENT_IDs' input page in the report card spreadsheet.
# Paste this into the 'MonitoringLocations' worksheet in the excel Report Card
# segmentDesc <- unique(monitoringStations[c("AUID", "PortionDes")])
# write.csv(segmentDesc[order(segmentDesc$AUID),] , "./Program_files/RC_Segment_List.csv", row.names=F)  


# output a map with sites summarized by data availability
# mapSiteData(watershedData, monitoringStations)


# Create a list of sites reports already completed so they won't be run again,
# for running the whole program in parts, a segment at a time, or if some sites 
# run but others have bugs...

exclude <-c("COLCLC15a_A", "COLCLC15b_A", "COLCLC15c_A", "COLCLC15d_A", "COLCLC16_A" , "COLCLC17a_A", "COLCLC17b_A")
completed <- c()
completed <-sub(".csv" ,"" ,list.files(pattern="[.]csv$", path="./Output/", full.names=F))
segment_list <- sort(unique(segmentIDs)[!(unique(segmentIDs) %in% completed) & !(unique(segmentIDs) %in% exclude)])
print("Remaining segments to analyze:")
print(segment_list)
 
   
###############################################################################

#'     SEGMENT ANALYSIS

   
 for (segment in segment_list){
   stationIDs = unique(as.vector(monitoringStations$MonitoringLocationID[
     tolower(monitoringStations$SegmentID) == tolower(segment)]))
   cat("\n 305(b) Segment ID: ", segment)
   cat("\n   Monitoring stations: ",stationIDs)
   cat("\n")
   #for troubleshooting if a certain segment and/or parameter is causing issues
   #if(segment=="COUCEA02_A"){debug(some_function)}
   summarizeSegments(segment, stationIDs)
 }
 
  
############################################################################## 

#' Miscellaneous troublshooting code
#' 
# debug(some_function)
# undebug(some_function)