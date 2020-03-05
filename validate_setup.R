# set working directory
setwd("D:/wq_report_card/WQ-ReportCard")

# Load Functions
sapply(list.files(pattern="[.]R$", path="./Functions/", full.names=TRUE), source)

# Load Validation Functions
sapply(list.files(pattern="[.]R$", path="./Functions_validate/", full.names=TRUE), source)

#Load the monitoring stations file that was prepped in the GIS with 305(b) Segment ID information joined in
monitoringStations <- read.csv(file="./Program_Files/Monitoring_Stations_MCW_2018_30y.csv", 
                               header=TRUE, sep=",",stringsAsFactors=F, strip.white=T)

# WQPortal has been returning some datasets with duplicate rows, check for and remove these, they
# may not be apparent in the GIS prep if the point features were perfectly stacked on each other
monitoringStations <- monitoringStations %>% distinct()

# Load the validation dataset that has already been produced using getDataByHUC8 function
watershedData <- read.csv(file="./Data/HUC_14010005_dataSheet.2020-02-24.csv",header=T,sep = "",stringsAsFactors=F,strip.white=T) #retrieve the datasheet

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

#' Define data query and assessment date ranges. Range should span 5 years to allow for trends 
#' assessment and to conform with WQCD listing methodology
StartDate <- ('01-01-1988') # MM-DD-YYYY This is the start date for the entire data set you want.
EndDate <- ('12-31-2018') # MM-DD-YYYY This is the end date for the entire data set you want.
HUC8 <- "14010005" #COLORADO HEADWATERS-PLATEAU


UseClass <- "Human_Health"
Category <- "Water_Supply"
Indicator <-"Cadmium"
CharacteristicNames <- "Cadmium;;;;;;;"
Units <- "ug/l"
valueType <-"Total"
standardType <- "one_day"
standard <- 5
segmentName <- "COLCLC10_A"
monitoringStation <- "21COL001_WQX-TMDL-MC06"
stationIDs = unique(as.vector(monitoringStations$MonitoringLocationID[monitoringStations$SegmentID == segmentName]))
stationIDs


Data = watershedData[watershedData$MonitoringLocationIdentifier %in% stationIDs,]
#Tidy up the data sheet to include only relevant columns
Data = Data[,c("OrganizationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultValueTypeName", "DetectionQuantitationLimitMeasure.MeasureValue")]

#Retrieve the relevant WQCD water quality standards for the segment
wqStandards <- read.csv(file="./Program_Files/2018_Standards.csv", header=TRUE, stringsAsFactors=F)

# read in the gauge site key
dischargeStations <- read.csv('./Program_Files/discharge_stations_MCW.csv', 
                              colClasses=c(rep("character", 4)), stringsAsFactors=FALSE)

#remove the reach designator from the segmentID so that the program will just grab the standards assigned to the larger
#WBID (305b segment), not the smaller AUID (305b reach)
segmentWBID <- gsub("_.*","",segmentName)

segmentStandards = wqStandards[,c("UseClass", "Category", "Indicator", "CharacteristicNames", "Units", "valueType", "standardType", segmentWBID)]
#Then rename the column of water quality standards to 'standard'
colnames(segmentStandards)[8] <- "standard"

#Set up data for validation
Data$ActivityStartDate = as.Date(strptime(as.character(Data$ActivityStartDate), "%Y-%m-%d")) #format as dates
Data$ResultSampleFractionText[Data$ResultSampleFractionText=="Recoverable"] = "Total" 
Data$ResultMeasureValue =  as.numeric(as.character(Data$ResultMeasureValue)) #convert columns to numeric
Data$ResultMeasureValue[Data$ResultMeasureValue==""] = NA #convert blanks to NA
Data$ResultMeasureValue[Data$DetectionQuantitationLimitMeasure.MeasureValue >= Data$ResultMeasureValue] = NA #convert values below lower detection or reporting limits to NA

