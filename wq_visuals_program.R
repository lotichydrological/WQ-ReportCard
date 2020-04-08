# debugging section

# Category <- "Metals"
# UseClass <- "Aquatic_Life"
# segmentName <- "COLCLC10_A"
# Indicator <-"Cadmium"
# CharacteristicNames <- "Cadmium;;;;;;;"
# Units <- "ug/l"
# standard <- "1.101672-(log(hardness)*(0.041838))*exp(0.7998*(log(hardness))-4.4451)"
# valueType <-"Dissolved"
# eval_type <- "hardness_calc"
# StartDate <- ('01-01-2008') # MM-DD-YYYY This is the start date for the entire data set you want.
# EndDate <- ('12-31-2015') # MM-DD-YYYY This is the end date for the entire data set you want.

#########################################################################

# Working directory will already be set if you opened the project from its directory in windows explorer
setwd("D:/wq_report_card/WQ-ReportCard")

# Initialize program and clear workspace
rm(list = ls())

# Load Functions
sapply(list.files(pattern="[.]R$", path="./Functions/", full.names=TRUE), source)

#Load the monitoring stations file that was prepped in the GIS with 305(b) Segment ID information joined in
monitoringStations <- read.csv(file="./Program_Files/Monitoring_Stations_MCW_2018_30y.csv", 
                               header=TRUE, sep=",",stringsAsFactors=F, strip.white=T)

# WQPortal has been returning some datasets with duplicate rows, check for and remove these, they
# may not be apparent in the GIS prep if the point features were perfectly stacked on each other
monitoringStations <- monitoringStations %>% distinct()

# read in segment standards
wqStandards <- read.csv(file="./Program_Files/2018_Standards_new.csv", header=TRUE, stringsAsFactors=F)

# Load the dataset
watershedData <- read.csv(file="./Data/HUC_14010005_dataSheet.2020-03-11.csv",header=T,sep = "",stringsAsFactors=F,strip.white=T) #retrieve the datasheet

# Format dates
watershedData$ActivityStartDate <- as.Date(watershedData$ActivityStartDate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")) #format as dates

# Retrieve all stations where data was actuall collected during your specified time period in the database extract
Monitoring_Station_List <- unique(as.list(watershedData$MonitoringLocationIdentifier)) 

# Filter the dataset for only those stations we want to use;
# data will not be present at all sites initially downloaded from the WQP for the watershed, so pare the site list
# down to just those sites present in the dataset
monitoringStations <- monitoringStations[monitoringStations$MonitoringLocationID %in% Monitoring_Station_List,]

#check range of dates present in the dataset
range(watershedData$ActivityStartDate, na.rm=T) 

# remove rows with missing dates, this data can't be used
watershedData <- watershedData[!is.na(watershedData$ActivityStartDate),]

#Tidy up the data sheet to include only relevant columns
watershedData = watershedData[,c("OrganizationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultValueTypeName", "DetectionQuantitationLimitMeasure.MeasureValue")]

# subset data frame to only include characteristics of interest
# characteristics <- wqStandards$CharacteristicNames

# parameterNames = unlist(strsplit(characteristics, split = ";")) #Retrieve all the possible parameter names
# parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
 
# watershedData = watershedData[watershedData$CharacteristicName %in% parameterNames,]

watershedData <- standardize_units(watershedData) # make sure nutrients are all in correct inified unit values for standards assessment

segment_list <- c("COLCLC01_A","COLCLC15a_A", "COLCLC15b_A", "COLCLC15c_A", "COLCLC15d_A", "COLCLC16_A" , "COLCLC17a_A", "COLCLC17b_A","COLCLC04a_B", "COLCLC02a_A", "COLCLC14c_C", "COLCLC14c_B", "COLCLC15d_A", "COLCLC16_A" 
                                ,"COLCLC15a_A", "COLCLC15c_A", "COLCLC13a_B", "COLCLC13a_A", "COLCLC12b_A", "COLCLC04d_A", "COLCLC04a_A" ,"COLCLC06_A",  "COLCLC11b_A" ,"COLCLC11f_A" ,"COLCLC17b_A"
                                ,"COLCLC10_A",  "COLCLC10_B" , "COLCLC04a_C", "COLCLC04c_A" ,"COLCLC14a_A", "COLCLC11h_A", "COLCLC04e_A", "COLCLC17a_A", "COLCLC09a_A", "COLCLC04a_D" ,"COLCLC14b_A"
                                ,"COLCLC11g_A", "COLCLC11e_A", "COLCLC08_A"  ,"COLCLC11a_A" ,"COLCLC15b_A", "COLCLC09c_A")

characteristics <- wqStandards$CharacteristicNames

categories <- unique(wqStandards$Category)

indicators <- unique(wqStandards$Indicator)

useclasses <- unique(wqStandards$UseClass)

library(rgdal)

seg_shp <- readOGR("Data/reg_93_rpj.shp")


StartDate <- ('01-01-1988') # MM-DD-YYYY This is the start date for the entire data set you want.
EndDate <- ('12-31-2018') # MM-DD-YYYY This is the end date for the entire data set you want.

########################################################################################
# generate box plot
buildBoxplot(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate)

# generate scatter plot 
buildScatterplot(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate)

# generate summary statistics table
getSummaryStats(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate)

# access narrative segment description
segment_description <- monitoringStations[monitoringStations$AUID == segmentName,"PortionDes"][1]
print(segment_description)


