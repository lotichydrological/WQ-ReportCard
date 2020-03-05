library(reshape2)
library(reshape)



###########################333
# # Use these values only to test a specific site or segment
# sitename <- "ERWC-ERaRC"
# sitename <- "ERWC-ERaGC"
# sitename <- "USFS-gypsmMIS"
# Data <- watershedData[watershedData$MonitoringLocationIdentifier==sitename,]
# remove(Data)
# 
# HUC="1401000302"
# hucStations = monitoringStations[monitoringStations$HUC10 == HUC,]
# segmentname <- "COUCEA10a_6300"
# segmentname <- "COUCEA02_6300"
# stationIDs <- as.vector(unique(hucStations$StationID[hucStations$SegmentID == segmentname]))
# stationIDs
# 
# Data <- watershedData[watershedData$MonitoringLocationIdentifier %in% stationIDs,]
# unique(watershedData$MonitoringLocationIdentifier)
# 
# unique(watershedData$ActivityStartDate)
# remove(Data)
# remove(hucStations)
# remove(stationIDs)
# remove(segmentname)
# remove(HUC)
#######################################################################

# This function retrieves all CO_MMI_2010 scores along with associated HBI and Shannon scores 
# in order to assess standards compliance according to Policy 10-1 Aquatic Life Use Attainment

mmi_eval_validate = function(UnusedMMIData,UnusedDummyArgument){ ## THE MMI STANDARD VARIES BASED ON BIOTYPE OF SITE, THEREFORE DOES NOT NEED A STANDARD PASSED TO IT LIKE THE OTHERS, WILL GET THE STANDARD LATER USING THE 'BIOTYPE_KEY' FILE
  
  #take out after it works
  #debug(retrieveInvertebrateData)
  
  # get the auxiliary metric data + MMI data needed to assess MMI scores
  Data <- get("Data", envir = parent.frame()) #the full data set for the segment
  CharacteristicNames <- "CO_MMI_2010;HBI;Shannon Diversity;Shannon;;;;"
  invertebrateData <- retrieveInvertebrateData_validate(Data, CharacteristicNames, "score", "") #data of interest formatted as numerical time series
  invertebrateData$CharacteristicName <- as.character(invertebrateData$CharacteristicName)
  invertebrateData$CharacteristicName[invertebrateData$CharacteristicName =="Shannon Diversity"] <- "Shannon" #Shannon has two different codings, change them all to just 'Shannon'
  auxmetlist <- c("HBI","Shannon Diversity","Shannon")
  
  #str(invertebrateData)
  #test to see if any auxiliary data has been reported with the mmi data (otherwise the conditional tests below will throw an error)
  #if mmi scores are > 52 without auxiliary data, they can still be assessed
  if( (sum(invertebrateData$CharacteristicName %in% auxmetlist) == 0) & (min(invertebrateData$ResultMeasureValue) < 52 )) {
    Exceedances <- NA
    Impaired <- NA
    Assessment <- "Data Gap"
    return(c(Exceedances, Impaired, Assessment))
    break
  }
  
  #put data into wide form
  macroData <- cast(invertebrateData, MonitoringLocationIdentifier + ActivityStartDate ~ CharacteristicName, value = 'ResultMeasureValue', mean)
  
  #remove rows with no MMI but do have HBI or Shannon values
  macroData <- macroData[!is.na(macroData$CO_MMI_2010),]
  macroData <- macroData[!is.nan(macroData$CO_MMI_2010),]
  
  #make some new columns to hold the assessment results  
  macroData$GreyZone   <- ""
  macroData$Status     <- ""
  macroData$Assessment <- ""
  
  #assign biotypes to each site using the biotype key in program files. the merge will eliminate data points without a biotype assignment
  #if sites do not have a biotype assignment, break and return 'No data' to avoid an error
  #monStations <- = get("monitoringLocations", envir = parent.frame())
  #biotypeKey <- read.csv("./Program_files/BiotypeKey_TEST.csv",header=T,stringsAsFactors=F)
  biotypeKey <- monitoringStations[,c("StationID","Biotype")]  # get the two columns from monitoring stations that specify site and biotype
  names(biotypeKey)[names(biotypeKey) == "StationID"] <- "MonitoringLocationIdentifier"  #rename the 'StationID' column for merging with the data
  macroData <- merge(macroData,biotypeKey,"MonitoringLocationIdentifier")
  if(length(macroData$Biotype) == 0) {
    Assessment <- "Missing biotypes"
    return(c(Exceedances, Impaired, Assessment))
    break
  }
  
  # Find out the biotype for the site and set values to the attain/impair threshold variables based on biotype, use 
  # for each segment, needs to loop through the dataframe and individually assess each site 
  #using a combination of 3 metrics.  Assessing individual sites is seen as a special case of segments
  #with only one obs
   
  # biotype    #attainment     #impairment
  # 1 trans       52              42
  # 2 mtns        50              42
  # 3 plns        37              22
  
  #auxiliary metrics thresholds
  #               HBI           Shannon
  # 1 trans       <5.4           >2.4
  # 2 mtns        <5.1           >3.0
  # 3 plns        <7.7           >2.5


for(event in 1:nrow(macroData)){
  print(macroData$EventID[event])
  # set thresholds based on biotype
  if(macroData$Biotype[event] == 1){
    AttainThreshold  <- 52
    ImpairThreshold  <- 42
    HBIThreshold     <- 5.4
    ShannonThreshold <- 2.4
  } else if(macroData$Biotype[event]==2){
    AttainThreshold  <- 50
    ImpairThreshold  <- 42
    HBIThreshold     <- 5.1
    ShannonThreshold <- 3.0   
  } else if(macroData$Biotype[event]==3){
    AttainThreshold  <- 37
    ImpairThreshold  <- 22
    HBIThreshold     <- 7.7
    ShannonThreshold <- 2.5  
  }

  # attaining condition
  if(macroData$CO_MMI_2010[event] >= AttainThreshold) {
    macroData$GreyZone[event] <- F 
    macroData$Status[event] <- "Attain"
    macroData$Assessment[event] <- "Good"
  } 
  
  # greyzone condition
  if(macroData$CO_MMI_2010[event] >= ImpairThreshold & macroData$CO_MMI[event] < AttainThreshold) {
    macroData$GreyZone[event] <- T
    if(is.na(macroData$HBI[event]) | is.na(macroData$Shannon[event])) { 
        macroData$Status[event] <- "?"    #status is in grey zone and without auxiliary metrics, they can't be assessed
        } else {
      if(macroData$HBI[event] < HBIThreshold & macroData$Shannon[event] > 2.4){
        macroData$Status[event] <- "Attain"
        macroData$Assessment[event] <- "Concern"
      } else {
        macroData$Status[event] <- "Impaired"
        macroData$Assessment[event] <- "Poor"
        } 
      }
    }
   
  #impaired condition
  if(macroData$CO_MMI[event] < ImpairThreshold) {
    macroData$GreyZone[event] <- F 
    macroData$Status[event] <- "Impaired"
    macroData$Assessment[event] <- "Poor"
  }
} # end for loop

#remove all rows that could not be assessed
macroData <- macroData[macroData$Status!="?",] 

if(sum(macroData$Status %in% "Impaired") > 0){
   Exceedances <- TRUE 
   Impaired <- TRUE
  } else {
    Exceedances <- FALSE 
    Impaired <- FALSE
  }

Assessment <- ifelse(sum(macroData$Assessment %in% c("Poor","Concern"))>0,
                     ifelse(sum(macroData$Assessment %in% "Poor")>0,"Poor","Concern"),"Good")

return(c(Exceedances, Impaired, Assessment))
}


