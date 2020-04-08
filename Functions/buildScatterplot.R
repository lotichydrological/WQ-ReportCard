#' Create scatterplot of water quality sample results
#' 
#' @description Creates a time series scatterplot of water quality sample results
#' 
#' @param watershedData Data frame of all water quality samples generated for the specified watershed using getDataByHUC8 function or manually.
#' @param UseClass Class type of water quality data
#' @param Indicator Desired water quality parameter name
#' @param Category Subcategory of water quality data type
#' @param segmentName 305(b) reach AUID for desired stream segment
#' @param StartDate YYYY-MM-DD desired start date
#' @param EndDate YYYY-MM-DD desired end date
#' 
#' @return ggplot scatterplot showing sample results over time with standard highlighted
#' 
#' @usage buildScatterplot(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate)
#' 
#' @export

########################################################
## Debugging section
# UseClass <- "Human_Health"
# segmentName <- "COLCLC10_A"
# Indicator <-"Cadmium"
# CharacteristicNames <- "Cadmium;;;;;;;"
# Units <- "ug/l"
# standard <- 5
# valueType <-"Total"
# StartDate <- ('2008-01-01') # YYYY-MM-DD This is the start date for the entire data set you want.
# EndDate <- ('2015-12-31') # YYYY-MM-DD This is the end date for the entire data set you want.

library(ggplot2)
library(lubridate)
library(directlabels)

buildScatterplot <- function(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate){
  # call required parent data frames 
  monitoringStations <- get("monitoringStations", envir = parent.frame())
  wqStandards <- get("wqStandards", envir = parent.frame())
  
  # create list of station IDs to select data 
  stationIDs = unique(as.vector(monitoringStations$MonitoringLocationID[
    tolower(monitoringStations$SegmentID) == tolower(segmentName)]))
  
  #remove the reach designator from the segmentID so that the program will just grab the standards assigned to the
  # WBID (305b segment), not the smaller AUID (305b reach)
  segmentWBID <- gsub("_.*","",segmentName)
  
  # retrieve all data for the segment
  Data = watershedData[watershedData$MonitoringLocationIdentifier %in% stationIDs,]
  
  # filter dataset to desired date range
  Data <- Data[date(Data$ActivityStartDate) >= as.Date(StartDate, "%m-%d-%Y") & date(Data$ActivityStartDate) <= as.Date(EndDate, "%m-%d-%Y"),]
  
  #retrieve associated datasets for standards calculation on metals and ammonia
  temperatureData = retrieveNumericalData(Data, c("Temperature", "Temperature, water"), "deg C", "")
  pHData = retrieveNumericalData(Data, "pH", "Std. Units", "")
  hardnessData = retrieveNumericalData(Data, c("Hardness, Ca, Mg as CaCO3","Hardness, Ca, Mg", "Total hardness -- SDWA NPDWR", "Hardness"), c("mg/l", "mg/l CaCO3"), "")
  
  # retrieve characteristic names
  characteristicNames <- wqStandards[wqStandards$Indicator == Indicator & wqStandards$Category == Category & wqStandards$UseClass == UseClass, "CharacteristicNames"]
  
  # retrieve valueType
  valueType <- wqStandards[wqStandards$Indicator == Indicator & wqStandards$Category == Category & wqStandards$UseClass == UseClass, "valueType"]
  
  # retrieve evaluation type
  eval_type <- wqStandards[wqStandards$Indicator == Indicator & wqStandards$Category == Category & wqStandards$UseClass == UseClass, "standardType"]
  
  # retrieve units 
  Units <- wqStandards[wqStandards$Indicator == Indicator & wqStandards$Category == Category & wqStandards$UseClass == UseClass, "Units"]
  
  # retrieve relevent data 
  dataset <- retrieveNumericalData(Data, characteristicNames, Units, valueType)  
  
  # remove zeroes
  # dataset <- dataset[dataset$ResultMeasureValue != 0.0,]
  
  # retrieve standard
  standard <- standardCalc(UseClass, Indicator, Category, segmentWBID, eval_type)
  
  
  # build scatterplot
  if(nrow(dataset) == 0){
    sp <- ggplot(dataset) + geom_blank()
    sp <- sp + ggtitle("No data available")
    sp <- sp + theme(plot.title = element_text(color="red", size=14, face="bold.italic"))
    sp
  }else{
    sp <- ggplot(dataset, aes(x = ActivityStartDate, y = ResultMeasureValue)) 
    if(nrow(dataset) > 25) {
      sp <- sp + geom_smooth(se=FALSE)
      sp <- sp + geom_dl(aes(label = "Trendline**"), color = "blue",method = "smart.grid")
    }
    sp <- sp + geom_point(size = 2)
    sp <- sp + geom_hline(yintercept = standard, linetype = "dashed",size = 1.5, color = "red")
    sp <- sp + geom_text(aes(x = StartDate, y=standard, hjust = -0.25, vjust=1.5,label = "Standard", size = 12,color = "red", face = "bold"))
    sp <- sp + ggtitle("Measured concentration \nthrough time")
    sp <- sp + labs(x = "Date", y = Units)
    sp <- sp + theme_bw() + theme(
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"),
      axis.text = element_text(size = 12, face = "bold"),
      panel.border = element_rect(color = "black", fill=NA, size=.8),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none" 
    )
    sp 
  }

  
  return(sp)
}
