#' Create boxplot of water quality sample results
#' 
#' @description Creates a seasonal boxplot of water quality sample results
#' 
#' @param watershedData Data frame of all water quality samples generated for the specified watershed using getDataByHUC8 function or manually.
#' @param UseClass Class type of water quality data
#' @param Indicator Desired water quality parameter name
#' @param Category Subcategory of water quality data type
#' @param segmentName 305(b) reach AUID for desired stream segment
#' @param StartDate YYYY-MM-DD desired start date
#' @param EndDate YYYY-MM-DD desired end date
#' 
#' @return ggplot boxplot showing sample results grouped by season with standard highlighted
#' 
#' @usage buildBoxplot(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate)
#' 
#' @export

########################################################
## Debugging section
# UseClass <- "Human_Health"
# segmentName <- "COLCLC10_A"
# Indicator <-"Cadmium"
# characteristicNames <- "Cadmium;;;;;;;"
# Units <- "ug/l"
# standard <- 5
# valueType <-"Total"
# StartDate <- ('2008-01-01') # YYYY-MM-DD This is the start date for the entire data set you want.
# EndDate <- ('2015-12-31') # YYYY-MM-DD This is the end date for the entire data set you want.

library(ggplot2)
library(lubridate)

buildBoxplot <- function(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate){
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
  
  # Add season to dataset
  dataset$Season <- ifelse((month(dataset$ActivityStartDate)) %in% 3:5,"Spring",
                           ifelse((month(dataset$ActivityStartDate)) %in% 6:8,"Summer",
                                  ifelse((month(dataset$ActivityStartDate)) %in% 9:11,"Fall",
                                         ifelse((month(dataset$ActivityStartDate)) %in% c(12,1,2),"Winter","none"
                                         ))))
  
  # retrieve standard
  standard <- standardCalc(UseClass, Indicator, Category, segmentWBID, eval_type)
  
  
  # build boxplot
  if(nrow(dataset) == 0){
    bp <- ggplot(dataset) + geom_blank()
    bp <- bp + ggtitle("No data available")
    bp <- bp + theme(plot.title = element_text(color="red", size=14, face="bold.italic"))
    bp
  }else{
    bp <- ggplot(dataset, aes(factor(Season),ResultMeasureValue, fill = Season)) # + stat_boxplot(geom ='errorbar')
    bp <- bp + geom_violin() + geom_jitter(height = 0, width = 0.1)
    bp <- bp + scale_fill_manual(values=c("darkorange1","chartreuse2", "orangered3", "darkslategray2"))
    bp <- bp + labs(x = "Season", y = Units)
    bp <- bp + geom_hline(yintercept = standard, linetype = "dashed",size = 1.5, color = "red")
    bp <- bp + geom_text(aes(0, standard,label = "Standard", hjust = -0.5, vjust = 1.5, size = 12,color = "red", face = "bold"))
    bp <- bp + ggtitle("Seasonal distribution of \naggregated sample data")
    bp <- bp + guides(fill = guide_legend(override.aes = list(linetype = "blank")))
    bp <- bp + theme_bw() + theme(
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"),
      axis.text = element_text(size = 12, face = "bold"),
      panel.border = element_rect(colour = "black", fill=NA, size=.8),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none" 
    )
    bp 
  }
  
  
  return(bp)
}

