#' Create table of water quality sample results
#' 
#' @description Creates a table of summary statistics and assesses impairment
#' 
#' @param watershedData Data frame of all water quality samples generated for the specified watershed using getDataByHUC8 function or manually.
#' @param UseClass Class type of water quality data
#' @param Indicator Desired water quality parameter name
#' @param Category Subcategory of water quality data type
#' @param segmentName 305(b) reach AUID for desired stream segment
#' @param StartDate YYYY-MM-DD desired start date
#' @param EndDate YYYY-MM-DD desired end date
#' 
#' @return formattable table object showing key summary statistics and impairment assessment
#' 
#' @usage getSummaryStats(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate)
#' 
#' @export

library(htmlwidgets)

getSummaryStats <- function(watershedData, UseClass, Indicator, Category, segmentName, StartDate, EndDate){
  
  # call required parent data frames 
  monitoringStations <- get("monitoringStations", envir = parent.frame())
  wqStandards <- get("wqStandards", envir = parent.frame())
  
  if(is.null(segmentName)){
    df <- data.frame("Select a segment")
    names(df)[1] <- "No segment selected"
    color_table <- formattable(df, align = "c", list(
      area(col = length(colnames(df))) ~ formatter("span", style = x ~ style(display = "block",
                                                                                     "border-radius" = "4px",
                                                                                     "padding-right" = "4px",
                                                                                     "background-color"="white",
                                                                                     width = "240px"))))
  }
  else{
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
  
  # retrieve standard 
  standard <- wqStandards[wqStandards$Indicator == Indicator & wqStandards$Category == Category & wqStandards$UseClass == UseClass, segmentWBID]
  
  # retrieve relevent data 
  dataset <- retrieveNumericalData(Data, characteristicNames, Units, valueType)  
  
  # remove zeroes
  dataset <- dataset[dataset$ResultMeasureValue != 0.0,]
  
  # generate summary statistics
  evaluation <- summaryCalcs(UseClass, Category, Indicator, characteristicNames, Units, valueType, eval_type, standard)
  
  # drop kendall slope columns
  drops <- c("Kendal_pValue","Kendal_Slope", "Severity")
  evaluation <- evaluation[,!(names(evaluation) %in% drops) ]
  
  # retrieve standard
  standard <- signif(standardCalc(UseClass, Indicator, Category, segmentWBID, eval_type), digits = 4)
  
  # overwrite data frame standard with calculated value (for hardness/ammonia calcs)
  ifelse(eval_type == "range", evaluation$standard <- evaluation$standard ,evaluation$standard <- standard)
  
  # rename columns for good data 
  names(evaluation)[1] <- "Number of samples"
  names(evaluation)[2] <- "Number of censored \n samples"
  names(evaluation)[4] <- paste("Minimum (",Units,")",sep = "")
  names(evaluation)[5] <- paste("Median (",Units,")",sep = "")
  names(evaluation)[6] <- paste("Maximum (",Units,")",sep = "")
  names(evaluation)[7] <- "Date of maximum \n value"
  names(evaluation)[8] <- paste("15th Percentile (",Units,")",sep = "")
  names(evaluation)[9] <- paste("85th Percentile (",Units,")",sep = "")
  names(evaluation)[10] <- paste("Standard* (",Units,")",sep = "")
  names(evaluation)[11] <- "Standards Exceeded"
  
  # reorder columns to place summary stats next to each other
  evaluation <- evaluation[c(1,2,3,4,8,5,9,6,7,10,11,12,13)]
  
  # transpose data 
  evaluation <- as.data.frame(t(evaluation))
  
  # rename column after indicator
  names(evaluation)[1] <- Indicator
  
  # separate organizations into new rows
  evaluation[1]<- sapply(evaluation[1], function(x) { gsub("[,]", "\n", x) })
  
  # define conditional formatting for cell backgrounds
  bg.picker <- function(z){
    if(is.na(z)){return("white");
    } else if(z == "Concern"){return("yellow");
    } else if(z == "Poor"){return("red");
    } else if(z == "Acceptable"){return("lightgreen");
    } else if(z == "Poor Resolution"){return("lightgrey");
    } else if(z == "Good"){return("green");  
    } else {return("white");
    }
  }
  
  # create formatted table of all data
  color_table <- formattable(evaluation, align = "c", list(
    area(col = length(colnames(evaluation))) ~ formatter("span", style = x ~ style(display = "block",
                                                                             "border-radius" = "4px",
                                                                             "padding-right" = "4px",
                                                                             "background-color"= sapply(x,bg.picker),
                                                                             width = "100px"))
    ))
  }
  
  # export as html widget
  # html_w <- as.htmlwidget(color_table,width = "100%", height = NULL)
  
  # return html widget for display
  return(color_table)
}
