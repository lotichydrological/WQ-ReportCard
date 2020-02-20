################################################################
#  For debugging only
# UseClass <- "Human_Health"
# Category <- "Water_Supply"
# Indicator <-"Cadmium"
# CharacteristicNames <- "Cadmium;;;;;;;"
# Units <- "ug/l"
# valueType <-"Total"
# standardType <- "one_day"
# standard <- 5
# segmentName <- "COLCLC10_A"
# stationIDs = unique(as.vector(monitoringStations$StationID[monitoringStations$SegmentID == segmentName]))
# stationIDs
# 
# Data = watershedData[watershedData$MonitoringLocationIdentifier %in% stationIDs,]
# #Tidy up the data sheet to include only relevant columns
# Data = Data[,c("OrganizationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultValueTypeName", "DetectionQuantitationLimitMeasure.MeasureValue")]
# 
# Data = Data[,c("OrganizationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultValueTypeName", "DetectionQuantitationLimitMeasure.MeasureValue")]
# 
# temperatureData = retrieveNumericalData(Data, c("Temperature", "Temperature, water"), "deg C", "")
# pHData = retrieveNumericalData(Data, "pH", "Std. Units", "")
# hardnessData = retrieveNumericalData(Data, c("Hardness, Ca, Mg as CaCO3","Hardness, Ca, Mg", "Total hardness -- SDWA NPDWR", "Hardness"), c("mg/l", "mg/l CaCO3"), "")
# 
# #Retrieve the relevant WQCD water quality standards for the segment
# 
# #remove the reach designator from the segmentID so that the program will just grab the standards assigned to the larger
# #WBID (305b segment), not the smaller AUID (305b reach)
# segmentWBID <- gsub("_.*","",segmentName)
# 
# segmentStandards = wqStandards[,c("UseClass", "Category", "Indicator", "CharacteristicNames", "Units", "valueType", "standardType", segmentWBID)]
# #Then rename the column of water quality standards to 'standard'
# colnames(segmentStandards)[8] <- "standard"
# 
# # put the standard evaluation function you want to debug here
# debug(one_day_eval)
# 
# 
# # End debugging section
#################################################################################################################################################

summaryCalcs = function(UseClass, Category, Indicator, CharacteristicNames, Units, valueType, standardType, standard){
  segmentName <- get("segmentName", envir = parent.frame())
  Data <- get("Data", envir = parent.frame()) #the full data set for the segment
  temperatureData <- get("temperatureData", envir = parent.frame()) #temperature data time series
  pHData <- get("pHData", envir = parent.frame()) #pH data time series
  hardnessData <- get("hardnessData", envir = parent.frame()) #hardness data time series
  

# # # Various switches that turn on/off for debugging at a certain indicator and segmment during the looping
#   if(UseClass=="Aquatic_Life" & Indicator=="Chlorophyll_a"){
#     # debug(retrieveNumericalData)
#     # debug(standardize_units)}
#     debug(summaryCalcs)
#     # debug(standardize_units)
#     # debug(annual_median_eval)
# #  else {
#   #undebug(retrieveNumericalData)
# #    undebug(summaryCalcs)
# #    undebug(hardness_calc_eval)
#    }
# # #   
  
 
  numericalData = retrieveNumericalData(Data, CharacteristicNames, Units, valueType) #data of interest formatted as numerical time series
  categoricalData = retrieveCategoricalData(Data, CharacteristicNames, Units, valueType) #data of interest formatted as categorical time series 
    
  n = max(nrow(categoricalData), nrow(numericalData), na.rm=T)

  Organization = orgList(CharacteristicNames, Units, valueType)
  
  Censored = if (n > 0 & Indicator == "Temperature"){
    0
  } else if (n > 0 & standardType != "severity" & standardType != "manual"){
    sum(numericalData$ResultMeasureValue==0)
  } else -9999
  
  Minimum = if (n > 0 & Censored < n & standardType != "severity" & standardType != "manual") {
    min(numericalData$ResultMeasureValue, na.rm=TRUE)
  } else -9999
  
  Median = if (n > 0 & Censored < n & standardType != "severity" & standardType != "manual") {
    median(numericalData$ResultMeasureValue, na.rm=TRUE)
  } else -9999
  
  Maximum = if (n > 0 & Censored < n & standardType != "severity" & standardType != "manual") {
    max(numericalData$ResultMeasureValue, na.rm=TRUE)
  } else -9999
  
  Severity = if (n > 0 & standardType == "severity") {
    findSeverity(categoricalData$ResultMeasureValue)
  } else ""
  
  DateOfMax = if (Maximum > 0) {
    maxDate(CharacteristicNames, Units, valueType, Maximum) 
  } else ""
  
  Percentile_15th = if (Maximum > 0) { #find the 15th percentile of the observations
    quantile(numericalData$ResultMeasureValue, c(0.15), na.rm=TRUE, names=FALSE)
  } else -9999
  
  Percentile_85th = if (Maximum > 0) { #find the 85th percentile of the observations
    quantile(numericalData$ResultMeasureValue, c(0.85), na.rm=TRUE, names=FALSE) 
  } else -9999

if (n > 0 & standardType != "severity" & standardType != "manual"){
  trendParameters = calculateTrend(numericalData)
  Kendal_Slope = trendParameters[1]
  Kendal_pValue = trendParameters[2]
}else{
  Kendal_Slope = -9999
  Kendal_pValue = -9999
}
 
  # Console status update, for troubleshooting if crashing to see where/when
  print(paste("Currently assessing", failparameter, "for", UseClass, "use."))
  
  computeStandards = if (n > 0 & standardType == "severity"){
    #read in the type of function required as indicated by the standardType
    function_call = paste(c(standardType, "_eval", "(Severity)"), collapse="") #evaluate on the Severity ranking
    eval(parse(text=function_call)) #execute the appropriate function
  } else if (standardType == "manual"){
    c("", "", "") #return an empty vector so that it can be populated manually
  } else if (Maximum > 0){
    #read in the type of function required as indicated by the standardType
    function_call = paste(c(standardType, "_eval", "(numericalData, standard)"), collapse="") #evaluate on the numerical data
    eval(parse(text=function_call)) #execute the appropriate function
  } else if (Censored == n | Maximum == 0){ #if all values are all non-detects or extremely low concentrations
    c("", "", "Poor Resolution") #return an vector indicating that all values are below detection 
  } else c("", "", "Data Gap") #return a vector indicating the data gap
  
  
  
  StandardsExceeded = as.character(computeStandards[1])
  Impaired = as.character(computeStandards[2])
  Assessment = as.character(computeStandards[3])
  
  evaluation = data.frame(
    n, 
    Censored, 
    Organization, 
    Minimum, 
    Median, 
    Maximum,
    Severity,
    DateOfMax,
    Percentile_15th,
    Percentile_85th,
    Kendal_Slope,
    Kendal_pValue,
    standard,
    StandardsExceeded,
    Impaired,
    Assessment
    )
  return(evaluation)
}