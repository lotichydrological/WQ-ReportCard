#' Retrieve invertebrate sample data 
#' 
#' @description Retrieve invertebrate data from the water quality data frame.
#' Data are filtered to remove values below detection limits and unitless data, and blank values are removed.
#' 
#' @param Data Data frame containing raw water quality data.
#' @param characteristicNames Name of the variable of interest.
#' @param Units Measurement units of the variable of interest.
#' @param valueType Measurement type.
#' 
#' @return Data frame containing monitoring location, sampling date, water quality variable, and sample value.
#' 
#' @usage retrieveInvertebrateData(Data, characteristicNames, Units, valueType)
#' 
#' @export

retrieveInvertebrateData = function(Data, characteristicNames, Units, valueType){
  #reformat some of the data columns
  Data$ActivityStartDate = as.Date(strptime(as.character(Data$ActivityStartDate), "%Y-%m-%d")) #format as dates
  Data$ResultSampleFractionText[Data$ResultSampleFractionText=="Recoverable"] = "Total" 
  Data$ResultMeasureValue =  as.numeric(as.character(Data$ResultMeasureValue)) #convert columns to numeric
  Data$ResultMeasureValue[Data$ResultMeasureValue==""] = NA #convert blanks to NA
  Data$ResultMeasureValue[Data$DetectionQuantitationLimitMeasure.MeasureValue >= Data$ResultMeasureValue] = NA #convert values below lower detection or reporting limits to NA
  parameterNames = unlist(strsplit(characteristicNames, split = ";")) #Retrieve all the possible parameter names
  parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
  unitList = c("None", "", Units) #include 'None' and blank values as a unit to capture non-detects and unitless data like pH 
  dataSet = Data[Data$CharacteristicName %in% parameterNames & Data$ResultSampleFractionText %in% valueType & Data$ResultMeasure.MeasureUnitCode  %in% unitList,] #subset the data
  dataSet$ResultMeasureValue[is.na(dataSet$ResultMeasureValue)] = 0 #convert blanks values to zero as per WQCD regulations
  
  #remove spaces to simplify variable names for casting data into wide form later
  dataSet$CharacteristicName <- as.character(dataSet$CharacteristicName)
  dataSet$CharacteristicName[dataSet$CharacteristicName == "Shannon Diversity"] <- "Shannon"
  
  ResultMeasureValue = as.numeric(as.character(dataSet$ResultMeasureValue))
  ActivityStartDate = dataSet$ActivityStartDate
  CharacteristicName = dataSet$CharacteristicName
  MonitoringLocationIdentifier = dataSet$MonitoringLocationIdentifier
  
  return(data.frame(MonitoringLocationIdentifier, ActivityStartDate, CharacteristicName, ResultMeasureValue))
}