#' Retrieve numerical data for a water quality parameter
#' 
#' @description Retrieve numerical data for a desired water quality parameter from the water quality data frame.
#' Data are filtered to remove values below detection or reporting limits, and blank values are converted to zero as per WQCD regulations.
#' 
#' @param Data Data frame containing raw water quality data.
#' @param characteristicNames Name of the water quality variable of interest.
#' @param Units Measurement units of the water quality variable of interest.
#' @param valueType Measurement type.
#' 
#' @return Data frame containing sampling dates and results.
#' 
#' @usage retrieveNumericalData(Data, characteristicNames, Units, valueType)
#' 
#' @export

retrieveNumericalData = function(Data, characteristicNames, Units, valueType){
  #reformat some of the data columns
  Data$ActivityStartDate = as.Date(strptime(as.character(Data$ActivityStartDate), "%Y-%m-%d")) #format as dates
  Data$ResultSampleFractionText[Data$ResultSampleFractionText=="Recoverable"] = "Total" 
  Data$ResultMeasureValue =  as.numeric(as.character(Data$ResultMeasureValue)) #convert columns to numeric
  Data$ResultMeasureValue[Data$ResultMeasureValue==""] = NA #convert blanks to NA
  Data$ResultMeasureValue[Data$DetectionQuantitationLimitMeasure.MeasureValue >= Data$ResultMeasureValue] = NA #convert values below lower detection or reporting limits to NA
  parameterNames = unlist(strsplit(characteristicNames, split = ";")) #Retrieve all the possible parameter names
  parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
  unitList = c("None", "", Units) #include 'None' and blank values as a unit to capture non-detects and unitless data like pH
  Data <- standardize_units(Data) # make sure nutrients are all in correct inified unit values for standards assessment
  dataSet = Data[Data$CharacteristicName %in% parameterNames & Data$ResultSampleFractionText %in% valueType & Data$ResultMeasure.MeasureUnitCode  %in% unitList,] #subset the data
  dataSet$ResultMeasureValue[is.na(dataSet$ResultMeasureValue)] = 0 #convert blanks values to zero as per WQCD regulations
  ResultMeasureValue = as.numeric(as.character(dataSet$ResultMeasureValue))
  
  ActivityStartDate = dataSet$ActivityStartDate
  CharacteristicName = dataSet$CharacteristicName
  MonitoringLocationIdentifier = dataSet$MonitoringLocationIdentifier
  auxMetric_list <- c("HBI","Shannon Diversity")
  return(data.frame(ActivityStartDate, ResultMeasureValue))
}
