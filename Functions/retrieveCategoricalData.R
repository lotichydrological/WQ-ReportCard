#' Retrieve categorical data for a water quality parameter
#' 
#' @description Retrieve categorical data for a desired water quality parameter from the water quality data frame.
#' Data are filtered to remove values below detection limits and unitless data, and blank values are removed.
#' 
#' @param Data Data frame containing raw water quality data.
#' @param characteristicNames Name of the water quality variable of interest.
#' @param Units Measurement units of the water quality variable of interest.
#' @param valueType Measurement type.
#' 
#' @return Data frame containing sample date and sample value.
#' 
#' @usage retrieveCategoricalData(Data, characteristicNames, Units, valueType)
#' 
#' @export

retrieveCategoricalData = function(Data, characteristicNames, Units, valueType){
  #reformat some of the data columns
  Data$ResultSampleFractionText[Data$ResultSampleFractionText=="Recoverable"] = "Total" 
  parameterNames = unlist(strsplit(characteristicNames, split = ";")) #Retrieve all the possible parameter names
  #parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
  Data$ResultMeasureValue[Data$ResultMeasureValue==""] = NA #convert blanks to NA  (This used to just be in retrieveNumericData)
  unitList = c("None", "", Units) #include 'None' and blank values as a unit to capture non-detects and unitless data like pH 
  dataSet = Data[Data$CharacteristicName %in% parameterNames & Data$ResultSampleFractionText %in% valueType & Data$ResultMeasure.MeasureUnitCode  %in% unitList,] #subset the data
  ResultMeasureValue = dataSet$ResultMeasureValue
  ActivityStartDate = dataSet$ActivityStartDate
  return(data.frame(ActivityStartDate, ResultMeasureValue))
}

