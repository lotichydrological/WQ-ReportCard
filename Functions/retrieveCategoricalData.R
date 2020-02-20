retrieveCategoricalData = function(Data, characteristicNames, Units, valueType){
  #reformat some of the data columns
  Data$ResultSampleFractionText[Data$ResultSampleFractionText=="Recoverable"] = "Total" 
  parameterNames = unlist(strsplit(characteristicNames, split = ";")) #Retrieve all the possible parameter names
  #parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
  Data$ResultMeasureValue[Data$ResultMeasureValue==""] = NA #convert blanks to NA  (This used to just be in retrieveNumericData)
  unitList = c("None", "", Units) #include 'None' and blank values as a unit to capture non-detects and unitless data like pH 
  # remove emtpy rows so they don't interfere with retrieveNumericalData
  #Data <- Data[Data$ResultMeasureValue!="",]
  dataSet = Data[Data$CharacteristicName %in% parameterNames & Data$ResultSampleFractionText %in% valueType & Data$ResultMeasure.MeasureUnitCode  %in% unitList,] #subset the data
  ResultMeasureValue = dataSet$ResultMeasureValue
  ActivityStartDate = dataSet$ActivityStartDate
  return(data.frame(ActivityStartDate, ResultMeasureValue))
}

