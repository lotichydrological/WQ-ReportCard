retrieveNumericalData_validate = function(Data, characteristicNames, Units, valueType){
  #reformat some of the data columns
  Data$ActivityStartDate = as.Date(strptime(as.character(Data$ActivityStartDate), "%Y-%m-%d")) #format as dates
  Data$ResultSampleFractionText[Data$ResultSampleFractionText=="Recoverable"] = "Total" 
  Data$ResultMeasureValue =  as.numeric(as.character(Data$ResultMeasureValue)) #convert columns to numeric
  Data$ResultMeasureValue[Data$ResultMeasureValue==""] = NA #convert blanks to NA
  Data$ResultMeasureValue[Data$DetectionQuantitationLimitMeasure.MeasureValue >= Data$ResultMeasureValue] = NA #convert values below lower detection or reporting limits to NA
  parameterNames = unlist(strsplit(characteristicNames, split = ";")) #Retrieve all the possible parameter names
  parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
  unitList = c("None", "", Units) #include 'None' and blank values as a unit to capture non-detects and unitless data like pH
  Data <- standardize_units_validate(Data) # make sure nutrients are all in correct inified unit values for standards assessment
  dataSet = Data[Data$CharacteristicName %in% parameterNames & Data$ResultSampleFractionText %in% valueType & Data$ResultMeasure.MeasureUnitCode  %in% unitList,] #subset the data
  dataSet$ResultMeasureValue[is.na(dataSet$ResultMeasureValue)] = 0 #convert blanks values to zero as per WQCD regulations
  ResultMeasureValue = as.numeric(as.character(dataSet$ResultMeasureValue))
  
  ActivityStartDate = dataSet$ActivityStartDate
  CharacteristicName = dataSet$CharacteristicName
  MonitoringLocationIdentifier = dataSet$MonitoringLocationIdentifier
  auxMetric_list <- c("HBI","Shannon Diversity")
  #if(sum(parameterNames %in% auxMetric_list)>0) {return(data.frame(MonitoringLocationIdentifier, ActivityStartDate, CharacteristicName, ResultMeasureValue))}
  #else {return(data.frame(ActivityStartDate, ResultMeasureValue))}
  return(data.frame(ActivityStartDate, ResultMeasureValue))
}
