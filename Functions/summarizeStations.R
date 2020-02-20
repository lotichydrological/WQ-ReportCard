library(dplyr)

summarizeStations = function(stationID, segmentName){
  
  #First, aggregate data by segment and summarize
  #retrieve the data set for the segment
  Data = watershedData[watershedData$MonitoringLocationIdentifier == stationID,]
  if (length(Data)==0){
    return("No data available for this segment")
  } else {
  #Tidy up the data sheet to include only relevant columns
  Data = Data[,c("OrganizationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultValueTypeName", "DetectionQuantitationLimitMeasure.MeasureValue")]
  
  temperatureData = retrieveNumericalData(Data, c("Temperature", "Temperature, water"), "deg C", "")
  pHData = retrieveNumericalData(Data, "pH", "Std. Units", "")
  hardnessData = retrieveNumericalData(Data, c("Hardness, Ca, Mg as CaCO3","Hardness, Ca, Mg", "Total hardness -- SDWA NPDWR"), c("mg/l", "mg/l CaCO3"), "")
  
  #Retrieve the relevant WQCD water quality standards for the segment
  segmentStandards = wqStandards[,c("UseClass", "Category", "Indicator", "CharacteristicNames", "Units", "valueType", "standardType", segmentName)]
  #Then rename the column of water quality standards to 'standard'
  colnames(segmentStandards)[8] <- "standard"

  #loop through the list of water quality parameters and perform the report card summary functions on them
  output = segmentStandards %>% #use dplyr to add columns to the segmentStandards data frame
    mutate( #fix the order of the output rows to match the order of the input data
       UseClass =  factor(UseClass, levels = unique(wqStandards$UseClass)),
       Category =  factor(Category, levels = unique(wqStandards$Category))
       )%>%
    group_by(UseClass, Category, Indicator, Units, valueType) %>% #grouping the data into unique sets
    #then apply the function to each set
    do(summaryCalcs(.$UseClass, .$Category, .$Indicator, .$CharacteristicNames, .$Units, .$valueType, .$standardType, .$standard)) 
  
  output[output == -9999] = "" #clean the summary data frame by removing no data values
  
  #change any slashes in the segment or site name to a dash so they don't break the file path when writing
  stationID <- gsub("/","-", stationID)
  
  #write summary data to file
  x = c("./Output/", stationID, ".csv")
  outputName = paste(x, collapse='')
  write.table(output, file=outputName, sep=",", row.names=FALSE)
  print("Complete")
  return(output)

  }
}