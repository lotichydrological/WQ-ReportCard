library(dplyr)

summarizeSegments_validate = function(segmentName, monitoringStations){
  
  #Aggregate data by segment and summarize
  
  #retrieve the data set for the segment
  Data = watershedData[watershedData$MonitoringLocationIdentifier %in% monitoringStations,]
  if (length(Data)==0){
    return("No data available for this segment")
  } else {
  
  #Tidy up the data sheet to include only relevant columns
  Data = Data[,c("OrganizationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", "MonitoringLocationIdentifier", "ResultDetectionConditionText", "CharacteristicName", "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "ResultValueTypeName", "DetectionQuantitationLimitMeasure.MeasureValue")]
  
  #retrieve associated datasets for standards calculation on metals and ammonia
  temperatureData = retrieveNumericalData_validate(Data, c("Temperature", "Temperature, water"), "deg C", "")
  pHData = retrieveNumericalData_validate(Data, "pH", "Std. Units", "")
  hardnessData = retrieveNumericalData_validate(Data, c("Hardness, Ca, Mg as CaCO3","Hardness, Ca, Mg", "Total hardness -- SDWA NPDWR", "Hardness"), c("mg/l", "mg/l CaCO3"), "")
  
  
  #remove the reach designator from the segmentID so that the program will just grab the standards assigned to the
  # WBID (305b segment), not the smaller AUID (305b reach)
  segmentWBID <- gsub("_.*","",segmentName)
  
  #Retrieve the relevant WQCD water quality standards for the segment
  segmentStandards = wqStandards[,c("UseClass", "Category", "Indicator", "CharacteristicNames", "Units", "valueType", "standardType", segmentWBID)]
  
  #rename the column of water quality standards to 'standard'
  colnames(segmentStandards)[8] <- "standard"

  #loop through the list of water quality parameters and perform the report card summary functions on them
  output = segmentStandards %>% #use dplyr to add columns to the segmentStandards data frame
    mutate( #fix the order of the output rows to match the order of the input data
       UseClass =  factor(UseClass, levels = unique(wqStandards$UseClass)),
       Category =  factor(Category, levels = unique(wqStandards$Category))
       )%>%
    group_by(UseClass, Category, Indicator, Units, valueType) %>% #grouping the data into unique sets
    #then apply the function to each set
    do(summaryCalcs_validate(.$UseClass, .$Category, .$Indicator, .$CharacteristicNames, .$Units, .$valueType, .$standardType, .$standard)) 
  
  output[output == -9999] = "" #clean the summary data frame by removing no data values

  
  # [Don't have to these everytime after the figures have been plotted, will slow it down unnecessarily
  
  findSampleLocations_validate(segmentName, Data) #create summary tables for the samples collected on each segment
  
  #plotDischarge(HUC10, segmentName, Data) #create hydrographs and sample dates figure for each segment
  plotDischarge_validate(segmentName, Data) #create hydrographs and sample dates figure for each segment
  
  #summaryBarCharts(HUC10, segmentName, output)
  summaryBarCharts_validate(segmentName, output)
  
  #change any slashes in the segment or site name to a dash so they don't break the file path when writing
  # for example, Reg 85 sites names with 'U/S' and 'D/S' will be fixed
  segmentName <- gsub("/","-", segmentName)
  
  #write summary data to file
  x = c("./Output/", segmentName, ".csv")
  outputName = paste(x, collapse='')
  write.table(output, file=outputName, sep=",", row.names=FALSE)
  print("Complete")
  return(output)

  }
}