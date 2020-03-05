censoredValues_validate = function(dataSet){
  dataSet$ResultMeasureValue[dataSet$ResultMeasureValue==""] = NA #convert blanks to NA
  dataSet$ResultMeasureValue[dataSet$ResultMeasureValue=="0"] = NA #convert zeros to NA to account for RiverWatch BDL values == 0
  #count the number of censored values
  n = sum(is.na(dataSet$ResultMeasureValue))
  return(n)
}