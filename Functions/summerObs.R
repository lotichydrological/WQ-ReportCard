summerObs = function(dataSet, SampleDates){
  samples = data.frame("SampleDates"=dataSet[,c("ActivityStartDate")], Results=as.numeric(dataSet[,c("ResultMeasureValue")]), stringsAsFactors = FALSE)
  summerDates = read.zoo(SampleDates) #convert to zoo object
  summerDates = as.data.frame(summerDates[months(time(summerDates), TRUE) %in% c("Jul", "Aug", "Sep")])
  summerObs = merge(summerDates, samples, by="SampleDates", all=FALSE) #map sampling events over summer observation period  
  return(summerObs)
}