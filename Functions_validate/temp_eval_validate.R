library(zoo)

temp_eval_validate = function(Data, standard){
  segmentName = get("segmentName", envir = parent.frame()) #access the segment name
  standardPair = read.table(text=standard, sep=',') 
  summerStandard =  as.numeric(standardPair[1])
  winterStandard =  as.numeric(standardPair[2])
  colnames(standardPair) = c("summerStandard","winterStandard")
  #evaluate acute exceedances of the temperature standard here
  if (segmentName == "COUCEA09b_6300" | segmentName == "COUCEA09c_6300"){
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    summerData = irregularSeries[months(time(irregularSeries), TRUE) %in% c("Apr", "May", "Jun"," Jul", "Aug", "Sep", "Oct")]#extract only the summer data
    winterData = irregularSeries[months(time(irregularSeries), TRUE) %in% c("Nov","Dec","Jan","Feb","Mar")]#extract only the winter data
    if (length(coredata(summerData)) >= 1) {
      summerMax = max(coredata(summerData), na.rm = TRUE)
      summer85th = quantile(coredata(summerData), c(0.85), na.rm=TRUE, names=FALSE)
    } else {
      summerMax = 0
      summer85th = 0
    }
    if (length(coredata(winterData)) >= 1) {
      winterMax = max(coredata(winterData), na.rm = TRUE)
      winter85th = quantile(coredata(winterData), c(0.85), na.rm=TRUE, names=FALSE)
    } else {
      winterMax = 0
      winter85th = 0
    }  
  } else {
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    summerData = irregularSeries[months(time(irregularSeries), TRUE) %in% c("Jun"," Jul", "Aug", "Sep")]#extract only the summer data
    winterData = irregularSeries[months(time(irregularSeries), TRUE) %in% c("Oct", "Nov","Dec","Jan","Feb","Mar", "Apr", "May")]#extract only the winter data
    if (length(coredata(summerData)) >= 1) {
      summerMax = max(coredata(summerData), na.rm = TRUE)
      summer85th = quantile(coredata(summerData), c(0.85), na.rm=TRUE, names=FALSE)
    } else {
      summerMax = 0
      summer85th = 0
    }
    if (length(coredata(winterData)) >= 1) {
    winterMax = max(coredata(winterData), na.rm = TRUE)
    winter85th = quantile(coredata(winterData), c(0.85), na.rm=TRUE, names=FALSE)
    } else {
      winterMax = 0
      winter85th = 0
    }
  }
  Exceedances = ifelse(summerMax > as.numeric(summerStandard) | winterMax > as.numeric(winterStandard), TRUE, FALSE)
  Impaired = ifelse(summer85th > as.numeric(summerStandard) | winter85th > as.numeric(winterStandard), TRUE, FALSE)
  Assessment = if(Impaired == TRUE){
    "Poor"
  }else if (summer85th >= 0.5*(as.numeric(summerStandard)) & Exceedances == TRUE || winter85th >= 0.5*(as.numeric(winterStandard)) & Exceedances == TRUE){
    "Concern" 
  }else if (summer85th >= 0.5*(as.numeric(summerStandard)) & Exceedances == FALSE || winter85th >= 0.5*(as.numeric(winterStandard)) & Exceedances == FALSE){
    "Acceptable" 
  }else{
    "Good"
  }
  return(c(Exceedances, Impaired, Assessment))
}