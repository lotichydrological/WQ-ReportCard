library(zoo)
library(hydroTSM)

thirty_day_eval = function(Data, standard){
  valueType = get("valueType", envir = parent.frame()) #pass the valueType to the function
  
  if (length(unique(as.vector(Data$ActivityStartDate))) == 1) {
    MaxObs = max(Data$ResultMeasureValue, na.rm=TRUE)
    MedObs = median(Data$ResultMeasureValue, na.rm=TRUE)
    X85th = quantile(Data$ResultMeasureValue, c(0.85), na.rm=TRUE, names=FALSE)
  } else {
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
    rollingMean = rollapply(regularSeries, 30, mean, na.rm = TRUE) #calculate a rolling 30 day mean
    sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
    sampleData = rollingMean[sampleDates] #then pull only those dates from the regular time series
    
    #Calculate the summary statistics the way that WQCD does
    MaxObs = max(coredata(sampleData), na.rm=TRUE)
    MedObs = median(coredata(sampleData), na.rm=TRUE)
    X85th = quantile(coredata(sampleData), c(0.85), na.rm=TRUE, names=FALSE)
  }
  
  if (valueType == "Total" | valueType == "Total Residual") {
    Exceedances = ifelse(MaxObs > as.numeric(standard), TRUE, FALSE)
    Impaired = ifelse(MedObs > as.numeric(standard), TRUE, FALSE)
    
    Assessment = ifelse( !is.na(as.numeric(standard)),  
                         if(Impaired == TRUE){
                           "Poor"
                         }else if (MedObs >= 0.5*(as.numeric(standard)) & Exceedances == TRUE){
                           "Concern" 
                         }else if (MedObs >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
                           "Acceptable" 
                         }else{
                           "Good"
                         }, 
                         NA)
  }
  else{
    Exceedances = ifelse(MaxObs> as.numeric(standard), TRUE, FALSE)
    Impaired = ifelse(X85th > as.numeric(standard), TRUE, FALSE)
    Assessment = ifelse( !is.na(as.numeric(standard)),  
                         if(Impaired == TRUE){
                           "Poor"
                         }else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == TRUE){
                           "Concern" 
                         }else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
                           "Acceptable" 
                         }else{
                           "Good"
                         }, 
                         NA)
  }
  
 
  
  
  
  return(c(Exceedances, Impaired, Assessment))
}