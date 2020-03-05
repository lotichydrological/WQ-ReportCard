library(zoo)
library(hydroTSM)

range_eval_validate = function(Data, standard){
  #print("Evaluating 'range' standard") #for debugging
  
  # if there is no wq standard on this reach, return NA's and break out of function
  if(is.na(standard) | standard =='') {
    return(c(NA, NA, NA))
  }
  
  Data$ResultMeasureValue[Data$ResultMeasureValue==0] = NA #convert zeros to NA
  Data$ResultMeasureValue[Data$ResultMeasureValue > 14] = NA #convert zeros to NA
  standardRange <- read.table(text=standard, sep=',')
 
  colnames(standardRange) = c("minStd","maxStd") #assign some column headings so we can call them later
  if (length(unique(as.vector(Data$ActivityStartDate))) == 1) {
    MinObs = min(Data$ResultMeasureValue, na.rm=TRUE)
    MaxObs = max(Data$ResultMeasureValue, na.rm=TRUE)
    X15th = quantile(Data$ResultMeasureValue, c(0.15), na.rm=TRUE, names=FALSE)
    X85th = quantile(Data$ResultMeasureValue, c(0.85), na.rm=TRUE, names=FALSE)
  } else {
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
    rollingMean = rollapply(regularSeries, 30, mean, na.rm = TRUE) #calculate a rolling 30 day mean
    sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
    sampleData = rollingMean[sampleDates] #then pull only those dates from the regular time series
    
    #Calculate the summary statistics the way that WQCD does
    MinObs = min(coredata(sampleData), na.rm=TRUE)
    MaxObs = max(coredata(sampleData), na.rm=TRUE)
    X15th = quantile(coredata(sampleData), c(0.15), na.rm=TRUE, names=FALSE)
    X85th = quantile(coredata(sampleData), c(0.85), na.rm=TRUE, names=FALSE)

  }
  Exceedances = ifelse(MinObs > as.numeric(standardRange$minStd) & MaxObs < as.numeric(standardRange$maxStd), FALSE, TRUE)
  Impaired = ifelse(X15th > as.numeric(standardRange$minStd) & X85th < as.numeric(standardRange$maxStd), FALSE, TRUE)
  Assessment = ifelse(!is.na(Impaired),
    ifelse(Impaired == TRUE, "Poor", "Good"),
    NA)
  return(c(Exceedances, Impaired, Assessment))
}