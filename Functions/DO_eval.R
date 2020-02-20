library(zoo)
library(hydroTSM)

DO_eval = function(Data, standard){
  Data$ResultMeasureValue[Data$ResultMeasureValue==0] = NA #convert zeros to NA
  if (length(unique(as.vector(Data$ActivityStartDate))) == 1) {
    MinObs = min(Data$ResultMeasureValue, na.rm=TRUE)
    X15th = quantile(Data$ResultMeasureValue, c(0.15), na.rm=TRUE, names=FALSE)
  } else {
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
    rollingMean = rollapply(regularSeries, 30, mean, na.rm = TRUE) #calculate a rolling 30 day mean
    sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
    sampleData = rollingMean[sampleDates] #then pull only those dates from the regular time series
    
    #Calculate the summary statistics the way that WQCD does
    MinObs = min(coredata(sampleData), na.rm=TRUE)
    X15th = quantile(coredata(sampleData), c(0.15), na.rm=TRUE, names=FALSE)
  }
  Exceedances = ifelse(MinObs < as.numeric(standard), TRUE, FALSE)
  Impaired = ifelse(X15th < as.numeric(standard), TRUE, FALSE)
  Assessment = ifelse(Impaired == TRUE, "Poor", "Good")
  return(c(Exceedances, Impaired, Assessment))
}

