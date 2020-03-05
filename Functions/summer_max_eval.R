#' Evaluate summer maximum values from sample data
#' 
#' @description Evaluate summer maximum values from the water quality data frame. Only samples from July, August, and September are considered.
#' Summary statistics are then calculated as per WQCD guidance. Determines if exceedances exist in the dataset, if the reach is impaired, and produces an 
#' assessment of the reach for that parameter.
#' 
#' @param Data Data frame containing the raw water quality data.
#' @param standard Water quality standard for the given parameter
#' 
#' @return List stating if standards have been exceeded, if the reach is impaired, and the assessment for that parameter in that reach.
#' 
#' @usage summer_max_eval(Data, standard)
#' 
#' @export 

library(zoo)
library(hydroTSM)

summer_max_eval = function(Data, standard){
  if (length(unique(as.vector(Data$ActivityStartDate))) == 1) {
    MaxObs = max(Data$ResultMeasureValue, na.rm=TRUE)
    X85th = quantile(Data$ResultMeasureValue, c(0.85), na.rm=TRUE, names=FALSE)
  } else {
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
    rollingMean = rollapply(regularSeries, 365, function(x) median(x, na.rm=TRUE), fill = NA) #calculate a rolling annual day mean
    summerData = regularSeries[months(time(regularSeries), TRUE) %in% c("Jul", "Aug", "Sep")]#extract only the summer data
    sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
    sampleData = rollingMean[sampleDates] #then pull only those dates from the regular time series

    #Calculate the summary statistics the way that WQCD does
    MaxObs = max(coredata(sampleData), na.rm=TRUE)
    X85th = quantile(coredata(sampleData), c(0.85), na.rm=TRUE, names=FALSE)
  }
  
  Exceedances = ifelse(MaxObs> as.numeric(standard), TRUE, FALSE)
  Impaired = ifelse(X85th > as.numeric(standard), TRUE, FALSE)
  Assessment = if(Impaired == TRUE){
    "Poor"
  }else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == TRUE){
    "Concern" 
  }else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
    "Acceptable" 
  }else{
    "Good"
  }
  return(c(Exceedances, Impaired, Assessment))
}
