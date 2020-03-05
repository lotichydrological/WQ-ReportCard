#' Evaluate if the geometric mean of sample values over a two month period exceeds the water quality standard
#' 
#' @description Evaluate if the reach is impaired due to too many standard exceedances.
#' Sample data from water quality data frame is compared to the corresponding water quality standards. 
#' Reaches are impaired if the geometric mean of sample values exceeds the standard over a 60 day period.
#' Determines if exceedances exist in the dataset, if the reach is impaired, and produces an 
#' assessment of the reach for that parameter.
#' 
#' @param Data Data frame containing the raw water quality data.
#' @param standard Water quality standard for the given parameter
#' 
#' @return List stating if standards have been exceeded, if the reach is impaired, and the assessment for that parameter in that reach.
#' 
#' @usage two_month_geometric_mean_eval(Data, standard)
#' 
#' @export 

library(zoo)
library(hydroTSM)

two_month_geometric_mean_eval = function(Data, standard){
  Data$ResultMeasureValue[Data$ResultMeasureValue==0] = 1 #convert BDL values to 1 as per WQCD listing methodology for evaluating E.coli standards
  if (nrow(Data) == 1) {
    MaxObs = max(Data$ResultMeasureValue, na.rm=TRUE)
    X85th = quantile(Data$ResultMeasureValue, c(0.85), na.rm=TRUE, names=FALSE)
  } else {
    irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
    rollingMean = rollapply(regularSeries, 60, FUN = function(x) geometricMean(x), by = 60, fill = NA) #calculate a 60 day geometric mean over fixed two month intervals

    #Calculate the summary statistics the way that WQCD does
    MaxObs = max(coredata(rollingMean), na.rm=TRUE)
    X85th = quantile(coredata(rollingMean), c(0.85), na.rm=TRUE, names=FALSE)
  }
  Exceedances = ifelse(MaxObs> as.numeric(standard), TRUE, FALSE)
  Impaired = ifelse(X85th > as.numeric(standard), TRUE, FALSE)
  Assessment = if (Impaired == TRUE){
    "Poor"
  } else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == TRUE){
    "Concern" 
  } else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
    "Acceptable" 
  } else{
    "Good"
  }
  return(c(Exceedances, Impaired, Assessment))
}