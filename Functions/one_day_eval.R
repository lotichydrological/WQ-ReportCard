#' Evaluate if samples have exceeded standards more than once every 3 years
#' 
#' @description Evaluate if the reach is impaired due to too many standard exceedances.
#' Sample data from water quality data frame is compared to the corresponding water quality standards. 
#' Reaches are impaired if standards are exceeded more than once every three years as per 303d listing methodology.
#' As this code runs for 5 years of data, if 2 in 5 years have exceedances the reach is considered impaired.
#' Determines if exceedances exist in the dataset, if the reach is impaired, and produces an 
#' assessment of the reach for that parameter.
#' 
#' @param Data Data frame containing the raw water quality data.
#' @param standard Water quality standard for the given parameter
#' 
#' @return List stating if standards have been exceeded, if the reach is impaired, and the assessment for that parameter in that reach.
#' 
#' @usage one_day_eval(Data, standard)
#' 
#' @export 

library(zoo)
library(hydroTSM)

one_day_eval = function(Data, standard){
  if (length(unique(as.vector(Data$ActivityStartDate))) == 1) {
    MaxObs = max(Data$ResultMeasureValue, na.rm=TRUE)
    X85th = quantile(Data$ResultMeasureValue, c(0.85), na.rm=TRUE, names=FALSE)
  } else {
  irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
  regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest for assessing against standards
  sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
  sampleData = regularSeries[sampleDates] #then pull only those dates from the regular time series

  #Calculate the summary statistics the way that WQCD does
  MaxObs = max(coredata(sampleData), na.rm=TRUE)
  X85th = quantile(coredata(sampleData), c(0.85), na.rm=TRUE, names=FALSE)
  }
  
  Exceedances = ifelse(MaxObs > as.numeric(standard), TRUE, FALSE)
  
  # The criteria for impairment here is "data indicates nonattainment if the standard is exceeded more frequently than
  # once in three years" (from the listing methodology), since we do this analysis for 5 yrs of data, that's 2 in 5 years (1.666 yrs)
  # from '303d listing methodology 2018 pg 28
  
  # Count the number of years with exceedances ...
  # first, find the dates of exceedances
  exceedData <- Data[Data$ResultMeasureValue > standard,]
  # if there are, determine what years, see if it hits the 2 of 5 threshold, and see if any are in the last 3 years
  if(length(exceedData$ActivityStartDate) > 0){
    # next, find how many different years this occurred in
    yearsWithExceed <- unique(year(exceedData$ActivityStartDate))
    
    # see if any occurred in the last 3 consecutive years
    endYear <- year(as.Date(EndDate, format="%m-%d-%Y"))
    exceedInLastThree <- (yearsWithExceed %in% seq(endYear,endYear-2,-1) )
    if(any(yearsWithExceed %in% seq(endYear, endYear-2, -1))){exceedInLastThree <- TRUE}
    
    # if 'exceedInLastThree' = TRUE and 'yearsWithExceed' >=2, then impairment threshold is reached
    Impaired <- (isTRUE(exceedInLastThree) & length(yearsWithExceed) >= 2)
  } else {Impaired <- FALSE}
 
  #if there is a standard for the parameter, evaluate it, if not, return NA)
  Assessment = ifelse(!is.na(as.numeric(standard)),  
    if(isTRUE(Impaired)){
      "Poor"
    }else if (X85th >= 0.5*(as.numeric(standard)) & isTRUE(Exceedances)){
      "Concern" 
    }else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
      "Acceptable" 
    }else{
      "Good"
    }, 
    NA)
 
  return(c(Exceedances, Impaired, Assessment))
}
