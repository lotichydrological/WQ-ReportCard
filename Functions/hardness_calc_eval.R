#' Evaluate hardness sample data
#' 
#' @description Evaluate hardness sample data from water quality data frame. Does not calculate f no pH data is reported.
#' If more than one sample exists, a rolling mean of sample values is calculated. 
#' If the hardness standard is seasonal, samples are segregated by high metals months and low metals months. 
#' Summary statistics are then calculated as per WQCD guidance. Determines if exceedances exist in the dataset, if the reach is impaired, and produces an 
#' assessment of the reach for that parameter.
#' 
#' @param Data Data frame containing the raw water quality data.
#' @param standard Water quality standard for the given parameter
#' 
#' @return List stating if standards have been exceeded, if the reach is impaired, and the assessment for that parameter in that reach.
#' 
#' @usage hardness_calc_eval(Data, standard)
#' 
#' @export 

library(zoo)
library(hydroTSM)

hardness_calc_eval = function(Data, standard){
  valueType = get("valueType", envir = parent.frame()) #access the valueType
  segmentName = get("segmentName", envir = parent.frame()) #access the segment name
  hardnessData <- get("hardnessData", envir = parent.frame()) #access the hardness data time series
  parameter <- get("Indicator", envir = parent.frame()) #access the parameter name
  pHData <- get("pHData", envir = parent.frame()) #pass in the pH data time series
  
  pH = mean(pHData$ResultMeasureValue, na.rm = TRUE) #this value gets used in the standard calculation eval call.
  # If there is no pH data reported, no metal standard can be calculated, return NAs and break from function
  if(is.nan(pH)){
    return(c(NA, NA, NA))
  }

    #calculate the mean hardness during low flow periods
  if (length(unique(as.vector(hardnessData$ActivityStartDate))) == 1) {
    if (parameter == "Aluminum") {
      hardness = min(220, hardnessData$ResultMeasureValue) 
    }else {
      hardness = min(400, hardnessData$ResultMeasureValue) 
    }
  } else if (length(unique(as.vector(hardnessData$ActivityStartDate))) == 0) {
    if (parameter == "Aluminum") {
      hardness = 220
    }else {
      hardness = 400
    }
  } else {
    irregularHardness = read.zoo(hardnessData, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    lowFlowHardness = irregularHardness[months(time(irregularHardness), TRUE) %in% c("Sep","Oct","Nov","Dec","Jan","Feb")]#extract low flow data
    if (parameter == "Aluminum") {
      hardness = min(220, mean(coredata(lowFlowHardness)), na.rm = TRUE) 
    }else {
      hardness = min(400, mean(coredata(lowFlowHardness)), na.rm = TRUE) 
    }
  }
  if (nrow(Data) == 1){
    sampleData = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
  } else {
  irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
  regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
  rollingMean = rollapply(regularSeries, 30, mean, na.rm = TRUE) #calculate a rolling 30 day mean
  sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
  if(length(sampleDates)>1){
    sampleData = rollingMean[sampleDates] #then pull only those dates from the regular time series
  } else {
    sampleData = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) # account for instances in which there is 2 observations from one single day only
  }
 
  }
  
  print(paste("hardness ", hardness))
  print(paste("Mean pH: ", pH))
  #print(standard)
  standard_test <- eval(parse(text = standard))
  print(paste("Calculated standard is: ", standard_test, sep=""))
  
  #Evaluate on the seasonal hardness standard for segments with that kind of standard
  
  if( length(pHData[1]) < 1 | is.null(pHData) ) {
    Exceedances <- NA
    Impaired <- NA
    Assessment <- "Poor Resolution"
  } else if (segmentName %in% c("COUCEA05b_A","COUCEA07b_A") & parameter == "Zinc") {
    
    highMetalSeason = TRUE #this is a switch for the standard evaluation
    highMetalStandard = eval(parse(text = standard)) #evaluate the high metals season standard
    highMetalData = sampleData[months(time(sampleData), TRUE) %in% c("Jan","Feb","Mar","Apr")]#extract high metals data
    if(length(coredata(highMetalData))==0){
      highMetalMax = 0
      highMetal85th = 0
    } else {
    highMetalMax = max(coredata(highMetalData), na.rm=TRUE)
    highMetal85th = quantile(coredata(highMetalData), c(0.85), na.rm=TRUE, names=FALSE)
    }
    
    highMetalSeason = FALSE
    lowMetalStandard = eval(parse(text = standard)) #evaluate the low metals season standard
    lowMetalData = sampleData[months(time(sampleData), TRUE) %in% c("May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")]#extract low metals data
    if(length(coredata(lowMetalData)) == 0){
      lowMetalMax = 0
      lowMetal85th = 0
    } else {
    lowMetalMax = max(coredata(lowMetalData), na.rm=TRUE)
    lowMetal85th = quantile(coredata(lowMetalData), c(0.85), na.rm=TRUE, names=FALSE)
    }
    
    Exceedances = ifelse(highMetalMax < highMetalStandard && lowMetalMax < lowMetalStandard, FALSE, TRUE)
    Impaired = ifelse(highMetal85th < highMetalStandard && lowMetal85th < lowMetalStandard, FALSE, TRUE)
    Assessment = if(Impaired == TRUE){
      "Poor"
    }else if (Exceedances == TRUE & highMetalMax >= 0.5*(highMetalStandard) || Exceedances == TRUE & lowMetalMax >= 0.5*(lowMetalStandard)){
      "Concern" 
    }else if (Exceedances == FALSE & highMetalMax >= 0.5*(highMetalStandard) || Exceedances == FALSE & lowMetalMax >= 0.5*(lowMetalStandard)){
      "Acceptable" 
    }else {
      "Good"
    }
  } else if (valueType == "Total" | valueType == "Total Residual") {
    standard = eval(parse(text = standard)) #read the standard in as an expression
    
    #Calculate the summary statistics the way that WQCD does
    MaxObs = max(coredata(sampleData), na.rm=TRUE)
    MedObs = median(coredata(sampleData), na.rm=TRUE)
    
    Exceedances = ifelse(MaxObs > as.numeric(standard), TRUE, FALSE)
    Impaired = ifelse(MedObs > as.numeric(standard), TRUE, FALSE)
    Assessment = if(Impaired == TRUE){
      "Poor"
    }else if (MedObs >= 0.5*(as.numeric(standard)) & Exceedances == TRUE){
      "Concern" 
    }else if (MedObs >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
      "Acceptable" 
    }else {
      "Good"
    }
  } else {
    standard = eval(parse(text = standard)) #read the standard in as an expression
    
    #Calculate the summary statistics the way that WQCD does
    MaxObs = max(coredata(sampleData), na.rm=TRUE)
    X85th = quantile(coredata(sampleData), c(0.85), na.rm=TRUE, names=FALSE)
    
    Exceedances = ifelse(MaxObs > as.numeric(standard), TRUE, FALSE)
    Impaired = ifelse(X85th > as.numeric(standard), TRUE, FALSE)
    Assessment = if (Impaired == TRUE) {
      "Poor"
    } else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == TRUE){
      "Concern" 
    } else if (X85th >= 0.5*(as.numeric(standard)) & Exceedances == FALSE){
      "Acceptable" 
    } else {
      "Good"
    }
  }
  return(c(Exceedances, Impaired, Assessment))
  #if you want to return the actual numeric value for the standard that was calculated, use this:
  #return(c(Exceedances, Impaired, Assessment, standard))
}