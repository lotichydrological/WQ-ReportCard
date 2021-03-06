library(zoo)
library(hydroTSM)

hardness_calc_eval_validate = function(Data, standard){
  valueType = get("valueType", envir = parent.frame()) #access the valueType
  segmentName = get("segmentName", envir = parent.frame()) #access the segment name
  hardnessData <- get("hardnessData", envir = parent.frame()) #access the hardness data time series
  parameter <- get("Indicator", envir = parent.frame()) #access the parameter name
  pHData <- get("pHData", envir = parent.frame()) #pass in the pH data time series
  
  pH = mean(pHData$ResultMeasureValue, na.rm = TRUE)
  
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
  if (nrow(Data) == 1 | ( nrow(Data)>1 & length(unique(Data$ActivityStartDate))==1) ){   #second conditions applies if samples all on same date
    sampleData = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
  } else {
  irregularSeries = read.zoo(Data, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
  regularSeries = izoo2rzoo(irregularSeries, from = StartDate, to = EndDate, date.fmt = "%m-%d-%Y", tstep = "days") #then make it regular on a daily time step across the entire time period of interest
  rollingMean = rollapply(regularSeries, 30, mean, na.rm = TRUE) #calculate a rolling 30 day mean
  sampleDates = unique(as.list(Data$ActivityStartDate)) #retrieve the list of unique sampling dates from the original data set
  sampleData = rollingMean[sampleDates] #then pull only those dates from the regular time series
  }
  
  
  #Evaluate on the seasonal hardness standard for segments with that kind of standard
  
  if(length(pHData$ResultMeasureValue)==0){   #for aluminum sites with no pH data, which will break the standard eval()
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
}