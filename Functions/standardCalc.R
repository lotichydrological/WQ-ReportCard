#' Returns standard for desired water quality parameter
#' 
#' @description Automatically retrieves or calculates standard for a water quality parameter
#' 
#' @param UseClass Class type of water quality data
#' @param Indicator Desired water quality parameter name
#' @param Category Subcategory of water quality data type
#' @param segmentWBID 305(b) segment WBID for desired stream segment
#' @param eval_type Type of method used for calculating standard
#' 
#' @return numeric standard for desired parameter 
#' 
#' @usage standardCalc(UseClass, Indicator, Category, segmentWBID, eval_type)
#' 
#' @export

########################################################
## Debugging section

standardCalc <- function(UseClass, Indicator, Category, segmentWBID, eval_type){
  
  wqStandards <- get("wqStandards", envir = parent.frame())
  pHData <- get("pHData", envir = parent.frame()) #pass in the pH data time series
  hardnessData <- get("hardnessData", envir = parent.frame()) #access the hardness data time series
  temperatureData <- get("temperatureData", envir = parent.frame()) #pass in the temperature data time series
  
  standard <- wqStandards[wqStandards$Indicator == Indicator & wqStandards$Category == Category &  
                                       wqStandards$UseClass == UseClass,segmentWBID]
  
  if (eval_type == "hardness_calc"){
  pH = mean(pHData$ResultMeasureValue, na.rm = TRUE) #this value gets used in the standard calculation eval call.
  # If there is no pH data reported, no metal standard can be calculated, return NAs and break from function
  if(is.nan(pH)){
    print("No pH data")
  }

  #calculate the mean hardness during low flow periods
  if (length(unique(as.vector(hardnessData$ActivityStartDate))) == 1) {
    if (Indicator == "Aluminum") {
      hardness = min(220, hardnessData$ResultMeasureValue)
    }else {
      hardness = min(400, hardnessData$ResultMeasureValue)
    }
  } else if (length(unique(as.vector(hardnessData$ActivityStartDate))) == 0) {
    if (Indicator == "Aluminum") {
      hardness = 220
    }else {
      hardness = 400
    }
  } else {
    irregularHardness = read.zoo(hardnessData, header = TRUE, format = "%Y-%m-%d", aggregate = mean) #read in the irregular time series and average values collected on the same day
    lowFlowHardness = irregularHardness[months(time(irregularHardness), TRUE) %in% c("Sep","Oct","Nov","Dec","Jan","Feb")]#extract low flow data
    if (Indicator == "Aluminum") {
      hardness = min(220, mean(coredata(lowFlowHardness)), na.rm = TRUE)
    }else {
      hardness = min(400, mean(coredata(lowFlowHardness)), na.rm = TRUE)
    }
  }

  print(paste("Hardness: ", hardness))
  print(paste("Mean pH: ", pH))
  
  standard_test <- as.numeric(eval(parse(text = standard)))
  print(paste("Calculated standard is: ", standard_test, sep=""))
  
  } else if (eval_type == "ammonia_calc"){
  pH = mean(pHData$ResultMeasureValue, na.rm = TRUE)
  temp = mean(temperatureData$ResultMeasureValue, na.rm = TRUE)
  standard_test <- as.numeric(eval(parse(text = standard))) #read the standard in as an expression
  print(paste("Calculated standard is: ", standard_test, sep=""))
  
  } else {
  print(paste("Standard is: ", standard, sep=""))
  standard_test <- as.numeric(standard)
  }
  return(standard_test)
}
