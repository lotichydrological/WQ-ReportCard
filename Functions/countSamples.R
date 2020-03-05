#' Count number of samples per sampling location
#'
#' @description Counts the number of samples gathered at each sampling station.
#'
#' @param monitoringStation String of desired monitoring station name.
#' @param Data Data frame containing the raw water quality data.
#'
#' @return Data frame containing list of monitoring stations and the number of samples gathered at each station.
#'
#' @usage countSamples(monitoringStation, Data)
#'
#' @export

countSamples = function(monitoringStation, Data){
  stationData = Data[Data$MonitoringLocationIdentifier == monitoringStation,]
  numSamples = length(unique(stationData$ActivityStartDate))
  output = data.frame("Monitoring Location" = monitoringStation, "# Samples" = as.numeric(numSamples))
  return(output)
}
