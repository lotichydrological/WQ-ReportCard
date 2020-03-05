groupStations_validate = function(segmentName){
  stations = monitoringLocations[monitoringLocations$X305b_ID == segmentName,] #find monitoing locations for the segment
  stationList = paste(as.vector(unique(stations$MonitoringLocationID)), collapse=";") #return list of unique location IDs
  return(stationList)
}