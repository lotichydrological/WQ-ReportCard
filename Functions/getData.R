getData = function(segmentName, StartDate, EndDate){ #retrieve segment data from the Water Quality Portal
  Stations = groupStations(segmentName)
  baseURL = c("http://waterqualitydata.us/Result/search?siteid=", Stations, '&startDateLo=', StartDate, '&startDateHi=', EndDate, '&mimeType=tsv')
  URL = URLencode(paste(baseURL, collapse=''))
  print(URL)
  Data = read.table(paste(URL), header=TRUE, sep="\t", strip.white=TRUE) #retrieve data from the Water Quality Portal
  Name = c("./Data/", segmentName, "_dataSheet.csv") #save the data to a file
  fileName = paste(Name, collapse='')
  write.table(Data, file = fileName, sep=",", row.names=FALSE)
  return(URL)
}