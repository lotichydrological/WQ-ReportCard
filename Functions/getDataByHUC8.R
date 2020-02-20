getDataByHUC8 = function(HUC8, StartDate, EndDate){ #retrieve segment data from the Water Quality Portal
  baseURL = c("http://waterqualitydata.us/Result/search?huc=", HUC8, '&siteType=Stream&startDateLo=', StartDate, '&startDateHi=', EndDate, '&mimeType=tsv')
  URL = URLencode(paste(baseURL, collapse=''))
  print(URL)
  Data = read.table(paste(URL), header=TRUE, sep="\t", strip.white=TRUE, quote=NULL, comment='') #retrieve data from the Water Quality Portal
  Name = c("./Data/HUC_", HUC8, "_dataSheet.", as.character(Sys.Date()),".csv") #save the data to a file
  #Name = c("./Data/HUC_", HUC8, "_dataSheet.", as.character(Sys.Date()))
  fileName = paste(Name, collapse='')
  print(paste("Writing data file to ", getwd(), fileName, sep=""))
  write.table(Data, file = fileName, sep="\t", row.names=FALSE)
}
