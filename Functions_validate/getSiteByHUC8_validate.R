getSiteByHUC8_validate = function(HUC8, StartDate, EndDate){ #retrieve segment data from the Water Quality Portal
  baseURL = c("http://waterqualitydata.us/data/Station/search?siteType=Stream&huc=", HUC8, "&startDateLo=", StartDate, '&startDateHi=', EndDate, '&mimeType=tsv')
  URL = URLencode(paste(baseURL, collapse=''))
  print(URL)
  Data = read.table(paste(URL), header=TRUE, sep="\t", strip.white=TRUE, quote=NULL, comment='') #retrieve data from the Water Quality Portal
  Name = c("./Site_prep/HUC_", HUC8, "_sites.", as.character(Sys.Date()),".csv") #save the data to a file
  fileName = paste(Name, collapse='')
  write.table(Data, file = fileName, sep=",", row.names=FALSE)
}
