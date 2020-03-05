#' Retrieve segment data from the Water Quality Portal
#' 
#' @description Retrieves segment data from the Water Quality Portal for a specified HUC8 watershed for a desired time period. 
#' Range should span at least 5 years to allow for trends assessment and to conform with WQCD listing methodology.
#' 
#' @param HUC8 An 8-digit number corresponding to the HUC watershed classification system.
#' @param StartDate MM-DD-YYYY start date for the entire data set you want to retrieve.
#' @param EndDate MM-DD-YYYY end date for the entire data set you want to retrieve. 
#' 
#' @return CSV of water quality data for all segments.
#' 
#' @usage getDataByHUC8(HUC8, StartDate, EndDate)
#' 
#' @example 
#' getDataByHUC8("1401003",'01-01-2014','12-31-2018')
#' 
#' @export

getDataByHUC8 = function(HUC8, StartDate, EndDate){ #retrieve segment data from the Water Quality Portal
  baseURL = c("http://waterqualitydata.us/Result/search?huc=", HUC8, '&siteType=Stream&startDateLo=', StartDate, '&startDateHi=', EndDate, '&mimeType=tsv')
  URL = URLencode(paste(baseURL, collapse=''))
  print(URL)
  Data = read.table(paste(URL), header=TRUE, sep="\t", strip.white=TRUE, quote=NULL, comment='') #retrieve data from the Water Quality Portal
  Name = c("./Data/HUC_", HUC8, "_dataSheet.", as.character(Sys.Date()),".csv") #save the data to a file
  fileName = paste(Name, collapse='')
  print(paste("Writing data file to ", getwd(), fileName, sep=""))
  write.table(Data, file = fileName, sep="\t", row.names=FALSE)
}
