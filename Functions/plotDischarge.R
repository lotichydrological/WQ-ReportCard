#' Plot hydrograph of segment discharge with sample dates.
#' 
#' @description Generates hydrograph plots for the nearest USGS gage to each stream segment,
#' with sampling dates plotted to indicate the flow conditions during sample collection.
#' 
#' @param segmentName String of the desired segment name.
#' @param wqData Data frame of water quality observations corresponding to each segment.
#' 
#' @return PNG plot of segment hydroraph during period of record with collection dates highlighted.
#' 
#' @usage plotDischarge(segmentName, wqData)
#' 
#' @export


plotDischarge = function(segmentName, wqData){
  segInfo = dischargeStations[dischargeStations$SegmentID == segment,]
  stationID = segInfo[1,3]
  #excel frequently truncates preceeding zeros if you inspect the spreadsheet and resave, re-format if necessary
  if(substring(stationID, 1, 1)=="9"){
    stationID <- paste("0", stationID, sep='')
  }
  startDate = as.character(as.Date(StartDate, format = "%m-%d-%Y"))
  endDate = as.character(as.Date(EndDate, format = "%m-%d-%Y"))
  #Retrieve streamflow data
  baseURL = c("http://waterservices.usgs.gov/nwis/dv/?site=",stationID,"&startDT=",startDate,"&endDT=",endDate,"&parameterCd=00060&format=rdb")
  URL = URLencode(paste(baseURL, collapse=''))
  #retrieve data from NWIS REST API
  qData = read.table(paste(URL), header=TRUE, sep="\t", strip.white=TRUE) 
  qData = qData[-1,] #remove first header row
  names(qData) <- c("organization", "stationID", "dateTime", "Q", "qualifier")
  qData$dateTime <- as.Date(qData$dateTime,format='%Y-%m-%d')
  qData$Q <- as.numeric(as.character(qData$Q))
  obsDates = unique(as.Date(wqData$ActivityStartDate))
  obsQ = qData[qData$dateTime %in% obsDates,]
  datesToShow = seq(from = as.Date(startDate, format='%Y-%m-%d'),to = as.Date(endDate,format='%Y-%m-%d'), by="month")
  #plot the data
  filename <- paste(c("./Figures/qPlots/",segment,"_Q_plot.png"),collapse="")
  png(filename, width = 6.5, height = 2.35, units = "in", res=400, pointsize=7)
  par(mai=c(0.25,0.75,0.25,0.25))
  caption = paste(c("Data collection sampling dates (red) on 305(b) segment ",
                    segment,
                    ". Streamflow (blue) derived from nearest available USGS gauge \nstation may not be representative of locations for the segment but provides a general sense of hydrological conditions during sampling."), 
                  collapse="")
  plot(qData$dateTime, qData$Q, 
       type = "l", col = "blue", 
       xlab ='', ylab = paste(c("Discharge (cfs) measured at \nUSGS station ",stationID), collapse=''))
  title(main = caption, adj=0, cex.main=.9)
  axis(side=2)
  points(as.Date(obsQ$dateTime, format='%Y-%m-%d'), as.numeric(obsQ$Q), col = "red")
  #add minor ticks at months
  at <- seq(from = as.Date(StartDate, format = "%m-%d-%Y"), to = as.Date(endDate, format = "%Y-%m-%d"), by = "month") # produces a regular sequence of dates  
  at2 <- at[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE)]  #subsets the minor date tick to every 3rd month (Mar, June, Sep)
  axis.Date(side = 1, at = at2, labels = F, tck=-0.03)
  at <- seq(from = as.Date(StartDate, format = "%m-%d-%Y"), to = as.Date(endDate, format = "%Y-%m-%d"), by = "month") # produces a regular sequence of dates  
  axis.Date(side = 1, at = at, labels = F, tck=-0.01) 
  dev.off()
}