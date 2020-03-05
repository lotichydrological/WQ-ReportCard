#' Find sampling locations per segment
#' 
#' @description Find water quality sampling locations for a desired strea segment. 
#' Returns a list of sampling sites and creates a table GROB from the dataframe.
#' 
#' @param segmentName String of the desired stream segment name.
#' @param Data Data frame containing the raw water quality data.
#' 
#' @return Data frame containing the station list and number of samples per station.
#' 
#' @usage findSampleLocations(segmentName, Data)
#' 
#' @export

library(plyr)
library(tools)
library(gplots)
library(grid)
library(gridExtra)
library(gtable)

findSampleLocations = function(segmentName, Data){
  uniqueStations = unique(as.list(Data$MonitoringLocationIdentifier))
  stationList <- do.call(rbind, lapply(uniqueStations, countSamples, Data))
  colnames(stationList) <- c("Monitoring Location", "# Samples")
  TableName <- paste(c("./Figures/site_tables/",segmentName,"_MonSites.png"),collapse="")    
  
  
  png(TableName, height=( (nrow(stationList) * 22) + 55 ), width = 450, units="px", pointsize=12) # for 12 pt font, each row is 16 pixels high, + extra padding for titles etc
  par(mar=c(0.1, 0.1, 0.1, 0.1),oma=c(0,0,0,0))
                 
        #table plotting code here

        #create a table GROB (graphical represenation object) from the dataframe
        siteTable <- tableGrob(stationList, theme = ttheme_minimal(clip="off", hjust="left"), rows=NULL)
  
        #set up the title information
        minYear <- as.character(min(year(Data$ActivityStartDate), na.rm=T))
        maxYear <- as.character(max(year(Data$ActivityStartDate), na.rm=T))
        caption <- paste("Active sites, ", minYear, " to ", maxYear,", Segment: ", segmentName, sep="")
        title <- textGrob(caption,gp=gpar(fontsize=12, fontface="bold"))
        padding <- unit(1,"line") 

        siteTable <- gtable_add_rows(siteTable, heights=grobHeight(title)+padding,pos=0)
        #siteTable <- gtable_add_grob(siteTable, title, t=1, l=1, r=1)
        siteTable <- gtable_add_grob(siteTable, title, t=1, l=1, r=ncol(siteTable), clip="off")       
  
        #add a line above the first row (table header row)
        siteTable <- gtable_add_grob(siteTable,
                                     grobs = segmentsGrob( # line across the bottom
                                       x0 = unit(0,"npc"),
                                       y0 = unit(0,"npc"),
                                       x1 = unit(1,"npc"),
                                       y1 = unit(0,"npc"),
                                       gp = gpar(lwd = 1.0)), t = 2, l = 1, r = ncol(siteTable))
        #add a line under the bottom row (last row)
        siteTable <- gtable_add_grob(siteTable,
                                     grobs = segmentsGrob( # line across the bottom
                                       x0 = unit(0,"npc"),
                                       y0 = unit(0,"npc"),
                                       x1 = unit(1,"npc"),
                                       y1 = unit(0,"npc"),
                                       gp = gpar(lwd = 1.0)), t = nrow(siteTable), l = 1, r = ncol(siteTable))
                  grid.draw(siteTable)               
    dev.off() 
  return(stationList)
}
