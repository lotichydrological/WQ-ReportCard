library(leaflet)
library(htmlwidgets)
library(dplyr)
library(lubridate)
library(viridis)
library(RColorBrewer)
library(pals)


# testing
# dat <- watershedData
# sites <- monitoringStations


mapSiteData_validate <- function(dat, sites){
  
  # collapse watershed data to summarize unumber of observations and parameters per site
  datSum <- dat %>% 
    group_by(MonitoringLocationIdentifier) %>%
    summarize(n=n(),
              org = unique(OrganizationIdentifier),
              start=year(range(ActivityStartDate)[1]),
              end  =year(range(ActivityStartDate)[2]),
              paramCount = length(unique(CharacteristicName,ResultSampleFractionText)),
              paramNames = paste0(sort(unique(CharacteristicName)),';  ',collapse='')
              ) %>% 
    arrange(desc(n))
  
  # join the summary to the site list
  sites <- merge(sites, datSum, by.x="MonitoringLocationID", 
                 by.y="MonitoringLocationIdentifier")
  
  # create a color palette for monitoring groups
  groupPal <- colorFactor(tol(10), domain=datSum$org)
  
  # # build pop-up info
  # site_popup <- paste0("<strong>Site: </strong>", sites$MonitoringLocationID, "<br>",
  #                      "<strong>Desc: </strong>", sites$MonitoringLocationName, "<br>",
  #                      "<strong>Obs: </strong>", sites$n,
  #                      "<strong>  Start: </strong>", sites$start, 
  #                      "<strong>  End: </strong>", sites$end, "<br>",
  #                      "<strong>Params:<br> </strong>", sites$paramNames, "<br>")
  # names(site_popup) <- sites$OrganizationID
  

  # MAP IT
  
  # create a looping list of only organizations with data
  Orgs <- unique(datSum$org)
  
  # call the map function and set bounds
  map <- leaflet() %>%
    # add a basemap, default = stamen
    addProviderTiles("Stamen.Terrain",
                     options=providerTileOptions(opacity=0.3), group="Terrain") %>%
    addProviderTiles("Esri.WorldImagery", 
                     options=providerTileOptions(opacity=0.5), group="Satellite") %>%
    # set the panning boundaries of the map
    setMaxBounds(lng1 = max(sites$Longitude, na.rm=T) + 1,
                 lat1 = max(sites$Latitude, na.rm=T) - 1,
                 lng2 = min(sites$Longitude, na.rm=T) - 1,
                 lat2 = min(sites$Latitude, na.rm=T) + 1
    ) 

  #loop through and each each organization's data as a layer that can be togglee on/off
  for (org in Orgs){
    #subset org data
    orgData <- sites[sites$OrganizationID==org,]
    # build pop-up info
    site_popup <- paste0("<strong>Site: </strong>", orgData$MonitoringLocationID, "<br>",
                         "<strong>Desc: </strong>", orgData$MonitoringLocationName, "<br>",
                         "<strong>Obs: </strong>", orgData$n,
                         "<strong>  Start: </strong>", orgData$start, 
                         "<strong>  End: </strong>", orgData$end, "<br>",
                         "<strong>Params: </strong>", orgData$paramCount," params available<br>",
                         orgData$paramNames, "<br>")
    #ladd it to map
    map <- map %>%
      addCircleMarkers(
        data = orgData,
        #data = sites %>% dplyr::filter(OrganizationID == org), 
        group = as.character(org),
        lng = ~Longitude, lat = ~Latitude, 
        radius = ~sqrt(n/10),
        #stroke=F,
        color="#000",
        weight=.7,
        fillColor= ~groupPal(org),
        fillOpacity=0.7,
        popup = site_popup
      )
  }
  
  # plot the map with a few more control widgets
  map <- map %>% 
  # add layer controls 
  addLayersControl(
    baseGroups = c("Terrain", "Satellite"),
    overlayGroups = unique(sites$OrganizationID),
    options = layersControlOptions(collapsed = FALSE)) %>%
  #add a legend 
  addLegend(position="bottomleft",
            pal=groupPal,
            values=sites$org)
  map
  # save it to an HTML file in 'Maps' subdirectory for viewing in browser
  saveWidget(map, file=paste(getwd(),"/Site_maps/data_sites_by_n_obs.html", sep=""))
  
}  #end main function


#sites$paramNames[sites$MonitoringLocationID == "USGS-09095500"]

