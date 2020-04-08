library(formattable)
library(htmlwidgets)

buildAssessTable <- function(segment_list){
  # read in column names from RDS file
  segdata <- readRDS(paste("Output/",segment_list[1],".rds",sep = ""),refhook = NULL)
  df <- segdata[as.character(c("UseClass","Category","Indicator","Units","valueType"))]
  
  for(segmentName in segment_list){
    # read segment data from RDS objects
    segdata <- readRDS(paste("Output/",segmentName,".rds",sep = ""),refhook = NULL)
    assessments = as.character(segdata$Assessment)
    
    # write segment data to table 
    if(segmentName %in% colnames(df)){
      df[[segmentName]] <- assessments
    }
    else{
      assessments <- as.data.frame(assessments)
      df <- bind_cols(df, assessments)
      names(df)[names(df) == "assessments"] <- segmentName
    }
    
  }
  
  # save R object of data frame to access later
  saveRDS(df, "Output/Assessment_table.rds")
  
  # define conditional formatting for cell backgrounds
  bg.picker <- function(z){
    if(is.na(z)){return("white");
    } else if(z == "Concern"){return("yellow");
    } else if(z == "Poor"){return("red");
    } else if(z == "Acceptable"){return("lightgreen");
    } else if(z == "Poor Resolution"){return("lightgrey");
    } else if(z == "Good"){return("green");  
    } else {return("white");
    }
  }
  
  # create formatted table of all data
  color_table <- formattable(df, align = "c", list(
                    area(col = 5:length(colnames(df))) ~ formatter("span", style = x ~ style(display = "block",
                                                             "border-radius" = "4px",
                                                             "padding-right" = "4px",
                                                             "background-color"= sapply(x,bg.picker)))
                               ))
  # export as html widget
  html_w <- as.htmlwidget(color_table,width = "100%", height = NULL)
  
  # save html widget to file
  saveWidget(html_w,"Assessment_table.html", selfcontained = TRUE, libdir = NULL)
  
  
}
