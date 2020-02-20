
# put this into 'summaryCalcs()' below the Percentile_85th() calculation and above the somputeStandards block.
##############################################

#Trend_5yr = if( n > 0 & ( (Censored / n ) > 0.5) & standardType != "severity" & standardType != "manual"){
#  calculateTrend(numericalData)
#} else Trend_5yr <- c(-9999,-9999)
#TrendSlope <- Trend_5yr[1]
#Pvalue     <- Trend_5yr[2]


####################################3333


library(rkt)

# This function tests a time-series dataset for a monotonic trend using the Seasonal Kendall Test
# from the R package 'rkt'

calculateTrend = function(data){
    
  # Test data for appropriate use in Seasonal Kendall test with these criteria:
  # 1) 5 years of data minimum
  # 2) first and last 1/3rd of time range includes at least 20% of data coverage (Helsel and Hirsch)
  # 3) at least 1 data point in every seasonal block, (required for package rkt() not to fail)
  # 4)  <50% censoring of data (checked prior to launching function) 
  
  #Data equaully distributed
  
  SampleDates = data.frame("SampleDates"=seq(as.Date(StartDate, "%m-%d-%Y"), as.Date(EndDate, "%m-%d-%Y"), by="days"))
  datebreaks <- c(  c(levels(cut(SampleDates$SampleDates,3,include.lowest=T))),
                    as.character((SampleDates$SampleDates[length(SampleDates$SampleDates)])))
  datebreaksdf <- data.frame(datebreaks)
  datebreaksdf$datebreaks <- as.Date(datebreaksdf$datebreaks, "%Y-%m-%d")
  samp_date_table <- table(as.Date(cut(data$ActivityStartDate, breaks=datebreaksdf$datebreaks, include.lowest=T)))
  equaldatadist <- FALSE  #assume data does not meet criteria until tested
  if (length(samp_date_table) == 3) {   # test to see if there are data points in each 1/3, then see if minumum 20% in each third
    if ( samp_date_table[1]/length(data$ActivityStartDate) >= 0.2 &
           samp_date_table[2]/length(data$ActivityStartDate) >= 0.2 &
           samp_date_table[3]/length(data$ActivityStartDate) >= 0.2 ) {equaldatadist <- TRUE}
  } else {
    equaldatadist <- FALSE
  }
  
          
  
  #format the data and result fields to work in the trend test
  data$ActivityStartDate <- as.Date(data$ActivityStartDate,"%Y-%m-%d") 
  data$ResultMeasureValue <- as.numeric(data$ResultMeasureValue)
  
  #write new columns extracting months, years, seasons from dates for use in Seasonal Kendall Test rkt()
  data$year<- as.numeric(format(data$ActivityStartDate, "%Y"))
  data$mon <- as.factor(months(data$ActivityStartDate,abbreviate=T))
  data$mon_num <-as.integer(format(data$ActivityStartDate, "%m")) #extracts numerical months
  
  #Seasonal blocks are currently assigned these integers here, but can be user-specified to anything
  #Dec-Feb 1=winter, Mar-May 2=spring, June-Aug 3=summer, Sep-Nov 4=fall
  data$seas <- as.integer(ifelse((data$mon_num == 12| data$mon_num == 1| data$mon_num ==2),1,
                                                 ifelse((data$mon_num == 3| data$mon_num == 4| data$mon_num == 5),2,
                                                        ifelse((data$mon_num == 6| data$mon_num == 7| data$mon_num ==8),3,4))))
  
  #does data meet all the criteria? if data is inadequate, return an empty summary
  if (equaldatadist == FALSE ) {
    slope  <- -9999
    pvalue <- -9999
  } else {
    #perform seasonal kendal test
    seas_test <- rkt(data$year, data$ResultMeasureValue, data$seas, rep="m")
    if (is.numeric(seas_test$sl) == TRUE) {  #if the test fails, all values will be NA
      if (seas_test$sl <= 0.05) {         # record results with signficance level of <= 5% or p <= 0.05
        slope  <- seas_test$B
        pvalue <- seas_test$sl
      } else{
        slope  <- -9999
        pvalue <- -9999
      }
    } else{
      slope  <- -9999
      pvalue <- -9999
    }
  }
  Trend_5yr <- c(slope, pvalue)
  return(Trend_5yr)
}

