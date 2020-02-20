# for any parameters that frequently are reported in more than one unit, insert a correction code here
# for the desired unit used in standards comparison

standardize_units <- function(dataset){
  
  # patch to take care of units converstion for nutrients, implemented 2/18/2010 by BH for MCW project
    TN_forms <- c("Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)", "Nitrogen" )
    TP_forms <- c("Phosphorus", "Total Phosphorus, mixed forms")
    # phosphorus and nitrogen only:
    # any reported as mg/l as P or N, just label the unit as mg/l
    dataset$ResultMeasure.MeasureUnitCode[((dataset$CharacteristicName %in% TN_forms) |
                                           (dataset$CharacteristicName %in% TP_forms)) &
                                            ((dataset$ResultMeasure.MeasureUnitCode == "mg/l as P") |
                                              (dataset$ResultMeasure.MeasureUnitCode == "mg/l as N")) 
                                              ] <- "mg/l"
    # convert any mg/l to ug/l
    dataset$ResultMeasureValue[((dataset$CharacteristicName %in% TN_forms) |
                                (dataset$CharacteristicName %in% TP_forms)) &
                                 dataset$ResultMeasure.MeasureUnitCode == "mg/l"] <- 
        dataset$ResultMeasureValue[((dataset$CharacteristicName %in% TN_forms) |
                                    (dataset$CharacteristicName %in% TP_forms)) &
                                     dataset$ResultMeasure.MeasureUnitCode == "mg/l"] * 1000
    
    # re-label the units from mg/l to ug/l
    dataset$ResultMeasure.MeasureUnitCode[((dataset$CharacteristicName %in% TN_forms) |
                                           (dataset$CharacteristicName %in% TP_forms)) &
                                            dataset$ResultMeasure.MeasureUnitCode == "mg/l"] <- "ug/l"
   
    return(dataset)
}




# phos_params <-  c("Phosphorus", "Total Phosphorus, mixed forms")
# phos_data <- watershedData[watershedData$CharacteristicName %in% phos_params,]
# phos_data$ResultMeasureValue <- as.numeric(phos_data$ResultMeasureValue)
# dataset <- phos_data
