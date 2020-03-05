#' Generate a list of sampling organizations.
#' 
#' @description Generate a list of organizations that gathered the water quality samples in a given dataset.
#' 
#' @param CharacteristicNames Analyte in water sample.
#' @param Units Measurement units of analyte.
#' @param valueType Total or other type of value.
#' @param standardType One day or other type of water quality standard.
#' @param standard EPA or state standard for water quality impairment.
#' 
#' @return List of organizations that gathered water quality samples.
#' 
#' @usage orgList(CharacteristicNames, Units, valueType, standardType, standard)
#' 
#' @export

orgList = function(characteristicNames, Units, valueType, standardType, standard){
  Data <- get("Data", envir = parent.frame())
  parameterNames = unlist(strsplit(characteristicNames, split = ";")) #Retrieve all the possible parameter names
  parameterNames = parameterNames[parameterNames != ""] #Remove empty elements from the vector
  unitList = c("None", "", Units) #include 'None' and blank values as a unit to capture non-detects and unitless data like pH 
  dataSet = Data[Data$CharacteristicName %in% parameterNames & Data$ResultSampleFractionText %in% valueType & Data$ResultMeasure.MeasureUnitCode  %in% unitList,] #subset the data
  Organizations = paste(unique(as.vector(dataSet$OrganizationIdentifier)), collapse=",") #list the unique organizations
  return(Organizations)
}