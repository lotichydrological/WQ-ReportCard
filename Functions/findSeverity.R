#' Find severity values in data frame 
#'
#' @description Retrieves severity data from categorical data frame and recodes so data is compatible with the rest of the function.
#'
#' @param Levels String variable of severity data from categorical data frame.
#' 
#' @return String variable of severity values.
#' 
#' @usage findSeverity(Levels)
#' 
#' @example findSeverity('Severe')
#' 
#' @export

findSeverity = function(Levels){
  if('Severe' %in% Levels){
    Severity="Severe" 
  }else if('Moderate' %in% Levels){ 
    Severity="Moderate" 
  }else if('Mild' %in% Levels){  
    Severity="Mild" 
  }else if('None' %in% Levels){  
    Severity="None"
  }else
    Severity="NA"
  return (Severity)
}
