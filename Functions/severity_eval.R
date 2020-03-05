#' Evaluate water quality severity
#' 
#' @description Evaluate severity data from water quality data frame. Returns an assessment for that reach based on the severity of the 
#' 
#' @param Severity String variable stating the severity of the water quality, either "Severe", "Moderate", or "Mild".
#' 
#' @return List stating if standards have been exceeded, if the reach is impaired, and the assessment for that parameter in that reach.
#' 
#' @usage severity_eval(Severity)
#' 
#' @export 

severity_eval = function(Severity){
  Exceedances = ""
  Impaired = ""
  Assessment = ifelse(Severity == "Severe", "Poor", ifelse(Severity == "Moderate", "Concern", ifelse(Severity == "Mild", "Acceptable", "Good")))
  return(c(Exceedances, Impaired, Assessment))
}