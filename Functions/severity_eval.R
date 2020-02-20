severity_eval = function(Severity){
  Exceedances = ""
  Impaired = ""
  Assessment = ifelse(Severity == "Severe", "Poor", ifelse(Severity == "Moderate", "Concern", ifelse(Severity == "Mild", "Acceptable", "Good")))
  return(c(Exceedances, Impaired, Assessment))
}