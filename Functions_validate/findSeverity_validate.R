findSeverity_validate = function(Levels){
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
