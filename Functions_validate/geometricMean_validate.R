geometricMean_validate = function(data) {
  log_data = log(data)
  gm = exp(mean(log_data[is.finite(log_data)]))
  return(gm)
}