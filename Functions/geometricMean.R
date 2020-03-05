#' Calculate a geometric mean
#' 
#' @description Calculates a geometric mean from regular time series data.
#' 
#' @param data List variable of a regular time series.
#' 
#' @usage geometricMean(data)
#' 
#' @export

geometricMean = function(data) {
  log_data = log(data)
  gm = exp(mean(log_data[is.finite(log_data)]))
  return(gm)
}