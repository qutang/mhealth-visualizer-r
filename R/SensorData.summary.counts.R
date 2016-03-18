#' @name SensorData.summary.counts.compute
#' @title Compute activity counts using [citation] algorithm
#' @param sensorData input sensor data frame that fits the mhealth specification
#' @param filterType "butter", "bessel"
#' @param breaks epoch to compute counts on
#' @export
SensorData.summary.counts.compute = function(sensorData, breaks, filterType = "butter"){
  # TODO: validate sensorData format
  # TODO: deal with inconsistent sampling rate
  sr = SensorData.getSamplingRate(sensorData)
  # TODO: apply extrapolation algorithm
  # Apply filter cascade (TODO: decide to apply to the whole sequence or only within the break)
  if(filterType == "butter"){
    filteredData = SensorData.filter.butterworth(sensorData, Fs = sr, Fc = c(0.6, 10), order = 8, type = "pass")
    filteredData = filteredData[[1]]
  }else if(filterType == "bessel"){
    filteredData = SensorData.filter.averageRemovalFIR(sensorData, Fs = sr, order = 0.5)
    filteredData = filteredData[[1]]
    filteredData = SensorData.filter.bessel(filteredData, Fs = sr, Fc = 20, order = 8)
    filteredData = filteredData[[1]]
  }
  # Compute the AUC
  trapzData = SummaryData.auc(sensorData = filteredData, breaks = breaks)
  # Compute vector magnitude
  countsData = Magnitude.compute(trapzData)
  return(countsData)
}