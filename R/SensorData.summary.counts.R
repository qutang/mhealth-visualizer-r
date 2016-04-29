#' @name SensorData.summary.counts.compute
#' @title Compute activity counts using [citation] algorithm
#' @param sensorData input sensor data frame that fits the mhealth specification
#' @param filterType "butter", "bessel"
#' @param breaks epoch to compute counts on
#' @param cutoffs cut off frequencies to be used in filtering, default is 0.6Hz and 10Hz. If choosing bessel, the low pass cut off would be multipled by 2 when being used.
#' @param integrationType the integration method to be used: "trapz", "power", "sum", "meanBySecond", "meanBySize"
#' @export
SensorData.summary.counts.compute = function(sensorData, breaks, filterType = "butter", cutoffs = c(0.6, 10), integrationType = "trapz"){
  # TODO: validate sensorData format
  # TODO: deal with inconsistent sampling rate
  sr = SensorData.getSamplingRate(sensorData)
  # TODO: apply extrapolation algorithm
  # Apply filter cascade (TODO: decide to apply to the whole sequence or only within the break)
  if(filterType == "butter"){
    filteredData = SensorData.filter.butterworth(sensorData, Fs = sr, Fc = cutoffs, order = 4, type = "pass")
    filteredData = filteredData[[1]]
  }else if(filterType == "bessel"){
    filteredData = SensorData.filter.averageRemovalFIR(sensorData, Fs = sr, order = 0.5)
    filteredData = filteredData[[1]]
    filteredData = SensorData.filter.bessel(filteredData, Fs = sr, Fc = cutoffs[2]*2, order = 8)
    filteredData = filteredData[[1]]
  }
  # Compute the AUC
  integratedData = SummaryData.auc(sensorData = filteredData, breaks = breaks, type = integrationType)
  
  # Compute vector magnitude
  countsData = Magnitude.compute(integratedData)
  return(countsData)
}