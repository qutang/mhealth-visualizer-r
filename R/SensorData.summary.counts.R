#' @name SensorData.summary.counts.compute
#' @title Compute activity counts using [citation] algorithm
#' @return Will return NA if a segment break is too short to have enough samples to compute AUC
#' @param sensorData input sensor data frame that fits the mhealth specification
#' @param breaks epoch to compute counts on
#' @param range dynamic range of the device (two element vector)
#' @param noise_std noise level should be between 0.01 and 0.1
#' @param k neighborhood duration for extrapolation in seconds, default is 0.1 seconds
#' @param spar smoothing parameter for extrapolation, default is 0.4
#' @param confident confident level of judging maxed out region
#' @param resample set 0 to not use resampling, otherwise set to the desired sampling rate in numerical, e.g 40 for 40Hz. Default is 50.
#' @param filterType "butter", "ellip", "bessel"

#' @param cutoffs cut off frequencies to be used in filtering, default is 0.3Hz and 5Hz. If choosing bessel, the low pass cut off would be multipled by 2 when being used.
#' @param integrationType the integration method to be used: "trapz", "absoluteMeanByPoints".
#' @export
SensorData.summary.counts.compute = function(sensorData, breaks, range, noise_std, k = 0.1, spar = 0.4, confident = 0.5, resample = 50, filterType = "butter", cutoffs = c(0.2, 5), integrationType = "trapz"){
  # apply extrapolation algorithm
  extrapolatedData = SensorData.extrapolate.v2(sensorData, range, noise_std, k, spar, confident)
  sr = SensorData.getSamplingRate(extrapolatedData)
  # Resample to a consistent sampling rate
  if(resample){
    resampledData = SensorData.filter.resample(extrapolatedData, origSr = sr, newSr = resample)
    resampledData = resampledData[[1]]
    # update to the new sampling rate
    sr = resample
  }else{
    resampledData = extrapolatedData
  }
  
  # Apply filter cascade
  if(filterType == "butter"){
    filteredData = SensorData.filter.iir(resampledData, Fs = sr, Fc = cutoffs, order = 4, type = "pass", filter = "butter")
    filteredData = filteredData[[1]]
  }else if(filterType == "bessel"){
    filteredData = SensorData.filter.averageRemovalFIR(resampledData, Fs = sr, order = 0.5)
    filteredData = filteredData[[1]]
    filteredData = SensorData.filter.bessel(filteredData, Fs = sr, Fc = cutoffs[2]*2, order = 8)
    filteredData = filteredData[[1]]
  }else if(filterType == "ellip") {
    filteredData = SensorData.filter.iir(resampledData, Fs = sr, Fc = cutoffs, order = 4, type = "pass", filter = "ellip")
    filteredData = filteredData[[1]]
  }
  # Compute the AUC
  integratedData = SummaryData.auc(sensorData = filteredData, breaks = breaks, type = integrationType, rectify = TRUE)
  
  # Compute vector magnitude
  countsData = Magnitude.compute(integratedData)
  return(countsData)
}