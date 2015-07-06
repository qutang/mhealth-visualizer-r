SamplingRate.summary = function(sensorData, breaks = "min"){
  require("plyr")
  source("mhealthformat/GeneralConvention.R")
  result = ddply(sensorData,.(cut(HEADER_TIME_STAMP, breaks= breaks)),nrow)
  names(result)[1] = "BREAKS"
  names(result)[2] = "SAMPLING_RATE_PER_MINUTE"
  result$BREAKS = as.POSIXct(result$BREAKS)
  return(result)
}
