#' @export
#' @import plyr
SamplingRate.summary = function(sensorData, breaks = "min"){
  result = plyr::ddply(sensorData,.(cut(HEADER_TIME_STAMP, breaks= breaks)),nrow)
  names(result)[1] = "BREAKS"
  names(result)[2] = "SAMPLING_RATE_PER_MINUTE"
  result$BREAKS = as.POSIXct(result$BREAKS)
  result$INTERVAL = breaks
  return(result)
}

#' @export
#' @import lubridate
SamplingRate.plot = function(sr_dat){
  breaks = sr_dat$INTERVAL[1]
  sr_dat = sr_dat[1:2]
  st = sr_dat$BREAKS[ceiling(nrow(sr_dat)/2)]
  hour(st) = hour(st) - 1
  minute(st) = 55
  second(st) = 0
  et = sr_dat$BREAKS[ceiling(nrow(sr_dat)/2)]
  hour(et) = hour(et) + 1
  minute(et) = 5
  second(et) = 0
  plot(sr_dat, type = "h", xaxt = 'n', xlim = c(st, et))
  axis.POSIXct(1, at = seq(st, et, by = "min"), format = "%M", x = seq(st, et, by = "min"))
  title(paste("Summary break is in",breaks))
}
