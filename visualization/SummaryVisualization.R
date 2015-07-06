SamplingRate.plot = function(sr_dat){
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