#' @name SamplingRate.summary
#' @title Calculate sampling rate over a certain break (e.g. hour, min)
#' @export
#' @import plyr
SamplingRate.summary = function(sensorData, breaks = "min"){
  result = plyr::ddply(sensorData,.(cut(HEADER_TIME_STAMP, breaks= breaks)),nrow)
  names(result)[1] = "BREAKS"
  names(result)[2] = paste("SAMPLING_RATE_PER_",toupper(breaks), sep = "")
  result$BREAKS = as.POSIXct(result$BREAKS)
  result$INTERVAL = breaks
  return(result)
}

#' @name SamplingRate.plot
#' @title Plot sampling rate over certain breaks (e.g. min, hour)
#' @export
#' @import lubridate
#' @param
#' type: "Count" or "Hz" or "Dataloss".
#' "Count" will display the sample counts;
#' "Hz" will display the sampling rate in Hz (count divided by break time)
#' "Dataloss" will display the sampling rate in data loss percentage (count divided by reference)
SamplingRate.plot = function(sr_dat, unit = "Count", ref){
  breaks = sr_dat$INTERVAL[1]
  sr_dat = sr_dat[1:2]
  st = sr_dat$BREAKS[1]
  et = tail(sr_dat$BREAKS, 1)
  ylab = paste("Sampling Rate", "(", unit,")", "per", breaks)
  if(unit == "Hz"){
    sr_dat[2] = sr_dat[2] / .SamplingRate.totalSecondsFromBreaks(breaks)
  }else if(unit == "Dataloss"){
    if(missing(ref)) stop("ref argument in sample count must be provided!")
    sr_dat[2] = (1 - sr_dat[2] / ref) * 100
    ylab = paste("Data loss (%) per", breaks)
    if(min(sr_dat[2]) < 0) ylim = c(min(sr_dat[2], 100))
    else ylim = c(0, 100)
    plot(sr_dat, type = "h",
         xaxt = 'n', xlim = c(st, et), xlab = breaks,
         ylab = ylab, ylim = ylim)
  }

  if(unit != "Dataloss"){
    plot(sr_dat, type = "h",
         xaxt = 'n', xlim = c(st, et), xlab = breaks,
         ylab = ylab)
    if(!missing(ref)){
      abline(h = ref, lty = "dashed")
      text(x = et, y = ref, labels = paste(ref, unit), adj = c(0.75, -0.5), cex = 0.75)
    }
  }
  axis.POSIXct(1, at = seq(st, et, by = breaks), format = .SamplingRate.formatFromBreaks(breaks), x = seq(st, et, by = breaks))
  title(paste("Summary break is in",breaks,
              paste("\n", st,
                    "\n", et,
                    sep="")))
}

#' @import stringr
.SamplingRate.totalSecondsFromBreaks = function(breaks){
  unit = breaks
  value = 1
  if(str_detect(breaks, " ")){
    tokens = str_split(breaks, " ")
    unit = breaks[[2]]
    value = as.numeric(breaks[[1]])
  }
  result = switch(unit,
         sec = 1 * value,
         min = 60 * value,
         hour = 3600 * value,
         day = 3600*24 * value,
         week = 3600*24*7 * value)
  return(result)
}

#' @import stringr
.SamplingRate.formatFromBreaks = function(breaks){
  unit = breaks
  if(str_detect(breaks, " ")){
    tokens = str_split(breaks, " ")
    unit = breaks[[2]]
  }
  result = switch(unit,
                  sec = "%S",
                  min = "%M",
                  hour = "%H",
                  day = "%d",
                  week = "%V")
  return(result)
}
