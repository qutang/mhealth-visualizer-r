MHEALTH_CSV_SAMPLING_RATE_HEADER = "SAMPLING_RATE"

#' @name SamplingRate.summary
#' @title Calculate sampling rate over a certain break (e.g. hour, min)
#' @export
#' @import plyr

SamplingRate.summary = function(sensorData, breaks = "min"){
  result = plyr::ddply(sensorData,.(cut(HEADER_TIME_STAMP, breaks= breaks)),nrow)
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  names(result)[2] = MHEALTH_CSV_SAMPLING_RATE_HEADER
  result[,1] = as.POSIXct(result[,1])
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
  ts = sr_dat[,1]
  value = sr_dat[,2]
  breaks = pretty_dates(ts, n = 10)
  st = breaks[1]
  et = tail(breaks, 1)
  ylab = "value"
  interval = as.period(new_interval(ts[1], ts[2]))
  if(unit == "Hz"){
    value = value / period_to_seconds(interval)
  }else if(unit == "Dataloss"){
    if(missing(ref)) stop("ref argument in sample count must be provided!")
    value = (1 - value / ref) * 100
    ylab = "value (%)"
    if(min(value) < 0) ylim = c(min(value), 100)
    else ylim = c(0, 100)
    plot(ts, value, type = "h",
         xaxt = 'n', xlim = c(st, et), xlab = "time",
         ylab = ylab, ylim = ylim)
  }

  if(unit != "Dataloss"){
    plot(ts, value, type = "h",
         xaxt = 'n', xlim = c(st, et), xlab = "time",
         ylab = ylab)
    if(!missing(ref)){
      abline(h = ref, lty = "dashed")
      text(x = et, y = ref, labels = paste(ref, unit), adj = c(0.75, -0.5), cex = 0.75)
    }
  }
  axis.POSIXct(1, at = breaks, x = breaks)
  title(paste("Sampling Rate in", unit,"per",interval,
              paste("\n", st,
                    "\n", et,
                    sep="")))
}

#' @name SamplingRate.ggplot
#' @title Plot sampling rate over certain breaks (e.g. min, hour) using ggplot2
#' @export
#' @import lubridate ggplot2
#' @param
#' type: "Count" or "Hz" or "Dataloss".
#' "Count" will display the sample counts;
#' "Hz" will display the sampling rate in Hz (count divided by break time)
#' "Dataloss" will display the sampling rate in data loss percentage (count divided by reference)
SamplingRate.ggplot = function(sr_dat, unit = "Count", ref){
  data = sr_dat
  breaks = pretty_dates(sr_dat[,1], n = 10)
  st = breaks[1]
  et = tail(breaks, 1)
  interval = as.period(new_interval(sr_dat[1,1], sr_dat[2,1]))
  ylab = "value"
  xlab = "time"
  titleText = paste("Sampling Rate in", unit,"per",interval,
                    paste("\n", st,
                          "\n", et,
                          sep=""));


  if(unit == "Hz"){
    data[,2] = data[,2] / period_to_seconds(interval)
  }else if(unit == "Dataloss"){
    if(missing(ref)) stop("ref argument in sample count must be provided!")
    data[,2] = (1 - data[,2] / ref) * 100
    ylab = "value (%)"
    if(min(data[,2]) < 0) ylim = c(min(data[,2]), 100)
    else ylim = c(0, 100)
  }

  p = ggplot(data = data, aes_string(x = MHEALTH_CSV_TIMESTAMP_HEADER, y = MHEALTH_CSV_SAMPLING_RATE_HEADER)) +
    geom_bar(stat="identity", width = 10) +
    labs(title = titleText, x = xlab, y = ylab) + xlim(c(st, et))

  if(unit == "Dataloss"){
    p = p + ylim(ylim)
  }

  if(!missing(ref)){
    p = p + geom_hline(yintercept = ref, linetype = "dashed") +
      annotate("text", x = et, y = ref, label = paste(ref, unit), hjust = 1, vjust = 0)
  }
  p = p + scale_color_gdocs() + theme_bw()
  return(p)
}
