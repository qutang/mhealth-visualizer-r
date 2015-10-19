MHEALTH_CSV_MEAN_ACCELEROMETER_CALIBRATED_X_HEADER = "MEAN_X_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_MEAN_ACCELEROMETER_CALIBRATED_Y_HEADER = "MEAN_Y_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_MEAN_ACCELEROMETER_CALIBRATED_Z_HEADER = "MEAN_Z_ACCELATION_METERS_PER_SECOND_SQUARED"

MHEALTH_CSV_AUC_ACCELEROMETER_CALIBRATED_X_HEADER = "AUC_X_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_AUC_ACCELEROMETER_CALIBRATED_Y_HEADER = "AUC_Y_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_AUC_ACCELEROMETER_CALIBRATED_Z_HEADER = "AUC_Z_ACCELATION_METERS_PER_SECOND_SQUARED"

#' @name SummaryData.simpleMean
#' @title Calculate summary value (simple mean) over a certain break (e.g. hour, min)
#' @export
#' @import plyr
SummaryData.simpleMean = function(sensorData, breaks = "min"){
  result = plyr::ddply(sensorData,.(cut(HEADER_TIME_STAMP, breaks= breaks)), function(rows){
      meanValues = colMeans(rows[,2:4], na.rm = TRUE)
      return(meanValues)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  names(result)[2] = MHEALTH_CSV_MEAN_ACCELEROMETER_CALIBRATED_X_HEADER
  names(result)[3] = MHEALTH_CSV_MEAN_ACCELEROMETER_CALIBRATED_Y_HEADER
  names(result)[4] = MHEALTH_CSV_MEAN_ACCELEROMETER_CALIBRATED_Z_HEADER
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  return(result)
}

#' @name SummaryData.auc
#' @title Calculate summary value (area under curve) over a certain break (e.g. hour, min)
#' @export
#' @import plyr flux
SummaryData.auc = function(sensorData, breaks = "min", method = 1){
  sensorData[,2:4] = abs(sensorData[,2:4])
  result = plyr::ddply(sensorData,.(cut(HEADER_TIME_STAMP, breaks=breaks)), function(rows){
    rows[,1] = as.numeric(rows[,1])
    aucValues = numcolwise(auc, x = rows[,1])(rows[,2:4])
    return(aucValues)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  names(result)[2] = MHEALTH_CSV_AUC_ACCELEROMETER_CALIBRATED_X_HEADER
  names(result)[3] = MHEALTH_CSV_AUC_ACCELEROMETER_CALIBRATED_Y_HEADER
  names(result)[4] = MHEALTH_CSV_AUC_ACCELEROMETER_CALIBRATED_Z_HEADER
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  return(result)
}

#' @name SummaryData.plot
#' @title Plot summary data
#' @export
#' @import lubridate
SummaryData.plot = function(summaryData, range = NULL){
  ts = summaryData[[MHEALTH_CSV_TIMESTAMP_HEADER]]
  values = summaryData[,2:4]
  ylab = "value"
  interval = as.period(new_interval(ts[1], ts[2]))

  if(is.null(range)){
    maxy = max(abs(values))
    range = c(-maxy, maxy)*1.1
  }

  breaks = pretty_dates(ts, n = 10)
  st = breaks[1]
  et = tail(breaks, 1)

  plot(ts, values[,1], type = "l", lty = "solid",
       xaxt = 'n', xlim = c(st, et), ylim = range, xlab = "time",
       ylab = ylab, col = "red")
  grid()
  lines(x = ts, y = values[,2], type = "l", lty = "solid", col = "blue")
  lines(x = ts, y = values[,3], type = "l", lty = "solid", col = "green")


  axis.POSIXct(1, at = breaks, x = breaks)
  title(paste("Summary data per", interval,
              paste("\n", st,
                    "\n", et,
                    sep="")))
}

#' @name SummaryData.ggplot
#' @title Plot summary data using ggplot2
#' @export
#' @import lubridate ggplot2 reshape2
SummaryData.ggplot = function(summaryData, range = NULL){
  data = summaryData[,1:4]
  xlab = "time"
  ylab = "value"
  interval = as.period(new_interval(data[1,1], data[2,1]))


  if(is.null(range)){
    maxy = max(abs(data[,2:4]))
    range = c(-maxy, maxy)*1.1
  }

  breaks = pretty_dates(data[,1], n = 6)
  minor_breaks = pretty_dates(data[,1], n = 30)
  st = breaks[1]
  et = tail(breaks, 1)
  titleText = paste("Summary data per", interval,
                    paste("\n", st,
                          "\n", et,
                          sep=""))

  data = melt(data, id = 1)

  p = ggplot(data = data, aes_string(x = MHEALTH_CSV_TIMESTAMP_HEADER, y = "value", colour = "variable"))

  p = p + geom_line() +
    labs(title = titleText, x = xlab, y = ylab, colour = "axes") + xlim(c(st, et)) + ylim(range)

  p = p + scale_x_datetime(breaks = breaks)

  p = p + scale_color_few(labels = c("x","y","z")) + theme_bw()

  p

  return(p)
}



