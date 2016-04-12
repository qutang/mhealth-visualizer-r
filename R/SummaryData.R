#' @name SummaryData.simpleMean
#' @title Calculate summary value (mean value) for each column over a certain time break (e.g. hour, min)
#' @export
#' @note If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME
#' @import plyr
#' @note If certain break is not provided or missing, will use the entire sequence
#' @param sensorData input dataframe that matches mhealth sensor data format.
#' @param breaks could be "sec", "min", "hour", "day", "week", "month", "quarter" or "year"; or preceded by an interger and a space.
SummaryData.simpleMean = function(sensorData, breaks){
  nCols = ncol(sensorData)
  if(missing(breaks) || is.null(breaks)){
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER])
  }else{
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks = breaks)
  }
  result = plyr::ddply(sensorData,.(breaks), function(rows){
      meanValues = colMeans(rows[2:nCols], na.rm = TRUE)
      return(meanValues)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  for(i in 2:nCols){
    names(result)[i] = paste("MEAN", names(result)[i],sep="_")
  }
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  return(result)
}

#' @name SummaryData.auc
#' @title Calculate summary value (area under curve) for each column over a certain break (e.g. hour, min).
#' @note If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME.
#' @export
#' @import plyr caTools
#' @param sensorData input dataframe that matches mhealth sensor data format.
#' @param breaks could be "sec", "min", "hour", "day", "week", "month", "quarter" or "year"; or preceded by an interger and a space.
SummaryData.auc = function(sensorData, breaks){
  nCols = ncol(sensorData)
  sensorData[,2:nCols] = abs(sensorData[,2:nCols])
  if(missing(breaks) || is.null(breaks)){
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER])
  }else{
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks = breaks)
  }
  result = plyr::ddply(sensorData,.(breaks), function(rows){
      rows[,1] = as.numeric(rows[,1])
      rows = na.omit(rows)
      if(nrow(rows) > 1){
        aucValues = numcolwise(trapz, x = rows[,1])(rows[2:nCols])
      }else{
        aucValues = as.data.frame(lapply(rows, function(x) rep.int(NA, 1)))
        aucValues = aucValues[2:nCols]
      }
      return(aucValues)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  for(i in 2:nCols){
    names(result)[i] = paste("AUC", names(result)[i],sep="_")
  }
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  return(result)
}

#' @name SummaryData.absoluteMean
#' @title Calculate summary value (absolute mean value) for each column over a certain break (e.g. hour, min).
#' @note If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME.
#' @export
#' @import plyr
#' @param sensorData input dataframe that matches mhealth sensor data format.
#' @param breaks could be "sec", "min", "hour", "day", "week", "month", "quarter" or "year"; or preceded by an interger and a space.
SummaryData.absoluteMean = function(sensorData, breaks){
  nCols = ncol(sensorData)
  if(missing(breaks) || is.null(breaks)){
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER])
  }else{
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks = breaks)
  }
  result = plyr::ddply(sensorData,.(breaks), function(rows){
    meanValues = colMeans(abs(rows[2:nCols]), na.rm = TRUE)
    return(meanValues)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  for(i in 2:nCols){
    names(result)[i] = paste("ABSMEAN", names(result)[i],sep="_")
  }
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  return(result)
}

#' @name SummaryData.ggplot
#' @title Plot summary data using ggplot2
#' @description All columns will be on the same graph with different colors.
#' @export
#' @import lubridate ggplot2 reshape2
#' @param summaryData input dataframe that matches mhealth sensor data format.
#' @param plotType type of plot: "line" or "step"
SummaryData.ggplot = function(summaryData, plotType = "line"){
  data = summaryData

  nCols = ncol(data)
  labelNames = names(data[2:nCols])
  labelNames = c(str_match(labelNames, "[A-Za-z0-9]+_[A-Za-z0-9]+"))
  xlab = "time"
  ylab = "value"
  interval = as.period(new_interval(data[1,MHEALTH_CSV_TIMESTAMP_HEADER], data[2,MHEALTH_CSV_TIMESTAMP_HEADER]))


  if(is.null(range)){
    maxy = max(abs(data[,2:nCols]))
    range = c(-maxy, maxy)*1.1
  }

  breaks = pretty_dates(data[,MHEALTH_CSV_TIMESTAMP_HEADER], n = 6)
  minor_breaks = pretty_dates(data[,MHEALTH_CSV_TIMESTAMP_HEADER], n = 30)
  st = breaks[1]
  et = tail(breaks, 1)
  titleText = paste("Summary data per", interval,
                    paste("\n", st,
                          "\n", et,
                          sep=""))

  data = melt(data, id = c(MHEALTH_CSV_TIMESTAMP_HEADER))

  p = ggplot(data = data, aes_string(x = MHEALTH_CSV_TIMESTAMP_HEADER, y = "value", colour = "variable"))
  if(plotType == "line"){
    p = p + geom_line(alpha = 0.7) + geom_point(alpha = 0.7)
  }else if(plotType == "step"){
    p = p + geom_step(direction = "hv", alpha = 0.7)
  }
  p = p +
    labs(title = titleText, x = xlab, y = ylab, colour = "axes") + xlim(c(st, et))

  p = p + scale_x_datetime(breaks = breaks)

  p = p + scale_color_few(labels = labelNames) + theme_bw() + theme(legend.position="bottom")

  p

  return(p)
}

#' @import lubridate
.SummaryData.getBreaks = function(ts, breaks){
  if(missing(breaks) || is.null(breaks)){
    br = ts[1]
    return(br)
  }else if(str_detect(breaks, "sec")){
    ts[1] = floor_date(ts[1], unit = c("second"))
  }else if(str_detect(breaks, "min")){
    ts[1] = floor_date(ts[1], unit = c("minute"))
  }else if(str_detect(breaks, "hour")){
    ts[1] = floor_date(ts[1], unit = c("hour"))
  }else if(str_detect(breaks, "day")){
    ts[1] = floor_date(ts[1], unit = c("day"))
  }
  br = cut(ts, breaks= breaks)
  return(br)
}

