#' @name SummaryData.simpleMean
#' @title Calculate summary value (simple mean) over a certain break (e.g. hour, min)
#' @export
#' @import plyr
#' @param sensorData: should be compatible with the mhealth sensor data format, first column should be HEADER_TIME_STAMP, and the following arbitrary number of columns should be numeric
SummaryData.simpleMean = function(sensorData, breaks = "min"){
  nCols = ncol(sensorData)
  sensorData$breaks = cut(sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks= breaks)
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
#' @title Calculate summary value (area under curve) over a certain break (e.g. hour, min)
#' @export
#' @import plyr flux
SummaryData.auc = function(sensorData, breaks = "min"){
  nCols = ncol(sensorData)
  sensorData[,2:nCols] = abs(sensorData[,2:nCols])
  sensorData$breaks = cut(sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks = breaks)
  result = plyr::ddply(sensorData,.(breaks), function(rows){
      rows[,1] = as.numeric(rows[,1])
      rows = na.omit(rows)
      if(nrow(rows) > 1){
        aucValues = numcolwise(auc, x = rows[,1])(rows[2:nCols])
      }else{
        aucValues = as.data.frame(lapply(rows, function(x) rep.int(NA, 1)))
        aucValues = aucValues[,2:nCols]
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

#' @name SummaryData.ggplot
#' @title Plot summary data using ggplot2
#' @export
#' @import lubridate ggplot2 reshape2
#' @param summaryData: should be compatible with the mhealth sensor data format, first column should be HEADER_TIME_STAMP, and the following arbitrary number of columns should be numeric
SummaryData.ggplot = function(summaryData){
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

  p = p + geom_line() +
    labs(title = titleText, x = xlab, y = ylab, colour = "axes") + xlim(c(st, et))

  p = p + scale_x_datetime(breaks = breaks)

  p = p + scale_color_few(labels = labelNames) + theme_bw()

  p

  return(p)
}



