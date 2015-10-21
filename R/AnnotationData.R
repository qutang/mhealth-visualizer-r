MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER = "START_TIME"
MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER = "STOP_TIME"
MHEALTH_CSV_ANNOTATION_LABEL_HEADER = "LABEL_NAME"

#' @name AnnotationData.importCsv
#' @title Import mhealth annotation data file and load into memory as data frame in mhealth format
#' @export
AnnotationData.importCsv = function(filename) {
  op <- options(digits.secs = 3)
  # get the time zone from filename
  tz = gregexpr(pattern = MHEALTH_FILE_TIMESTAMP_TZ_PATTERN, text = filename, perl = TRUE)
  tz = regmatches(filename, tz)[[1]]
  tz = gsub(pattern = "M", replacement = "-", x = tz)
  tz = gsub(pattern = "P", replace = "+", x = tz)
  if (!grepl("annotation.csv", filename))
    stop("Please make sure the raw data file is in annotaiton.csv or annotation.csv.gz format")
  # read.table supports csv.gz directly
  dat = read.table(
    filename, header = TRUE, sep = MHEALTH_CSV_DELIMITER, quote = "\"", stringsAsFactors = FALSE
  )
  # TODO: use the time zone specified in the filename
  dat[,MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(strptime(dat[,MHEALTH_CSV_TIMESTAMP_HEADER], format = MHEALTH_TIMESTAMP_FORMAT))
  dat[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER] = as.POSIXct(strptime(dat[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER], format = MHEALTH_TIMESTAMP_FORMAT))
  dat[,MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER] = as.POSIXct(strptime(dat[,MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER], format = MHEALTH_TIMESTAMP_FORMAT))
  return(dat)
}

#' @name AnnotationData.merge
#' @title merge two or more annotation data frames and sort according to start time
#' @export
AnnotationData.merge = function(annotationDataList, ...){
  if (!missing(annotationDataList)) {
    input = c(annotationDataList, list(...))
  }else{
    input = list(...)
  }
  dat = Reduce(rbind, input)
  dat = dat[order(dat[MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]),]
}

#' @name AnnotationData.getLabelNames
#' @title get all matched label names given a timestamp, return NULL if no match
#' @export
AnnotationData.getLabelNames = function(annotationData, currentTime) {
  if(is.null(currentTime)){
    return(NULL)
  }
  if(sum(class(currentTime) == class(annotationData[1, MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER])) == 0){
    stop("The class type of the timestamp should match the annotation data set")
  }
  criteria = annotationData[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER] <= currentTime &
    annotationData[,MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER] >= currentTime
  labelNames = unique(annotationData[criteria, MHEALTH_CSV_ANNOTATION_LABEL_HEADER])
  return(labelNames)
}

#' @name AnnotationData.addToGgplot
#' @title add annotation bars to an existing ggplot (most likely a sensor data plot)
#' @import foreach RColorBrewer
#' @export
AnnotationData.addToGgplot = function(p, annotationData) {
  categories = unique(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER])
  colors = brewer.pal(n = max(length(categories),3), name = "Paired")
  annotationData["Y_MIN"] = 0
  annotationData["Y_MAX"] = 0
  annotationData["COLOR"] = ""

  if (is.null(p) || sum(class(p) == "ggplot") == 0) {
    warning("p is not a ggplot object, draw annotation on a separate ggplot")
    count = 0
    foreach(cat = categories) %do% {
      annotationData[annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER] == cat,"Y_MIN"] = count
      annotationData[annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER] == cat,"Y_MAX"] = count + 0.8
      count = count + 1
      return(NULL)
    }
    p = ggplot(
      data = annotationData, aes_string(
        xmin = MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER,
        xmax = MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER,
        ymin = "Y_MIN", ymax = "Y_MAX", colour = MHEALTH_CSV_ANNOTATION_LABEL_HEADER,
        fill = MHEALTH_CSV_ANNOTATION_LABEL_HEADER
      )
    )
    p = p + geom_rect(alpha = 0.7)
    p = p + theme_bw() + scale_color_few()
    return(p)
  }else{
    ylimits = max(p$data$value, na.rm = TRUE)
    ylimitsl = min(p$data$value, na.rm = TRUE)
    newYLimit = ylimits*1.3
    # p = p + coord_cartesian(ylim = c(ylimitsl * 1.1, newYLimit))
    locations = seq(ylimits, newYLimit, by = (newYLimit - ylimits)/length(categories))

    count = 1
    foreach(cat = categories) %do% {
      annotationData[annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER] == cat,"Y_MIN"] = locations[count]
      annotationData[annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER] == cat,"Y_MAX"] = locations[count] + diff(locations)[1] * 0.8
      annotationData[annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER] == cat,"COLOR"] = colors[count]
      count = count + 1
      return(NULL)
    }

    p = p + annotate("rect", xmin = annotationData[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER],
                     xmax = annotationData[,MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER],
                     ymin = annotationData[,"Y_MIN"],
                     ymax = annotationData[,"Y_MAX"],
                     color = annotationData[, "COLOR"],
                     fill = annotationData[, "COLOR"],
                     alpha = 0.6
    )

    p = p + annotate("text", x = annotationData[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER],
                     y = annotationData[, "Y_MAX"],
                     label = annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER],
                     hjust = 0, vjust = 1, size = 3)

    p
    return(p)
  }
}
