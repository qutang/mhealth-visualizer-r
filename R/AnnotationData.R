MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER = "START_TIME"
MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER = "STOP_TIME"
MHEALTH_CSV_ANNOTATION_LABEL_HEADER = "LABEL_NAME"

#' @name AnnotationData.importCsv
#' @title Import mhealth annotation data file and load into memory as data frame in mhealth format.
#' @export
#' @param filename full file path for input annotation data file.
#' @note Time zone is from local computer for now. But should be changed to use filename in the future.
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
    filename, header = TRUE, sep = MHEALTH_CSV_DELIMITER, stringsAsFactors = FALSE, fill = TRUE,row.names = NULL
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
#' @param annotationDataList list of annotation data frames
#' @param ... other optional annotation data frames
AnnotationData.merge = function(annotationDataList, ...){
  if (!missing(annotationDataList)) {
    input = c(annotationDataList, list(...))
  }else{
    input = list(...)
  }
  if(length(input) == 0){
    return(NULL)
  }
  dat = Reduce(rbind, input)
  dat = dat[order(dat[MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]),]
}

#' @name AnnotationData.clip
#' @export
#' @title Clip annotation data according to the start and end time
#' @note Make sure that the data frame is compatible with mhealth annotation data file format.
#' @param annotationData annotation data frame that matches mhealth specification.
#' @param startTime POSIXct date object for start timestamp.
#' @param endTime POSIXct date object for start timestamp.
#' @return clipped annotation dataframe
AnnotationData.clip = function(annotationData, startTime, endTime){
  clippedTs = annotationData[[MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER]] >= startTime & annotationData[[MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]] <= endTime
  result = annotationData[clippedTs,]
  result[1,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER] = as.POSIXct(startTime)
  result[nrow(result), MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER] = as.POSIXct(endTime)
  return(result)
}

#' @name AnnotationData.offset
#' @title offset annotation data's start and stop timestamp by an offset value in seconds
#' @param annotationData annotation dataframe that matches mhealth specification.
#' @param offsetValue value in seconds specifies the offset time, could be negative, meaning go back to some time earlier. The default is 0, meaning no offset.
#' @export
#' @return annotation dataframe after timestamps being offset.
AnnotationData.offset = function(annotationData, offsetValue = 0){
  annotationData[[MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]] = annotationData[[MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]] + offsetValue
  annotationData[[MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER]] = annotationData[[MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER]] + offsetValue
  return(annotationData)
}

#' @name AnnotationData.filter.original
#' @title filter out/only include labels/categories presented in the input list
#' @import stringr
#' @export
AnnotationData.filter.original = function(annotationData, ontologyData, labels = NULL, categories = NULL, include.labels = FALSE, include.categories = FALSE){
  if(is.null(labels) && is.null(categories)){
    warning("labels and categories are null, return the original annotation data frame")
    return(annotationData)
  }else{
    filter_condition = logical(length = length(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER]))
    if(!is.null(labels)){
      
      label_filter_list = lapply(labels, function(label){
        return(str_detect(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER], pattern = label))
      })
      label_filter_condition = Reduce(f = "|", label_filter_list)
      filter_condition = label_filter_condition
      if(!include.labels) filter_condition = !filter_condition;
    }
    
    if(!is.null(categories) && !missing(ontologyData)){
      category_labels = ontologyData[str_to_lower(ontologyData[,2]) %in% str_to_lower(categories), 1]
      category_filter_list = lapply(category_labels, function(label){
        return(str_detect(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER], pattern = label))
      })
      category_filter_condition = Reduce(f = "|", category_filter_list)

      if(!include.categories) category_filter_condition = !category_filter_condition;
      filter_condition = filter_condition | category_filter_condition
    }

    return(annotationData[filter_condition,])
  }
}

#' @name AnnotationData.getLabelNames
#' @title get all matched label names given a timestamp, return NULL if no match.
#' @export
#' @param annotationData input annotation dataframe that matches mhealth specification
#' @param currentTime POSIXct date object of a timestamp to search for the corresponding labels.
AnnotationData.getLabelNames = function(annotationData, currentTime) {
  if(is.null(currentTime)){
    return(NULL)
  }
  if(sum(class(currentTime) == class(annotationData[1, MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER])) == 0 && sum(class(currentTime) == class(as.numeric(annotationData[1, MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]))) == 0){
    stop("The class type of the timestamp should match the annotation data set")
  }
  criteria = floor(as.numeric(annotationData[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER])) < as.numeric(currentTime) &
    floor(as.numeric(annotationData[,MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER])) >= as.numeric(currentTime)
  labelNames = unique(annotationData[criteria, MHEALTH_CSV_ANNOTATION_LABEL_HEADER])
  return(labelNames)
}

#' @name AnnotationData.simplify
#' @title simplify all annotations so that concurrent activities will be concatnated and timestamps will be connected
#' @import stringr
#' @export
AnnotationData.simplify = function(annotationData) {
  sts = annotationData[,MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER]
  ets = annotationData[,MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER]
  ts = sort(unique(floor(as.numeric(c(sts, ets)))), decreasing = FALSE)
  simplified = ldply(1:(length(ts)-1), function(index){
    chunk = mean(c(ts[index], ts[index + 1]))
    labels = AnnotationData.getLabelNames(annotationData, chunk)
    if(length(labels) > 0){
      result = data.frame(ts = ts[index], st = ts[index], et = ts[index+1], label = paste(labels, collapse = ","))
    }else{
      result = data.frame(ts = ts[index], st = ts[index], et = ts[index+1], label = "Unlabeled")
    }
    return(result)
  })
  simplified[,"ts"] = as.POSIXct(simplified[,"ts"], origin = "1970-01-01")
  simplified[,"st"] = as.POSIXct(simplified[,"st"], origin = "1970-01-01")
  simplified[,"et"] = as.POSIXct(simplified[,"et"], origin = "1970-01-01")
  colnames(simplified) = c(MHEALTH_CSV_TIMESTAMP_HEADER, MHEALTH_CSV_ANNOTATION_STARTTIME_HEADER, MHEALTH_CSV_ANNOTATION_STOPTIME_HEADER, MHEALTH_CSV_ANNOTATION_LABEL_HEADER)
  return(simplified)
}

#' @name AnnotationData.simplify.filter
#' @title filter out/include only simplified annotation data frame with the presented labels
#' @export
#' @import stringr
AnnotationData.simplify.filter = function(annotationData, labels, include.labels = TRUE, and.logic = TRUE){
  if(missing(labels)){
    return(annotationData)
  }else{
    annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER] = tolower(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER])
    criteria = logical(nrow(annotationData))
    if(and.logic){
      criteria = !criteria
      for(label in labels){
        label = tolower(label)
        if(!include.labels) {
          criteria = criteria & !str_detect(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER], label)
        }else{
          criteria = criteria & str_detect(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER], label)
        }
        
      }
    }else{
      for(label in labels){
        label = tolower(label)
        criteria = criteria | str_detect(annotationData[,MHEALTH_CSV_ANNOTATION_LABEL_HEADER], label)
      }
    }
    
    return(annotationData[criteria,])
  }
}

#' @name AnnotationData.addToGgplot
#' @title add annotation bars to an existing ggplot (most likely a sensor data plot) or if no existing ggplot object is provided, create an annotation graph.
#' @import foreach RColorBrewer
#' @export
#' @param p an existing ggplot object. Often to be the one with sensor data.
#' @param annotationData input annotation dataframe that matches mhealth specification.
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
    if(is.null(p$coordinates$limits$y)){
      pureNumbers = p$data[,-grep ("Date|factor|character|POSIXct|POSIXlt", sapply (p$data, class))]
      ylimits = max(pureNumbers, na.rm = TRUE)
      ylimitsl = min(pureNumbers, na.rm = TRUE)
    }else{
      ylimits = p$coordinates$limits$y[2]
      ylimitsl = p$coordinates$limits$y[1]
    }
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
