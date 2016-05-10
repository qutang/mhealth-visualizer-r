MHEALTH_CSV_TIMESTAMP_HEADER = "HEADER_TIME_STAMP"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER = "X_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER = "Y_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER = "Z_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_COUNT_X_HEADER = "X_ACTIVITY_COUNT"
MHEALTH_CSV_COUNT_Y_HEADER = "Y_ACTIVITY_COUNT"
MHEALTH_CSV_COUNT_Z_HEADER = "Z_ACTIVITY_COUNT"

#' @name SensorData.importCsv
#' @title Import mhealth sensor data file and load into memory as data frame in mhealth format.
#' @note input file must match mhealth specification. Note that the time zone of timestamps will be based on local computer instead of the filename, this needs to be changed.
#' @param filename full file path of input sensor data file.
#' @param violate violate file name convention, ignore time zones and other information in file name
#' @export
#' @import readr
#' @seealso [`SensorData.importBinary`](SensorData.importBinary.html), [`SensorData.importGT3X`](SensorData.importGT3X.html), [`SensorData.importActigraphCsv`](SensorData.importActigraphCsv.html)

SensorData.importCsv = function(filename, violate = FALSE) {
  op <- options(digits.secs = 3)
  # get the time zone from filename
  if(!violate){
    tz = gregexpr(pattern = MHEALTH_FILE_TIMESTAMP_TZ_PATTERN, text = filename, perl = TRUE)
    tz = regmatches(filename, tz)[[1]]
    tz = gsub(pattern = "M", replacement = "-", x = tz)
    tz = gsub(pattern = "P", replace = "+", x = tz)
    if (!grepl("csv", filename))
      stop("Please make sure the raw data file is in csv or csv.gz format")
  }
  # read.table supports csv.gz directly
  ncols = count_fields(filename, tokenizer_csv(), n_max = 1)
  colTypes = paste(c("c", rep("d", ncols - 1)),collapse = "")
  dat = read_csv(
    filename, col_names = TRUE, trim_ws = TRUE, col_types = colTypes)
  # TODO: use the time zone specified in the filename
  dat$HEADER_TIME_STAMP = as.POSIXct(strptime(dat$HEADER_TIME_STAMP, format = MHEALTH_TIMESTAMP_FORMAT))
  dat = as.data.frame(dat)
  return(dat)
}

#' @name SensorData.importBinary
#' @title Import and decode binary file from the smart watch and load into dataframe as mhealth format.
#' @note It will call `SensorData.importCsv` after decoding.
#' @export
#' @import rJava
#' @param filename full file path of input smart watch binary data file.
#' @param dest full directory path of destination folder. Default is ".fromBinary" folder of current working directory.
#' @seealso [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importGT3X`](SensorData.importGT3X.html), [`SensorData.importActigraphCsv`](SensorData.importActigraphCsv.html)
SensorData.importBinary = function(filename, dest = file.path(getwd(), ".fromBinary")) {
  if (dir.exists(dest)) {
    unlink(dest, recursive = TRUE, force = TRUE)
  }
  dir.create(dest, recursive = TRUE)
  paras = c(filename, dest)
  J("edu.neu.mhealthformat.utils.converter.WatchBinaryDecoder")$main(.jarray(paras))
  # load iteratively into dataframe
  csvFile = list.files(path = dest, full.names = TRUE)[1]
  return(SensorData.importCsv(csvFile))
}

#' @name SensorData.importGT3X
#' @title Import and decode GT3X files and load into dataframe as mhealth format.
#' @export
#' @import rJava
#' @note it will call `SensorData.importCsv` after decoding GT3X binary data.
#' @param filename full file path of input gt3x binary data file, should have extension "gt3x".
#' @param dest full directory path of destination folder. Default is ".fromGT3X" folder of current working directory.
#' @param split Whether to split input data into hourly dataframe list.
#' @return list of dataframes storing decoded gt3x sensor data file.
#' @seealso [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importBinary`](SensorData.importBinary.html), [`SensorData.importActigraphCsv`](SensorData.importActigraphCsv.html)
SensorData.importGT3X = function(filename, dest = file.path(getwd(), ".fromGT3X"), split = FALSE) {
  dir.create(dest, recursive = TRUE)
  if (split) {
    para_split = "SPLIT"
  }else{
    para_split = "NO_SPLIT"
  }
  paras = c(filename, dest, "G_VALUE", "WITH_TIMESTAMP", para_split)
  J("com.qmedic.data.converter.gt3x.ConverterMain")$main(.jarray(paras))

  # load iteratively into dataframe
  csvFiles = list.files(dest, pattern = ".csv", full.names = TRUE, recursive = TRUE)
  datList = lapply(csvFiles, function(file) {
    return(SensorData.importCsv(filename = file))
  })
}

#' @name SensorData.importActigraphCsv
#' @title Import and convert Actigraph raw csv files and load into data frame as in mhealth format.
#' @export
#' @import readr
#' @note Please make sure the Actigraph raw csv file has timestamp included. The Actigraph raw csv file is not IMU csv file supported by GT9X.
#' @param filename full file path of input Actigraph raw csv file.
#' @param ad_convert set as TRUE only when the input Actigraph csv file is in analog quantized format and need to be converted into g value
#' @param ts_provided set as TRUE only when timestamp is provided as the first column
#' @param header_provided set as TRUE only when column header is provided
#' @seealso [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importGT3X`](SensorData.importGT3X.html), [`SensorData.importBinary`](SensorData.importBinary.html)
SensorData.importActigraphCsv = function(filename, ad_convert = FALSE, ts_provided = TRUE, header_provided = TRUE) {
  actigraphHeader = SensorData.parseActigraphCsvHeader(filename)
  if(header_provided){
    dat = read_csv(
      filename, col_names = FALSE, skip = 11);
  }else{
    dat = read_csv(
      filename, col_names = FALSE, skip = 10);
  }
  if(!ts_provided){
    ts_col = seq(from = actigraphHeader$st, to = actigraphHeader$dt, length.out = nrow(dat))
    dat = cbind(ts_col, dat)
  }
  
  dat = dat[,1:4]
  
  names(dat) = c(
    MHEALTH_CSV_TIMESTAMP_HEADER,
    MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER,
    MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER,
    MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER
  )
  
  if(ts_provided){
    timeFormat = ifelse(test = actigraphHeader$imu,
                        yes = ACTIGRAPH_IMU_TIMESTAMP,
                        no = ACTIGRAPH_TIMESTAMP)
    dat[[MHEALTH_CSV_TIMESTAMP_HEADER]] = strptime(x = dat[[MHEALTH_CSV_TIMESTAMP_HEADER]],
                                                   format = timeFormat) + 0.0005
  }
  
  options(digits.secs = 3);
  dat = as.data.frame(dat);
  
  if(ad_convert){
    vs = actigraphHeader$vs
    res = actigraphHeader$res
    
    dat[,2:ncol(dat)] = (dat[,2:ncol(dat)] * vs / (2^res) - vs/2)/(vs/10)
    dat[,2:ncol(dat)] = as.data.frame(apply(dat[,2:ncol(dat)], 2, function(col){
      col[col == -5] = 0
      return(as.numeric(col))
    }))
  }
  return(dat)
}

#' @name SensorData.importActigraphCountCsv
#' @title Import and convert Actigraph count csv files and load into data frame as in mhealth format.
#' @export
#' @import readr
#' @param filename full file path of input Actigraph count csv file.
#' @seealso [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importGT3X`](SensorData.importGT3X.html), [`SensorData.importBinary`](SensorData.importBinary.html)
SensorData.importActigraphCountCsv = function(filename, count_col, count_col_name) {
  dat = read_csv(
    filename, col_names = TRUE, skip = 10, col_types = cols(timestamp = col_character(), vectormagnitude = col_double())
  );
  dat = as.data.frame(dat[,c(1, count_col)])
  
  dat[,1] = as.POSIXct(dat[,1], format = MHEALTH_TIMESTAMP_FORMAT)
  if(missing(count_col_name)){
    count_col_name = colnames(dat)[-1];
  }
  colnames(dat) = c(MHEALTH_CSV_TIMESTAMP_HEADER, count_col_name);
  return(dat)
}

#' @name SensorData.createActigraphCsvHeader
#' @title create a character vector representing each line of the actigraph csv header
#' @export
#' @import stringr
SensorData.createActigraphCsvHeader = function(startTime, downloadTime, samplingRate, sensorId, firmVersion, softVersion){
  line = c("------------ Data File Created By ActiGraph GT3X+ ActiLife vSOFT_VERSION Firmware vFIRM_VERSION date format M/d/yyyy at SAMPLING_RATE Hz  Filter Normal -----------")
  line = str_replace(line, "SOFT_VERSION", softVersion)
  line = str_replace(line, "FIRM_VERSION", firmVersion)
  line = str_replace(line, "SAMPLING_RATE", samplingRate)
  
  line = c(line, str_replace("Serial Number: ID", "ID", sensorId))
  line = c(line, str_replace("Start Time START_TIME", "START_TIME", format(startTime, "%H:%M:%S")))
  line = c(line, str_replace("Start Date START_DATE", "START_DATE", format(startTime, "%m/%d/%Y")))
  line = c(line, "Epoch Period (hh:mm:ss) 00:00:00")
  line = c(line, str_replace("Download Time DOWNLOAD_TIME", "DOWNLOAD_TIME", format(downloadTime, "%H:%M:%S")))
  line = c(line, str_replace("Download Date DOWNLOAD_DATE", "DOWNLOAD_DATE", format(downloadTime, "%m/%d/%Y")))
  line = c(line, "Current Memory Address: 0")
  line = c(line, "Current Battery Voltage: 4.19     Mode = 12")
  line = c(line, "--------------------------------------------------")
  line = c(line, ACTIGRAPH_HEADER_COLUMNS)
  return(line) 
}

#' @name SensorData.merge
#' @export
#' @title Merge two or more mhealth data frames by rows and sorted by timestamp, duplicated rows will be removed based on timestamp.
#' @note Make sure that the data frame is including timestamps.
#' @param listOfData list of input dataframes that matches mhealth specification.
#' @param ... other optional input dataframes that matches mhealth specification.
SensorData.merge = function(listOfData, ...) {
  if (!missing(listOfData)) {
    input = c(listOfData, list(...))
  }else{
    input = list(...)
  }
  dat = Reduce(rbind, input)
  dat = dat[!duplicated(dat[,MHEALTH_CSV_TIMESTAMP_HEADER]),] # remove duplication
  dat = dat[order(dat[MHEALTH_CSV_TIMESTAMP_HEADER]),]
  return(dat)
}

#' @name SensorData.cleanup
#' @export
#' @title Clean up sensor dataframe by removing invalid timestamps, according to a certain time level.
#' @description For example, if level is "year", sensor data will be trimmed according to the provided `ref` date's year; or if `gt` is not provided, the reference value of "year" level will be the most frequent year.
#' @note Make sure that the data frame is including timestamps.
#' @param sensorData input dataframe that matches mhealth specification
#' @param level "second", "minute", "hour", "day", "month", or "year"; used to trim data that doesn't match the reference value.
#' @param ref the reference date string for the certain time "level". E.g. "2015-10", for level "month"; the string format should follow "%Y-%m-%d %H:%M:%S".
SensorData.cleanup = function(sensorData, level = "year", ref = NULL){
  # extract a valid date
  pattern = switch(level,
         year = "%Y",
         month = "%Y-%m",
         day = "%Y-%m-%d",
         hour = "%Y-%m-%d %H",
         minute = "%Y-%m-%d %H:%M",
         second = "%Y-%m-%d %H:%M:%S")
  validDates = format(sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], pattern)
  if(is.null(ref)){
    countDates = as.data.frame(table(validDates))
    validDate = as.character(countDates$validDates[countDates$Freq == max(countDates$Freq)])
  }else{
    validDate = as.character(ref)
  }
  sensorData = sensorData[validDates == validDate,]
  return(sensorData)
}

#' @name SensorData.interpolate
#' @title Interpolate the missing points and unify sampling interval for the input sensor data
#' @export
#' @import akima plyr
SensorData.interpolate = function(sensorData, method = "spline_natural", polyDegree = 3){
    nRows = nrow(sensorData);
    nCols = ncol(sensorData);
    colLinearInterp = colwise(approx, x = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]], method = "linear", n = nRows)
    colSplineFmmInterp = colwise(spline, x = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]], method = "fmm", n = nRows)
    colSplineNaturalInterp = colwise(spline, x = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]], method = "natural", n = nRows)
    colAsplineOriginalInterp = colwise(aspline, x = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]], method = "original", n = nRows)
    colAsplineImprovedInterp = colwise(aspline, x = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]], method = "improved", n = nRows, degree = polyDegree)

    output = switch(method,
                    linear = colLinearInterp(y = sensorData[,2:nCols]),
                    spline_fmm = colSplineFmmInterp(y = sensorData[,2:nCols]),
                    spline_natural = colSplineNaturalInterp(y = sensorData[,2:nCols]),
                    aspline_original = colAsplineOriginalInterp(y = sensorData[,2:nCols]),
                    aspline_improved = colAsplineImprovedInterp(y = sensorData[,2:nCols]))

    names(output)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
    output[,MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXlt(output[,MHEALTH_CSV_TIMESTAMP_HEADER], origin = "1970-01-01")
    output = as.data.frame(output)
    return(output)
}

#' @name SensorData.clip
#' @export
#' @title Clip sensor data according to the start and end time.
#' @note Make sure that the data frame is including timestamps.
#' @param sensorData input dataframe that matches mhealth specification.
#' @param startTime POSIct date object for start time.
#' @param endTime POSIct date object for end time.
SensorData.clip = function(sensorData, startTime, endTime){
  clippedTs = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]] >= startTime & sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]] <= endTime
  return(sensorData[clippedTs,])
}

#' @name SensorData.split
#' @title Split sensor data into list of smaller data frame with meaningful intervals (e.g. hourly, minutely, secondly or daily)
#' @import plyr
#' @param sensorData input dataframe that matches mhealth specification.
#' @param breaks "sec","min","hour","day","week","month","quarter" or "year"; or preceded by integer and space.
#' @export
#' @return list of splitted dataframes
SensorData.split = function(sensorData, breaks = "hour"){
  result = plyr::dlply(sensorData,.(cut(HEADER_TIME_STAMP, breaks= breaks)), function(x)return(x))
  return(result)
}

#' @name SensorData.offset
#' @title offset sensor data's timestamp by an offset value in seconds
#' @param sensorData input dataframe that matches mhealth specification.
#' @param offsetValue value in seconds specifies the offset time, could be negative, meaning go back to some time earlier. The default is 0, meaning no offset.
#' @export
#' @return dataframe after timestamps being offset
SensorData.offset = function(sensorData, offsetValue = 0){
  sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]] = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]] + offsetValue
  return(sensorData)
}

#' @name SensorData.plot
#' @title Plot nicely the raw sensor data data frame.
#' @description All columns will be plotted on the same graph.
#' @export
#' @param sensorData input dataframe that matches mhealth specification.
SensorData.plot = function(sensorData){
  par(mfrow=c(3,1), mai=c(0.5,0.5,0.5,0.5))
  ts = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]]
  x = sensorData[[MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER]]
  y = sensorData[[MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER]]
  z = sensorData[[MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER]]
  cols = gdocs_pal()(3)
  par(mai=c(0,1,1,1))
  plot(ts, x, type = "o", col = cols[1], xaxt = "n")
  par(mai=c(0,1,0,1))
  plot(ts, y, type = "o", col = cols[2], xaxt = "n")
  par(mai=c(1,1,0,1))
  plot(ts, z, type = "o", col = cols[3])
}

#' @name SensorData.ggplot
#' @title Plot sensor raw data using ggplot2.
#' @description All columns will be plotted on the same graph with different colors.
#' @export
#' @import lubridate ggplot2 reshape2
#' @param sensorData input dataframe that matches mhealth specification.
SensorData.ggplot = function(sensorData){
  data = sensorData
  nCols = ncol(data)
  labelNames = names(data[2:nCols])
  labelNames = c(str_match(labelNames, "[A-Za-z0-9]+_[A-Za-z0-9]+"))
  xlab = "time"
  ylab = "value"

  if(is.null(range)){
    maxy = max(abs(data[,2:nCols]))
    range = c(-maxy, maxy)*1.1
  }

  breaks = pretty_dates(data[,MHEALTH_CSV_TIMESTAMP_HEADER], n = 6)
  minor_breaks = pretty_dates(data[,MHEALTH_CSV_TIMESTAMP_HEADER], n = 30)
  st = breaks[1]
  et = tail(breaks, 1)
  titleText = paste("Raw data plot",
                    paste("\n", st,
                          "\n", et,
                          sep=""))

  data = melt(data, id = c(MHEALTH_CSV_TIMESTAMP_HEADER))

  p = ggplot(data = data, aes_string(x = MHEALTH_CSV_TIMESTAMP_HEADER, y = "value", colour = "variable"))

  p = p + geom_line(lwd = 1.2)
  
  p = p +
    labs(title = titleText, x = xlab, y = ylab, colour = "axes") + xlim(c(st, et))

  p = p + scale_x_datetime(breaks = breaks)

  p = p + scale_color_few(labels = labelNames) + theme_bw() + theme(legend.position="bottom")

  p

  return(p)
}


#' @name SensorData.bokehplot
#' @title Plot sensor raw data using rbokeh interatively
#' @description All columns will be plotted on the same graph with different colors.
#' @export
#' @import lubridate ggplot2 reshape2 rbokeh
#' @param sensorData input dataframe that matches mhealth specification.
SensorData.bokehplot = function(sensorData){
  data = sensorData
  nCols = ncol(data)
  labelNames = names(data[2:nCols])
  labelNames = c(str_match(labelNames, "[A-Za-z0-9]+_[A-Za-z0-9]+"))
  xlab = "time"
  ylab = "value"

  if(is.null(range)){
    maxy = max(abs(data[,2:nCols]))
    range = c(-maxy, maxy)*1.1
  }

  breaks = pretty_dates(data[,MHEALTH_CSV_TIMESTAMP_HEADER], n = 6)
  minor_breaks = pretty_dates(data[,MHEALTH_CSV_TIMESTAMP_HEADER], n = 30)
  st = breaks[1]
  et = tail(breaks, 1)
  titleText = paste("Raw data plot",
                    paste("\n", st,
                          "\n", et,
                          sep=""))

  data = melt(data, id = c(MHEALTH_CSV_TIMESTAMP_HEADER))

  p = figure(data = data, title = titleText, xlim = c(st, et), xlabel = xlab, ylabel = ylab)

  p = p %>% ly_lines(x = HEADER_TIME_STAMP, y = value, color = variable, width = 1.2) %>%
    theme_plot(title_text_font_size = "10pt")

  p

  return(p)
}

#' @name SensorData.getSamplingRate
#' @title Get sensor data's sampling rate from the time difference of adjacent samples
#' @export
SensorData.getSamplingRate = function(sensorData){
  interval = which(sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER] == sensorData[1,MHEALTH_CSV_TIMESTAMP_HEADER] + 1)
  sr = round(interval/10)*10
  return(sr)
}

#' @name SensorData.getFilenameParts
#' @export
#' @import stringr
#' @title Get the mhealth filename parts out of a mhealth sensor data file name
SensorData.getFilenameParts = function(filename){
  if(!str_detect(filename, MHEALTH_FILE_NAME_REGEX_PATTERN)){
    stop(paste0("Not a valid mhealth file name: " + filename))
    return
  }
  tokens = str_split(filename, "\\.")
  section1_tokens = str_split(tokens[[1]], "-")
  section2_tokens = str_split(tokens[[2]], "-")
  sensorType = section1_tokens[[1]]
  dataType = section1_tokens[[2]]
  versionCode = section1_tokens[[3]]
  sensorId = section2_tokens[[1]]
  startTime = as.POSIXct(substr(tokens[[3]], 1, length(tokens[[3]]) - 6), format = MHEALTH_FILE_TIMESTAMP_FORMAT)
  timeZone = substr(tokens[[3]], length(tokens[[3]]) - 5, length(tokens[[3]]))
  return(list(
    sensorType = sensorType,
    dataType = dataType,
    versionCode = versionCode,
    sensorId = sensorId,
    startTime = startTime,
    timeZone = timeZone
  ))
}

#' @name SensorData.parseActigraphCsvHeader
#' @title parse actigraph csv header to get related version and sampling rate information
#' @export
#' @import stringr R.utils
SensorData.parseActigraphCsvHeader = function(filename, header = TRUE) {
  headlines = readLines(filename, n = 10, encoding = "UTF-8");

  # Sampling rate
  sr_pattern = ACTIGRAPH_HEADER_SR_PATTERN
  sr = headlines[[1]]
  sr = str_match(sr, sr_pattern)
  sr = as.numeric(sr[2])

  # Firmware code
  fw_pattern = ACTIGRAPH_HEADER_FIRMWARE_PATTERN
  fw = headlines[[1]]
  fw = str_match(fw, fw_pattern)
  fw = fw[2]

  # Software code
  sw_pattern = ACTIGRAPH_HEADER_SOFTWARE_PATTERN
  sw = headlines[[1]]
  sw = str_match(sw, sw_pattern)
  sw = sw[2]

  # Serial number
  sn_pattern = ACTIGRAPH_HEADER_SERIALNUM_PATTERN
  sn = headlines[[2]]
  sn = str_match(sn, sn_pattern)
  sn = sn[2]

  # actigraph type
  at = substr(sn, 1, 3)

  # IMU or not
  if (str_detect(headlines[[1]], "IMU")) {
    imu = TRUE
  }else{
    imu = FALSE
  }

  # Session start time
  st = headlines[[3]]
  sd = headlines[[4]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+";
  dateReg = "[0-9]+/[0-9]+/[0-9]{4}";
  st = regmatches(st, regexpr(timeReg, st, perl = TRUE))
  sd = regmatches(sd, regexpr(dateReg, sd, perl = TRUE))
  st = paste(sd, st, sep = ' ')
  timeFormat = ACTIGRAPH_TIMESTAMP
  st = strptime(st, timeFormat) + 0.0005
  options(digits.secs = 3);

  # Session download time
  dt = headlines[[6]]
  dd = headlines[[7]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+";
  dateReg = "[0-9]+/[0-9]+/[0-9]{4}";
  dt = regmatches(dt, regexpr(timeReg, dt, perl = TRUE))
  dd = regmatches(dd, regexpr(dateReg, dd, perl = TRUE))
  dt = paste(dd, dt, sep = ' ')
  timeFormat = ACTIGRAPH_TIMESTAMP
  dt = strptime(dt, timeFormat) + 0.0005
  options(digits.secs = 3);
  
  if(is.na(sr)){
    # determine sr by start and download time
    # options(digits = 13)
    duration = as.numeric(dt - st, units = "secs")
    if(header){
      nlines = countLines(filename) - 11
    }else{
      nlines = countLines(filename) - 10
    }
    sr = as.numeric(ceiling(nlines / duration))
  }
  
  # input voltage
  vs = headlines[[9]]
  vsReg = ": ([0-9](\\.[0-9]+)*)"
  vs = as.numeric(str_match(vs, vsReg)[2])
  
  # input resolution
  resolution = headlines[[9]]
  resReg = "= ([0-9]+)"
  resolution = as.numeric(str_match(resolution, resReg)[2])

  # header object as output
  header = {
  }
  header$sr = sr
  header$fw = fw
  header$sw = sw
  header$sn = sn
  header$st = st
  header$dt = dt
  header$at = at
  header$imu = imu
  header$vs = vs
  header$res = resolution

  return(header)
}

#' @import stringr
.SensorData.parseGT3XHeader = function(filename) {
  fromTicksToMs = function(ticks) {
    TICKS_AT_EPOCH = 621355968000000000;
    TICKS_PER_MILLISECOND = 10000;
    ms = (ticks - TICKS_AT_EPOCH) / TICKS_PER_MILLISECOND;
    sec = ms / 1000;
    return(sec)
  }
  # save in a hidden tmp folder
  tmpFolder = ".fromGT3X"
  unzip(file, overwrite = TRUE, exdir = tmpFolder)
  infoFile = file.path(tmpFolder, ACTIGRAPH_GT3X_HEADER_FILENAME)
  headerStr = paste(readLines(infoFile), collapse = " ")

  # Sampling rate
  sr_pattern = ACTIGRAPH_GT3X_HEADER_SR_PATTERN
  sr = headerStr
  sr = str_match(sr, sr_pattern)
  sr = as.numeric(sr[2])

  # Firmware code
  fw_pattern = ACTIGRAPH_GT3X_HEADER_FIRMWARE_PATTERN
  fw = headerStr
  fw = str_match(fw, fw_pattern)
  fw = fw[2]

  # Serial number
  sn_pattern = ACTIGRAPH_GT3X_HEADER_SERIALNUM_PATTERN
  sn = headerStr
  sn = str_match(sn, sn_pattern)
  sn = sn[2]

  # actigraph type
  at = substr(sn, 1, 3)

  # device type
  device_pattern = ACTIGRAPH_GT3X_HEADER_DEVICETYPE_PATTERN
  deviceType = headerStr
  deviceType = str_match(deviceType, device_pattern)
  deviceType = deviceType[2]

  # Session start time
  st_pattern = ACTIGRAPH_GT3X_HEADER_STARTDATE_PATTERN
  st = str_match(headerStr, st_pattern)
  timeFormat = ACTIGRAPH_TIMESTAMP
  st = fromTicksToMs(as.numeric(st[2]))
  st = as.POSIXct(st, "GMT", origin = "1970-01-01")
  options(digits.secs = 3);

  # Session download time
  dt_pattern = ACTIGRAPH_GT3X_HEADER_DOWNLOADTIME_PATTERN
  dt = str_match(headerStr, dt_pattern)
  timeFormat = ACTIGRAPH_TIMESTAMP
  dt = fromTicksToMs(as.numeric(dt[2]))
  dt = as.POSIXct(dt, "GMT", origin = "1970-01-01")
  options(digits.secs = 3);

  # Dynamic range
  dr_pattern = ACTIGRAPH_GT3X_HEADER_RANGE_PATTERN
  dr = str_match(headerStr, dr_pattern)
  dr = as.numeric(dr[2])
  options(digits.secs = 3);

  # header object as output
  header = {
  }
  header$sr = sr
  header$fw = fw
  header$sw = 'ownparser'
  header$sn = sn
  header$st = st
  header$dt = dt
  header$at = at
  header$dr = dr
  header$deviceType = deviceType

  return(header)
}

