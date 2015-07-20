
MHEALTH_CSV_TIMESTAMP_HEADER = "HEADER_TIME_STAMP"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER = "X_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER = "Y_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER = "Z_ACCELATION_METERS_PER_SECOND_SQUARED"

#' @name SensorData.importCsv
#' @title Import mhealth sensor data file and load into memory as data frame in mhealth format
#' @export
SensorData.importCsv = function(filename) {
  op <- options(digits.secs = 3)
  # get the time zone from filename
  tz = gregexpr(pattern = MHEALTH_FILE_TIMESTAMP_TZ_PATTERN, text = filename, perl = TRUE)
  tz = regmatches(filename, tz)[[1]]
  tz = gsub(pattern = "M", replacement = "-", x = tz)
  tz = gsub(pattern = "P", replace = "+", x = tz)
  if (!grepl("csv", filename))
    stop("Please make sure the raw data file is in csv or csv.gz format")
  # read.table supports csv.gz directly
  dat = read.table(
    filename, header = TRUE, sep = MHEALTH_CSV_DELIMITER, quote = "\"", stringsAsFactors = FALSE
  )
  # TODO: use the time zone specified in the filename
  dat$HEADER_TIME_STAMP = as.POSIXct(strptime(dat$HEADER_TIME_STAMP, format = MHEALTH_TIMESTAMP_FORMAT))
  return(dat)
}

#' @name SensorData.importBinary
#' @title Import and decode binary file from the smart watch and load into dataframe as mhealth format
#' @description The default destination directory for the decoded file is stored in .fromBinary folder of current working directory
#' @export
#' @import rJava
SensorData.importBinary = function(filename, dest = file.path(getwd(), ".fromBinary")) {
  if(dir.exists(dest)){
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
#' @title Import and decode GT3X files and load into dataframe as mhealth format
#' @export
#' @import rJava
#' @description The default destination folder will be .fromGT3X in current working directory
SensorData.importGT3X = function(filename, dest = file.path(getwd(), ".fromGT3X"), split = FALSE){
  dir.create(dest, recursive = TRUE)
  if(split){
    para_split = "SPLIT"
  }else{
    para_split = "NO_SPLIT"
  }
  paras = c(filename, dest, "G_VALUE", "WITH_TIMESTAMP", para_split)
  J("com.qmedic.data.converter.gt3x.ConverterMain")$main(.jarray(paras))

  # load iteratively into dataframe
  csvFiles = list.files(dest, pattern = ".csv", full.names = TRUE, recursive = TRUE)
  datList = lapply(csvFiles, function(file){
    return(SensorData.importCsv(filename = file))
  })
}


#' @name SensorData.importActigraphCsv
#' @title Import and convert actigraph raw csv files and load into data frame as in mhealth format
#' @export
#' @note Please make sure the actigraph raw csv file has timestamp included
SensorData.importActigraphCsv = function(filename){
  actigraphHeader = .SensorData.parseActigraphCsvHeader(filename)
  dat = read.table(filename, header = FALSE, sep= ",", strip.white = TRUE, skip = 11, stringsAsFactors = FALSE);
  dat = dat[,1:4]
  names(dat) = c(MHEALTH_CSV_TIMESTAMP_HEADER,
                 MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER,
                 MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER,
                 MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER)
  timeFormat = ifelse(test = actigraphHeader$imu,
                      yes = ACTIGRAPH_IMU_TIMESTAMP,
                      no = ACTIGRAPH_TIMESTAMP)
  dat[[MHEALTH_CSV_TIMESTAMP_HEADER]] = strptime(x = dat[[MHEALTH_CSV_TIMESTAMP_HEADER]],
                                                 format = timeFormat) + 0.0005
  options(digits.secs = 3);
  return(dat)
}

#' @import stringr
.SensorData.parseActigraphCsvHeader = function(filename){
  headlines = readLines(filename, n = 10, encoding="UTF-8");

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
  if(str_detect(headlines[[1]], "IMU")){
    imu = TRUE
  }else{
    imu = FALSE
  }

  # Session start time
  st = headlines[[3]]
  sd = headlines[[4]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+";
  dateReg = "[0-9]+/[0-9]+/[0-9]{4}";
  st = regmatches(st, regexpr(timeReg, st, perl=TRUE))
  sd = regmatches(sd, regexpr(dateReg, sd, perl=TRUE))
  st = paste(sd, st, sep=' ')
  timeFormat = ACTIGRAPH_TIMESTAMP
  st = strptime(st, timeFormat) + 0.0005
  options(digits.secs = 3);

  # Session download time
  dt = headlines[[6]]
  dd = headlines[[7]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+";
  dateReg = "[0-9]{2}/[0-9]{2}/[0-9]{4}";
  dt = regmatches(dt, regexpr(timeReg, dt, perl=TRUE))
  dd = regmatches(dd, regexpr(dateReg, dd, perl=TRUE))
  dt = paste(dd, dt, sep=' ')
  timeFormat = ACTIGRAPH_TIMESTAMP
  dt = strptime(dt, timeFormat) + 0.0005
  options(digits.secs = 3);

  # header object as output
  header = {}
  header$sr = sr
  header$fw = fw
  header$sw = sw
  header$sn = sn
  header$st = st
  header$dt = dt
  header$at = at
  header$imu = imu

  return(header)
}
