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

SensorData.merge = function(csvList, ...){
  otherList = list(...)

}
