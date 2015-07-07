MHEALTH_CSV_TIMESTAMP_HEADER = "HEADER_TIME_STAMP"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER = "X_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER = "Y_ACCELATION_METERS_PER_SECOND_SQUARED"
MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER = "Z_ACCELATION_METERS_PER_SECOND_SQUARED"

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

#' @export
SensorData.importBin = function(filename) {
  op <- options(digits.secs = 3)
  con <- file(filename, "rb")
  size = file.size(filename)
  count = 1
  N = floor(size / 20)
  rawx = double(N)
  rawy = double(N)
  rawz = double(N)
  timestamp = integer(N)
  while (count <= N) {
    rawx[count] = readBin(con = con, "numeric", 4)
    rawy[count] = readBin(con = con, "numeric", 4)
    rawz[count] = readBin(con = con, "numeric", 4)
    timestamp[count] = readBin(con = con, "double", 8)
    count = count + 1;
  }
  timestamp = as.POSIXct(timestamp / 1000, origin = origin)
  dat = data.frame(
    MHEALTH_CSV_TIMESTAMP_HEADER = timestamp,
    MHEALTH_CSV_ACCELEROMETER_CALIBRATED_X_HEADER = rawx,
    MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Y_HEADER = rawy,
    MHEALTH_CSV_ACCELEROMETER_CALIBRATED_Z_HEADER = rawz
  )
  return(dat)
}
