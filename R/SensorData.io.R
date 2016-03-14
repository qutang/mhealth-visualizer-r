#' @name SensorData.io.write
#' @title Write sensor data into mhealth folder structure and with mhealth filename convention.
#' @export
#' @import lubridate stringr
#' @param folder the output folder
#' @param sensorData the input dataframe that matches mhealth specification.
#' @param sensorType the sensor type string used in filename.
#' @param dataType the data type string used in filename.
#' @param sensorId the sensor ID string used in filename.
#' @param versionCode the version code string used in filename; default is "NA".
#' @param tz the time zone string (P/MHHMM) used in filename.
#' @param gzip whether to gzip the output csv file.
#' @param flatDir whether to use mhealth folder structure or just use flat directory.
#' @param splitHour whether to split input dataframe into hourly csv files.
#' @param custom_name if provided, the file name will be custom_name and all other file name preset parameters such as sensorType will be discarded.
#' @param append whether to append to a file if the file exists
#' @param header whether to add column header or not
SensorData.io.write = function(folder, sensorData, sensorType = NA, dataType = NA, sensorId = NA, versionCode = "NA", tz, gzip = TRUE, flatDir = FALSE, splitHour = TRUE, custom_name, append = FALSE, header = TRUE){
  if(missing(custom_name)){
    # TODO: support split hour
    if(missing(tz)){
      warning("Use local time zone in the file name")
      startTime = sensorData[1,1]
      utcTime = ymd_hms(startTime)
      localTime = startTime
      hourDiff = round(utcTime - localTime, digits = 2)
      tzStr = .SensorData.io.formatTimezone(hourDiff)
    }else{
      tzStr = tz
    }
    timeStamp = strftime(startTime, format = MHEALTH_FILE_TIMESTAMP_FORMAT, origin = origin)
    timeStamp = str_replace(timeStamp, pattern = "\\.", replacement = "-")
    timeStampStr = paste(timeStamp, tzStr, sep="-")
    section1 = paste(sensorType, dataType, versionCode, sep="-")
    section2 = paste(sensorId, dataType,sep="-")
    sensorFilename = paste(section1, section2, timeStampStr, "sensor", "csv", sep = ".")
  }else{
    sensorFilename = custom_name;
  }

  sensorData[,-1] = round(sensorData[,-1], digits = 4)
  if(!flatDir){
    containFolder = file.path(folder, year(startTime), month(startTime), day(startTime), hour(startTime))
  }else{
    containFolder = folder
  }
  dir.create(containFolder, showWarnings = FALSE, recursive = TRUE)
  fullPath = file.path(containFolder, sensorFilename)
  options(digits.secs = 3)
  if(gzip){
    gzFile = gzfile(description = paste(fullPath, "gz", sep = "."), open = "w", compression = 6, encoding = "UTF-8")
    write.table(x = sensorData, file = gzFile, append = append, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = header)
    close(gzFile)
  }else{
    write.table(x = sensorData, file = fullPath, append = append, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = header)
  }
}

#' @name SensorData.io.writeAsActigraphRaw
#' @title write sensor data frame into actigraph raw
#' @export
SensorData.io.writeAsActigraphRaw = function(folder, sensorData, headerStr, custom_name){
  filepath = file.path(folder, custom_name)
  file.remove(filepath)
  writeLines(text = headerStr, con = filepath, sep = "\n", useBytes = FALSE)
  sensorData[,-1] = round(sensorData[,-1], digits = 4)
  sensorData[,1] = format(sensorData[,1], ACTIGRAPH_TIMESTAMP)
  write.table(x = sensorData, file = filepath, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
}

.SensorData.io.formatTimezone = function(hourDiff){
    if(hourDiff < 0){
      prefix = "M"
    }else{
      prefix = "P"
    }
    hourStr = as.character(floor(abs(hourDiff)))
    if(hourDiff < 10){
      hourStr = paste0("0", hourStr)
    }
    minDiff = abs(hourDiff) - floor(abs(hourDiff))
    if(minDiff == 0){
      result = paste0(prefix, hourStr, "00")
    }else if(minDiff == 0.5){
      result = paste0(prefix, hourStr, "30")
    }else if(minDiff == 0.25){
      result = paste0(prefix, hourStr, "15")
    }else if(minDiff == 0.75){
      result = paste0(prefix, hourStr, "45")
    }else{
      result = paste0(prefix, hourStr, "00")
    }
    return(result)
}

#' @import stringr
.SensorData.io.parseTimezone = function(tzStr){
    tokens = str_match(tzStr, "([0-9]{2})(([0-9]{2}))")
    hourInt = as.numeric(tokens[1,2])
    hourFrag = as.numeric(tokens[1,3])/60
    hourDiff = hourInt + hourFrag
    if(str_detect(tzStr, "M")){
      hourDiff = -hourDiff
    }
    return(hourDiff)
}
