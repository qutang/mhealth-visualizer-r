#Sampling rate test, convert raw into ready data (from Actigraph csv to mhealth)

require(stringr)

hzs = seq(30, 100, by = 10)
for (hz in hzs){
  files = list.files(path = paste0("../../MATLAB/counts/sampling__rate_test/raw/", hz," HZ/csv/"), full.names = TRUE)
  files = files[!str_detect(files, "sessions|IMU")]
  
  session_file = paste0("../../MATLAB/counts/sampling__rate_test/raw/", hz, " HZ/csv/sessions.csv")
  
  for(file in files){
    data = SensorData.importActigraphCsv(file)
    # SensorData.ggplot(data)
    
    sessions = read.csv(session_file, header = TRUE, stringsAsFactors = FALSE)
    sessions[,1] = as.POSIXct(sessions[,1])
    sessions[,2] = as.POSIXct(sessions[,2])
    
    id = str_match(basename(file), ".+RAW")
    
    for(i in seq(1, nrow(sessions))){
      clipped = SensorData.clip(data, startTime = sessions[i, 1] + 30, sessions[i,2]-30)
      SensorData.io.write(paste0('../../MATLAB/counts/sampling__rate_test/data/', id), sensorData = clipped, sensorType = paste0(hz, "Hz"), dataType = sessions[i, 3], sensorId = id, versionCode = NA, gzip = FALSE, flatDir = TRUE, splitHour = FALSE)
    }
  }
}

