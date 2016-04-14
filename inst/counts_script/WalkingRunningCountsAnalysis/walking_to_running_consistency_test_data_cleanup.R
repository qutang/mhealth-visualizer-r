# Walking to Running Consistency test data clean up
# Clip into 30 seconds segments
# Save as mhealth and actigraph csv file
#
require(stringr);
require(ggplot2);
require(mhealthformatsupportr);

folder = "../../MATLAB/counts/consistency_test/data/";
subjects = list.dirs(path = folder, full.names = FALSE, recursive = FALSE)
ready_folder = "ready"
actigraphcsv_folder = "actigraphcsv"

for(subj in subjects[2]){
  ready_path = file.path(folder, subj, ready_folder);
  actigraphcsv_path = file.path(folder, subj, actigraphcsv_folder)
  
  files = list.files(path = file.path(folder, subj), pattern = "*RAW.csv", full.names = TRUE);
  segmentsFile = file.path(folder, subj, "segments.csv")
  locationsFile = file.path(folder, subj, "locations.csv")
  
  # read in segments
  segments = read.csv(file = segmentsFile, header = TRUE, as.is = TRUE)
  
  sts = as.POSIXct(segments[,1])
  ets = as.POSIXct(segments[,2])
  mphs = segments[,3]
  weights = segments[,4]
  
  # read in locations
  locations = read.csv(file = locationsFile, header = TRUE, as.is = TRUE)
  
  for(file in files){
    d = SensorData.importActigraphCsv(file);
    headerInfo = SensorData.parseActigraphCsvHeader(file);
    sr = headerInfo$sr
    id = headerInfo$sn
    location = locations[locations[,1] == id,2]
    for(i in seq(1, length(sts))){
      clipped = SensorData.clip(d, sts[i], ets[i]);
      p = SensorData.ggplot(clipped)
      filename = paste(location, sr, paste0("mph", mphs[i]), weights[i], sep = "_")
      ggsave(paste(filename, "png", sep="."), plot = p, path = ready_path, width = 11, height = 8)
      SensorData.io.write(ready_path, sensorData = clipped, gzip = FALSE, flatDir = TRUE, splitHour = FALSE, custom_name = paste(filename, "csv", sep="."))
      headerStr = SensorData.createActigraphCsvHeader(headerInfo$st, headerInfo$et, headerInfo$sr, headerInfo$sn, headerInfo$fw, headerInfo$sw)
      SensorData.io.writeAsActigraphRaw(actigraphcsv_path, clipped, headerStr, custom_name = paste(filename, "csv", sep="."))
      print(paste("Processed", filename))
    }
  }
}

