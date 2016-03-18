# Prepare SPADES lab data for counts analysis
# 
# 1. Clean up and merge raw data files
# 2. Clean up and merge annotation files
# 3. Map locations to different data files
require(mhealthformatsupportr)
require(foreach)
require(ggplot2)
require(plyr)

folder = "../../SPADES_labdata_used/"
ready_folder = "ready"
actigraph_folder = "actigraph_ready"
summary_folder = "summary"
subjects = paste("SPADES", seq(1,2), sep = "_")
sensorLocationFile = "Sensor_location_Lab.csv";
sessionsFile = "Sessions.csv";
sessionDateFormat = "%m/%d/%Y %H:%M";

for(subj in subjects){
  
  # Read in location and sensor ID mapping file
  
  sensorLocationMapping = read.csv(file = file.path(folder, subj, sensorLocationFile), header = TRUE, as.is = TRUE)
  print(paste("Prepare for", subj))
  
  # Read in sessions.csv to get valid time period information
  sessions = read.csv(file = file.path(folder, subj, sessionsFile), header = TRUE, as.is = TRUE)
  
  startTime = strptime(sessions[2,3], sessionDateFormat)
  endTime = strptime(sessions[2,4], sessionDateFormat)
  
  for(i in seq(1, nrow(sensorLocationMapping))){
    
    id = sensorLocationMapping[i, 1]
    location = sensorLocationMapping[i, 2]
    print(paste("Prepare", id, "at", location))
    dataFiles = normalizePath(
                      list.files(
                           path = file.path(folder, subj, "MasterSynced"), 
                           full.names = TRUE, 
                           recursive = TRUE, 
                           include.dirs = FALSE, 
                           no.. = TRUE,
                           pattern = paste0(id, ".*sensor.csv.*")))
    
    
    # Read in and clean up data file for each hour
    listOfData = foreach(dataFile = dataFiles, .combine = c) %dopar% {
      hourlyData = SensorData.importCsv(dataFile)
      hourlyData = SensorData.cleanup(hourlyData, level = "day", ref = format(startTime, "%Y-%m-%d"))
      return(list(hourlyData))
    }
    
    # merge data files
    
    mergedData = SensorData.merge(listOfData)
    
    # save merged data to actigraph format csv
    dir.create(file.path(folder, subj, actigraph_folder))
    headStr = SensorData.createActigraphCsvHeader(startTime = mergedData[1,1], 
                                        downloadTime = mergedData[nrow(mergedData),1],
                                        samplingRate = round(1/as.numeric(mergedData[2,1] - mergedData[1,1])),
                                        sensorId = id, 
                                        firmVersion = "1.5.0", 
                                        softVersion = "9.23.0")
    SensorData.io.writeAsActigraphRaw(file.path(folder, subj, actigraph_folder),sensorData = mergedData, headerStr = headStr, custom_name = paste(id, location, "merged.actigraph.csv", sep = "_"))
    
    # save merged data to ready folder
    dir.create(file.path(folder, subj, ready_folder))
    SensorData.io.write(file.path(folder, subj, ready_folder),
                        sensorData = mergedData, 
                        append = FALSE, 
                        header = TRUE, 
                        custom_name = paste(id, location, "merged.sensor.csv", sep = "_"), 
                        gzip = TRUE, 
                        flatDir = TRUE, 
                        splitHour = FALSE)
    
   
  }
  
  # Annotation file
  annotationFiles = normalizePath(list.files(
    path = file.path(folder, subj, "MasterSynced"),
    full.names = TRUE, 
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE,
    pattern = ".*annotation.csv.*"
  ))
  
  
  # Read in and categorize annotation file for each hour
  ontologyId = AnnotationData.io.getOntologyId(basename(annotationFiles[1]))
  annotatorId = AnnotationData.io.getAnnotatorId(basename(annotationFiles[1]))
  print(paste("Prepare", ontologyId, "by", annotatorId))
  listOfAnnotations = foreach(annFile = annotationFiles, .combine = c) %dopar% {
    hourlyAnnotation = AnnotationData.importCsv(annFile)
    return(list(hourlyAnnotation))
  }
  
  # merge annotation files
  mergedAnnotation = AnnotationData.merge(listOfAnnotations)
  
  
  # save merged annotation to ready folder
  AnnotationData.io.write(file.path(folder, subj, ready_folder),
                          annotationData = mergedAnnotation, 
                          append = FALSE, 
                          header = TRUE, 
                          custom_name = paste(ontologyId, annotatorId, "merged.annotation.csv", sep = "_"), 
                          gzip = TRUE, 
                          flatDir = TRUE, 
                          splitHour = FALSE)
}


