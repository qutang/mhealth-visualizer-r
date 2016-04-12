rm(list = ls(all = TRUE))
require(mhealthformatsupportr)
require(foreach)
require(plyr)
require(stringr)

folder = "../../SPADES_labdata_used/"
ready_folder = "ready"
actigraph_folder = "actigraph_ready/csv"
summary_folder = "summary"
plot_folder = "plot"
subjects = paste("SPADES", 1, sep = "_")
epochs = c(10)

mets = read.csv(file.path(folder, "mets.SpadesInLab.csv"), as.is = TRUE)
mets[,1] = tolower(mets[,1])
mets = with(mets, mets[order(METS),])

for(subj in subjects){
  
  print(paste("Prepare", subj, "by activities"))
  
  sensorFiles = normalizePath(list.files(path = file.path(folder, subj, ready_folder), pattern = "TAS.*sensor.csv.*", full.names = TRUE))
  annotationFile = normalizePath(list.files(path = file.path(folder, subj, ready_folder), pattern = "primary.*annotation.csv.*", full.names = TRUE))
  
  # summarize annotations
  annotationData = AnnotationData.importCsv(annotationFile)
  
  # summarize sensors
  listOfData = lapply(sensorFiles, function(sensorFile){
    sensorId = str_split(basename(sensorFile), "_")[[1]][1]
    sensorLocation = str_split(basename(sensorFile), "_")[[1]][2]
    
    print(paste("Read in", sensorId, "at", sensorLocation))
    
    sensorData = SensorData.importCsv(sensorFile)
    ourCount = list()
    simulatedActigraphCount = list()
    for(epoch in epochs){
      ourCount[[epoch]] = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "sec"), filterType = "butter", cutoffs = c(0.6, 10))
      simulatedActigraphCount[[epoch]] = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "sec"), filterType = "butter", cutoffs = c(0.25, 5))
    }
    
    return(list(sensorId = sensorId, sensorLocation = sensorLocation, sensorData = sensorData, 
                ourCount = ourCount, simulatedActigraphCount = simulatedActigraphCount))
  })
  
  # Also add actigraph count value
  listOfData = llply(listOfData, function(dataChunk){
    id = dataChunk$sensorId
    for(epoch in epochs){
      actigraphCountFile = list.files(path = file.path(folder, subj, actigraph_folder), full.names = TRUE, pattern = paste0(id, ".*actigraph.*", epoch, "sec"))
      actigraphCountFile = actigraphCountFile[[1]]
      actigraphCountData = SensorData.importActigraphCountCsv(actigraphCountFile, 3, "ACTIGRAPH_COUNT")
      dataChunk$actigraphCountData[[epoch]] = actigraphCountData
    }
    return(dataChunk)
  })
}