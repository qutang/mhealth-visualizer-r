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
epochs = c(2, 5, 10)
integr = c("trapz", "power")

mets = read.csv(file.path(folder, "mets.SpadesInLab.csv"), as.is = TRUE)
mets[,1] = tolower(mets[,1])
mets = with(mets, mets[order(METS),])

listOfSubjects = list()

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
    
    # ONLY read in waist data
#     if(!str_detect(sensorLocation,"wrist")){
#       return(list(sensorId = sensorId, sensorLocation = sensorLocation, sensorData = NA, 
#                   count1 = list(), count2 = list(), count3 = list(), count4 = list()))
#     }
    
    sensorData = SensorData.importCsv(sensorFile)
    count1 = list() # 0.6 ~ 10
    count2 = list() # 0.25 ~ 5
    count3 = list() # 0.25 ~ 2.5
    count4 = list() # 0.25 ~ 10
    for(epoch in epochs){
      count1[[epoch]] = list()
      count2[[epoch]] = list()
      count3[[epoch]] = list()
      count4[[epoch]] = list()
      for(intType in integr){
        count1[[epoch]][[intType]] = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "sec"), filterType = "butter", cutoffs = c(0.6, 10), integrationType = intType)
        count2[[epoch]][[intType]] = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "sec"), filterType = "butter", cutoffs = c(0.25, 5), integrationType = intType)
        count3[[epoch]][[intType]] = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "sec"), filterType = "butter", cutoffs = c(0.25, 2.5), integrationType = intType)
        count4[[epoch]][[intType]] = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "sec"), filterType = "butter", cutoffs = c(0.25, 10), integrationType = intType)
      }
    }
    
    
    
    return(list(sensorId = sensorId, sensorLocation = sensorLocation, sensorData = sensorData, 
                count1 = count1, count2 = count2, count3 = count3, count4 = count4))
  })
  
  # Also add actigraph count value
  listOfData = llply(listOfData, function(dataChunk){
    id = dataChunk$sensorId
    dataChunk$actigraphCountData = list()
    for(epoch in epochs){
      actigraphCountFile = list.files(path = file.path(folder, subj, actigraph_folder), full.names = TRUE, pattern = paste0(id, ".*actigraph.*", epoch, "sec"))
      if(length(actigraphCountFile) == 0){
        warning(paste("Actigraph count is not availabel for epoch:", epoch))
        dataChunk$actigraphCountData[[epoch]] = NA
      }else{
        actigraphCountFile = actigraphCountFile[[1]]
        actigraphCountData = SensorData.importActigraphCountCsv(actigraphCountFile, 3, "ACTIGRAPH_COUNT")
        dataChunk$actigraphCountData[[epoch]] = actigraphCountData
      }
    }
    return(dataChunk)
  })
  
  listOfSubjects[[subj]] = list(data = listOfData, annotation = annotationData)
}