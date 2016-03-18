require(mhealthformatsupportr)
require(foreach)
require(ggplot2)
require(plyr)
require(stringr)
require(gridExtra)

folder = "../../SPADES_labdata_used/"
ready_folder = "ready"
actigraph_folder = "actigraph_ready/csv"
summary_folder = "summary"
subjects = paste("SPADES", seq(1,2), sep = "_")

ontologyFile = normalizePath(list.files(path = folder, pattern = ".*ontology.*", full.names = TRUE))
ontologyData = read.csv(ontologyFile, header= TRUE, as.is = TRUE)

for(subj in subjects){
  
  print(paste("Summarize", subj))
  
  sensorFiles = normalizePath(list.files(path = file.path(folder, subj, ready_folder), pattern = "TAS.*sensor.csv.*", full.names = TRUE))[1]
  annotationFile = normalizePath(list.files(path = file.path(folder, subj, ready_folder), pattern = ".*annotation.csv.*", full.names = TRUE))[1]
  
  # summarize annotations
  annotationData = AnnotationData.importCsv(annotationFile)
  primaryAnnotations = AnnotationData.filter(annotationData, ontologyData, categories = c("Primary"), include.categories = TRUE)
  annotation_page = AnnotationData.addToGgplot(NULL, annotationData = annotationData)
  annotation_page = annotation_page + theme(legend.text = element_text(size=25, face="bold"), legend.key.size = unit(1.5, "cm"), text = element_text(size=50))
  
  # summarize sensors
  listOfData = lapply(sensorFiles, function(sensorFile){
    sensorId = str_split(basename(sensorFile), "_")[[1]][1]
    sensorLocation = str_split(basename(sensorFile), "_")[[1]][2]
    
    print(paste("Read in", sensorId, "at", sensorLocation))
    
    sensorData = SensorData.importCsv(sensorFile)
    
    return(list(sensorId = sensorId, sensorLocation = sensorLocation, sensorData = sensorData))
  })
  
  # Also add actigraph count value
  listOfData = llply(listOfData, function(dataChunk){
    id = dataChunk$sensorId
    actigraphCountFile = list.files(path = file.path(folder, subj, actigraph_folder), full.names = TRUE, pattern = paste0(id, ".*actigraph"))
    actigraphCountFile = actigraphCountFile[[1]]
    actigraphCountData = SensorData.importActigraphCountCsv(actigraphCountFile)
    dataChunk$actigraphCountData = actigraphCountData
    return(dataChunk)
  })
  
  # Sampling rate
  print("Analyze sampling rate for sensors")
  listOfSrPlots = foreach(dataChunk = listOfData, .combine = c) %dopar% {
    srData = SamplingRate.summary(dataChunk$sensorData, breaks = "min")
    srPlot = SamplingRate.ggplot(srData, unit = "Hz")
    srPlot = srPlot + ggtitle(paste(dataChunk$sensorId, "at", dataChunk$sensorLocation))
    return(list(srPlot))
  }
  sr_page = arrangeGrob(grobs = listOfSrPlots)
  
  # TODO: use actual counts algorithm to generate the Counts plot
  print("Analyze summary data for sensors")
  listOfPrimarySummaryPlots = foreach(dataChunk = listOfData, .combine = c) %dopar% {
    summaryData = SensorData.summary.counts.compute(dataChunk$sensorData, breaks = "5 sec")
    
    dir.create(file.path(folder, subj, summary_folder))
    filename = paste(dataChunk$sensorId, dataChunk$sensorLocation, "_counts.csv")
    write.table(x = summaryData, file = paste0(file.path(folder, subj, summary_folder), "/", filename), sep = ",", na = "NA", col.names = TRUE, row.names = FALSE, quote = FALSE)
    
    summaryPlot = SummaryData.ggplot(summaryData, plotType = "step")
    summaryPlot = AnnotationData.addToGgplot(summaryPlot, primaryAnnotations)
    summaryPlot = summaryPlot + ggtitle(paste(dataChunk$sensorId, "at", dataChunk$sensorLocation))
    summaryPlot = summaryPlot + theme(text = element_text(size=40), legend.position="none")
    
    result_plot = list(summaryPlot)
    
    if(exists(dataChunk$actigraphCountData)){
      actigraphCountData = dataChunk$actigraphCountData
      actigraphCountData = cbind(summaryData[[1]], actigraphCountData)
      summaryData2 = Magnitude.compute(actigraphCountData)
      summaryPlot2 = SummaryData.ggplot(summaryData2, plotType = "step")
      summaryPlot2 = AnnotationData.addToGgplot(summaryPlot2, primaryAnnotations)
      summaryPlot2 = summaryPlot2 + ggtitle(paste("Actigraph Count", dataChunk$sensorId, "at", dataChunk$sensorLocation))
      summaryPlot2 = summaryPlot2 + theme(text = element_text(size=40), legend.position="none")
      result_plot = arrangeGrob(grobs = list(summaryPlot, summaryPlot2)) 
    }
    
    return(list(result_plot))
  }
  sr_page = arrangeGrob(grobs = listOfSrPlots) 
  
  # export to pdf
  pages_list = c(list(sr_page, annotation_page), listOfPrimarySummaryPlots)
  pages = marrangeGrob(grobs = pages_list, ncol = 1, nrow = 1)
  dir.create(file.path(folder, subj, summary_folder))
  ggsave("summary_report.pdf", pages, path = file.path(folder, subj, summary_folder), scale = 7, width = 7, height = 3.5, dpi = 300)
}

