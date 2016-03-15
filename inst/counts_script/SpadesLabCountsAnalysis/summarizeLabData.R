require(mhealthformatsupportr)
require(foreach)
require(ggplot2)
require(plyr)
require(stringr)
require(gridExtra)

folder = "../../SPADES_labdata_used/"
ready_folder = "ready"
summary_folder = "summary"
subjects = paste("SPADES", seq(1,1), sep = "_")

ontologyFile = normalizePath(list.files(path = folder, pattern = ".*ontology.*", full.names = TRUE))
ontologyData = read.csv(ontologyFile, header= TRUE, as.is = TRUE)

for(subj in subjects){
  
  print(paste("Summarize", subj))
  
  sensorFiles = normalizePath(list.files(path = file.path(folder, subj, ready_folder), pattern = ".*sensor.csv.*", full.names = TRUE))
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
    summaryData = Magnitude.compute(SummaryData.absoluteMean(dataChunk$sensorData, breaks = "5 sec"))
    summaryPlot = SummaryData.ggplot(summaryData)
    summaryPlot = AnnotationData.addToGgplot(summaryPlot, primaryAnnotations)
    summaryPlot = summaryPlot + ggtitle(paste(dataChunk$sensorId, "at", dataChunk$sensorLocation))
    summaryPlot = summaryPlot + theme(text = element_text(size=40), legend.position="none")
    return(list(summaryPlot))
  }
  sr_page = arrangeGrob(grobs = listOfSrPlots) 
  
  # export to pdf
  pages_list = c(list(sr_page, annotation_page), listOfPrimarySummaryPlots)
  pages = marrangeGrob(grobs = pages_list, ncol = 1, nrow = 1)
  dir.create(file.path(folder, subj, summary_folder))
  ggsave("summary_report.pdf", pages, path = file.path(folder, subj, summary_folder), scale = 7, width = 7, height = 3.5, dpi = 300)
}

