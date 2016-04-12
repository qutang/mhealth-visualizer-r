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
epochs = c(5, 10)

ontologyFile = normalizePath(list.files(path = folder, pattern = ".*ontology.*", full.names = TRUE))
ontologyData = read.csv(ontologyFile, header= TRUE, as.is = TRUE)

for(subj in subjects){
  
  print(paste("Summarize", subj))
  
  sensorFiles = normalizePath(list.files(path = file.path(folder, subj, ready_folder), pattern = "TAS.*sensor.csv.*", full.names = TRUE))
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
    for(epoch in epochs){
      actigraphCountFile = list.files(path = file.path(folder, subj, actigraph_folder), full.names = TRUE, pattern = paste0(id, ".*actigraph.*", epoch, "sec"))
      actigraphCountFile = actigraphCountFile[[1]]
      actigraphCountData = SensorData.importActigraphCountCsv(actigraphCountFile, 3, "ACTIGRAPH_COUNT")
      dataChunk$actigraphCountData[[epoch]] = actigraphCountData
    }
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
  summary_pages = llply(listOfData, function(dataChunk){
    summary_single_page = llply(epochs, function(epoch){
      summaryData = SensorData.summary.counts.compute(dataChunk$sensorData, breaks = paste(epoch, "sec"))
      
      dir.create(file.path(folder, subj, summary_folder))
      filename = paste(dataChunk$sensorId, dataChunk$sensorLocation, "_counts.csv")
      write.table(x = summaryData, file = paste0(file.path(folder, subj, summary_folder), "/", filename), sep = ",", na = "NA", col.names = TRUE, row.names = FALSE, quote = FALSE)
      
      summaryPlot = SummaryData.ggplot(summaryData, plotType = "step")
      summaryPlot = AnnotationData.addToGgplot(summaryPlot, primaryAnnotations)
      summaryPlot = summaryPlot + ggtitle(paste("Epoch", epoch, "seconds", dataChunk$sensorId, "at", dataChunk$sensorLocation))
      summaryPlot = summaryPlot + theme(text = element_text(size=40), legend.position="none")
      
      summary_page = summaryPlot
      
      if(!is.null(dataChunk$actigraphCountData[[epoch]])){
        actigraphCountData = dataChunk$actigraphCountData[[epoch]]
        actigraphCountData[,2] = actigraphCountData[,2]/200;
        combined_nrow = min(nrow(actigraphCountData), nrow(summaryData))
        combinedSummaryData = merge(summaryData[1:combined_nrow,], actigraphCountData[1:combined_nrow,])
        summaryPlot2 = SummaryData.ggplot(combinedSummaryData, plotType = "step")
        summaryPlot2 = AnnotationData.addToGgplot(summaryPlot2, primaryAnnotations)
        summaryPlot2 = summaryPlot2 + ggtitle(paste("Epoch", epoch, " seconds", dataChunk$sensorId, "at", dataChunk$sensorLocation))
        summaryPlot2 = summaryPlot2 + theme(text = element_text(size=40))
        summary_page = summaryPlot2
      }
    })
    summary_single_page = arrangeGrob(grobs = summary_single_page, nrow = 2, ncol = 1)
    return(summary_single_page)
  }, .parallel = TRUE)
  sr_page = arrangeGrob(grobs = listOfSrPlots) 
  
  # export to pdf
  pages_list = c(list(sr_page, annotation_page), summary_pages)
  pages = marrangeGrob(grobs = pages_list, ncol = 1, nrow = 1)
  dir.create(file.path(folder, subj, summary_folder))
  ggsave("summary_report.pdf", pages, path = file.path(folder, subj, summary_folder), scale = 7, width = 7, height = 3.5, dpi = 300)
}

