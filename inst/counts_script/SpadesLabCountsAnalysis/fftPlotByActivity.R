require(mhealthformatsupportr)
require(foreach)
require(plyr)
require(stringr)
require(ggplot2)
require(gridExtra)
require(ggthemes)

# source("inst/counts_script/SpadesLabCountsAnalysis/prepareByActivity.R")

folder = "../../SPADES_labdata_used/"
ready_folder = "ready"
actigraph_folder = "actigraph_ready/csv"
summary_folder = "summary"
plot_folder = "plot"
subjects = paste("SPADES", 1, sep = "_")

# load mets values
mets = read.csv(file.path(folder, "mets.SpadesInLab.csv"), as.is = TRUE)
mets[,2] = as.numeric(mets[,2])
mets[,1] = tolower(mets[,1])
mets = with(mets, mets[order(METS),])

for(subj in subjects){
  labels = mets[,1]
  
  listOfData = llply(listOfData, function(dataChunk){
    location = dataChunk$sensorLocation
    id = dataChunk$sensorId
    sensorData = dataChunk$sensorData
     fftDataByActivity = llply(labels, function(label){
       i = which(labels == label)
      clips = annotationData[annotationData[,4] == label,c(2,3)]
      if(length(clips) > 0){
        fftByActivity = adply(clips, 1, function(clip){
          clippedData = SensorData.clip(sensorData, clip[1], clip[2])
          fftData = FrequencyResponse.fft(clippedData, Fs = SensorData.getSamplingRate(sensorData), type = "magnitude", normalize = "normalized")
          fftData = Magnitude.compute(fftData)
          return(fftData)
        })
      }else{
        return(NULL)
      }
      
      result = fftByActivity[,c(-1,-2)]
      # sort by frequency
      result = result[sort.list(result[,1]),]
      return(list(data = result, label = label, offset = max(result[result[,1]>0.05,2]), index = i))
    })
    maxOffset = Reduce(function(x,y){
      if(x[["offset"]]>y[["offset"]])
        return(x)
      else
        return(y)
      }, fftDataByActivity)
    maxOffset = maxOffset[["offset"]]
    maxOffset = ceiling(maxOffset * 10^8)/10^8
    filename = paste(dataChunk$sensorId, dataChunk$sensorLocation, "fftByActivities.csv", sep = "_")
    p = ggplot()
    for(fftData in fftDataByActivity){
      col_names = colnames(fftData$data)
      scaledData = fftData$data[fftData$data[,1]>0.05,]
      scaledData[,2] = scaledData[,2]/max(scaledData[,2])*maxOffset + (fftData$index - 1) * maxOffset
      fftData$data[,2] = fftData$data[,2] + (fftData$index - 1) * maxOffset
      fftData$data = fftData$data[fftData$data[,1]>0.05,]
      
      p = p + geom_line(data = fftData$data, aes_string(x = col_names[1], y = col_names[2])) + 
          geom_line(data = scaledData, aes_string(x = col_names[1], y = col_names[2]), color = "gray", alpha = 0.5)
    }

    breaks= seq(from = 0, to = (length(fftDataByActivity) -1) * maxOffset, by = maxOffset)
    x_breaks = c(seq(from = 0, to = 1, by = 0.1), seq(from = 1, to = 5, by = 0.5), 10, 0.25, 2.5)
    p = p + scale_y_continuous(breaks = breaks, labels = labels, name = element_blank())
    p = p + scale_x_log10(breaks = x_breaks, labels = x_breaks, name = "Hz", limits = c(0.1, 40))
    p = p + geom_vline(xintercept = c(0.25, 0.6, 2.5, 5, 10), linetype = "dashed")
    p = p + theme_bw()
    p = p + ggtitle(paste(id, location, sep = "_"))

      
    fftplot_filename = paste(id, location, "fftByActivities.png", sep = "_")
    fftplot_filepath = normalizePath(file.path(folder, subj, plot_folder, "byActivities"))
    dir.create(fftplot_filepath, recursive = TRUE)
    ggsave(filename = fftplot_filename, path = fftplot_filepath, plot = p, width = 5, height = 4, scale = 2)
    dataChunk$fftByActivityPlot = p
    return(dataChunk)
  })
  
  plotList = llply(listOfData, function(dataChunk){
    return(dataChunk$fftByActivityPlot)
  })
  plots = arrangeGrob(grobs = plotList, ncol = 2, nrow = 4)
  mplots = grid.arrange(plots)
  fftplot_filepath = normalizePath(file.path(folder, subj, plot_folder, "byActivities"))
  ggsave(filename = "fftByActivities.png", path = fftplot_filepath, plot = mplots, width = 8, height = 5, scale = 3)
}

