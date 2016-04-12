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
epochs = c(10)

# load mets values
mets = read.csv(file.path(folder, "mets.SpadesInLab.csv"), as.is = TRUE)
mets[,2] = as.numeric(mets[,2])
mets[,1] = tolower(mets[,1])
mets = with(mets, mets[order(METS),])

for(subj in subjects){
  
  labels = tolower(unique(annotationData[,4]))
  
  # load subject info
  subject_info = read.csv(file.path(folder, subj, "Subject.csv"), as.is = TRUE)
  weight = as.numeric(subject_info[,"START_WEIGHT_KILOGRAMS"])
  height = as.numeric(subject_info[,"START_HEIGHT_METERS"]) * 100
  age = as.numeric(subject_info[,"START_AGE"])
  
  # compute corrected METS value
  rmr = 66.4730 + 5.0033 *height + 13.7516 * weight - 6.7550 * age
  rmr = rmr / 1440 / 5 / weight * 1000
  corrected_mets = mets[,2] * 3.5/rmr
  mets[,"CORRECTED_METS"] = corrected_mets
  
  listOfData = llply(listOfData, function(dataChunk){
    location = dataChunk$sensorLocation
    id = dataChunk$sensorId
    ourCount = dataChunk$ourCount
    simulatedActigraphCount = dataChunk$simulatedActigraphCount
    actigraphCount = dataChunk$actigraphCountData
    for(epoch in epochs){
      countByActivities = ldply(labels, function(label){
        clips = annotationData[annotationData[,4] == label,c(2,3)]
        
        countByActivity = adply(clips, 1, function(clip){
          countValue = SensorData.clip(ourCount[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          return(countValue)
        })
        
        simulatedActigraphCountByActivity = adply(clips, 1, function(clip){
          countValue = SensorData.clip(simulatedActigraphCount[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          return(countValue)
        })
        
        actigraphCountByActivity = adply(clips, 1, function(clip){
          countValue = SensorData.clip(actigraphCount[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          return(countValue)
        })

        meanValue = mean(countByActivity[,4]) * 200
        stdValue = sd(countByActivity[,4]) * 200
        boxValue = boxplot.stats(countByActivity[,4])
        boxValue$stats = boxValue$stats * 200
        
        sim_acti_meanValue = mean(simulatedActigraphCountByActivity[,4]) * 200
        sim_acti_stdValue = sd(simulatedActigraphCountByActivity[,4]) * 200
        sim_acti_boxValue = boxplot.stats(simulatedActigraphCountByActivity[,4])
        sim_acti_boxValue$stats = sim_acti_boxValue$stats * 200
        
        actiMeanValue = mean(actigraphCountByActivity[,4])
        actiStdValue = sd(actigraphCountByActivity[,4])
        actiBoxValue = boxplot.stats(actigraphCountByActivity[,4])
        
        
        met_value = mets[mets[,1] == label,2]
        corrected_met_value = mets[mets[,1] == label,3]
        
        result = data.frame(ACTIVITY_NAME = label, mean = meanValue, std = stdValue, 
                            ymin = boxValue$stats[1], lower = boxValue$stats[2],
                            middle = boxValue$stats[3], higher = boxValue$stats[4], ymax = boxValue$stats[5], 
                            mean_sim_acti = sim_acti_meanValue, std_sim_acti = sim_acti_stdValue, 
                            ymin_sim_acti = sim_acti_boxValue$stats[1], lower_sim_acti = sim_acti_boxValue$stats[2],
                            middle_sim_acti = sim_acti_boxValue$stats[3], higher_sim_acti = sim_acti_boxValue$stats[4], ymax_sim_acti = sim_acti_boxValue$stats[5], 
                            mean_acti = actiMeanValue, std_acti = actiStdValue,
                            ymin_acti = actiBoxValue$stats[1], lower_acti = actiBoxValue$stats[2],
                            middle_acti = actiBoxValue$stats[3], higher_acti = actiBoxValue$stats[4], ymax_acti = actiBoxValue$stats[5],met = met_value,corrected_met = corrected_met_value, 
                            n = boxValue$n)
        return(result)
      })
      
      # scale mets value
      shift_value = min(countByActivities[,"corrected_met"])
      scale_factor = max(countByActivities[,"middle"])/max(countByActivities[,"corrected_met"] - shift_value)
      countByActivities[,"corrected_met"] = (countByActivities[,"corrected_met"] - shift_value)*scale_factor
      countByActivities[,"met"] = (countByActivities[,"met"] - shift_value)*scale_factor
      
      dataChunk$countByActivity[[epoch]] = countByActivities
      filename = paste(dataChunk$sensorId, dataChunk$sensorLocation, "countsByActivities", paste0(epoch,"sec.csv"), sep = "_")
      write.table(x = countByActivities, file = paste0(file.path(folder, subj, summary_folder), "/", filename), sep = ",", na = "NA", col.names = TRUE, row.names = FALSE, quote = FALSE)
      countByActivities = countByActivities[order(countByActivities[,"met"]),]
      levels = as.character(countByActivities[,1])
      countByActivities[,1] = factor(countByActivities[,1], levels = levels)
      
      p = ggplot(countByActivities, aes(ACTIVITY_NAME)) + 
        geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin, lower = lower, middle = middle, upper = higher, ymax = ymax, color = "our count"), stat = "identity", fill = NA) + 
            geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin_acti, lower = lower_acti, middle = middle_acti, upper = higher_acti, ymax = ymax_acti, color = "actigraph count"), stat = "identity", fill = NA) +
            # geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin_sim_acti, lower = lower_sim_acti, middle = middle_sim_acti, upper = higher_sim_acti, ymax = ymax_sim_acti, color = "simed actigraph count"), stat = "identity", fill = NA) +
            geom_boxplot(aes(x = ACTIVITY_NAME, lower = met, ymin = met, ymax = met, upper = met, middle = met, color = "met"), stat = "identity", fill = NA) +
            geom_boxplot(aes(x = ACTIVITY_NAME, lower = corrected_met, ymin = corrected_met, ymax = corrected_met, upper = corrected_met, middle = corrected_met, color = "met"), stat = "identity", fill = NA) + coord_flip()
      
      p = p + theme_few() + ggtitle(paste(id, location, sep = "_")) + guides(alpha = FALSE, color = FALSE) + theme(legend.title = element_blank()) + ylim(0, 5000)
      dataChunk$countByActivityPlot[[epoch]] = p
      boxplot_filename = paste(id, location, "countByActivities", paste0(epoch, "sec.png"), sep = "_")
      boxplot_filepath = normalizePath(file.path(folder, subj, plot_folder, "countByActivities"))
      dir.create(boxplot_filepath, recursive = TRUE)
      ggsave(filename = boxplot_filename, path = boxplot_filepath, plot = p, width = 6, height = 4, scale = 2)
    }
      
    return(dataChunk)
  })
  
  plotList = llply(listOfData, function(dataChunk){
    return(dataChunk$countByActivityPlot[10][[1]])
  })
  plots = arrangeGrob(grobs = plotList, ncol = 2, nrow = 4)
  mplots = grid.arrange(plots)
  boxplot_filepath = normalizePath(file.path(folder, subj, plot_folder, "countByActivities"))
  ggsave(filename = "countByActivities_10sec.png", path = boxplot_filepath, plot = mplots, width = 10, height = 6, scale = 3.5)
}
