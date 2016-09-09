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
mets = read.csv(file.path(folder, "mets.SpadesInLab.csv"), stringsAsFactors = FALSE)
mets[,1] = tolower(mets[,1])
mets = with(mets, mets[order(METS),])

# load scaling factor values
scalingFactors = read.csv(file.path(folder, "scalingFactors.SpadesInLab.csv"), stringsAsFactors = FALSE)

for(subj in subjects){
  
  listOfData = listOfSubjects[[subj]]$data
  annotationData = listOfSubjects[[subj]]$annotation
  
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
    count1 = dataChunk$count1
    count2 = dataChunk$count2
    count3 = dataChunk$count3
    count4 = dataChunk$count4
    actigraphCount = dataChunk$actigraphCountData
    for(epoch in epochs){
      selectedSF = scalingFactors[scalingFactors["epoch"] == epoch,]
      countByActivities = ldply(labels, function(label){
        clips = annotationData[annotationData[,4] == label,c(2,3)]
        
        countByActivity1 = adply(clips, 1, function(clip){
          countValue = SensorData.clip(count1[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          # scale
          countValue[,2] = countValue[,2] * selectedSF[1, "slope"] + selectedSF[1, "intercept"]
          countValue[countValue[,2] < 0, 2] = 0
          return(countValue)
        })
        
        countByActivity2 = adply(clips, 1, function(clip){
          countValue = SensorData.clip(count2[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          # scale
          countValue[,2] = countValue[,2] * selectedSF[1, "slope"] + selectedSF[1, "intercept"]
          countValue[countValue[,2] < 0, 2] = 0
          return(countValue)
        })
        
        countByActivity3 = adply(clips, 1, function(clip){
          countValue = SensorData.clip(count3[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          # scale
          countValue[,2] = countValue[,2] * selectedSF[1, "slope"] + selectedSF[1, "intercept"]
          countValue[countValue[,2] < 0, 2] = 0
          return(countValue)
        })
        
        countByActivity4 = adply(clips, 1, function(clip){
          countValue = SensorData.clip(count4[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          # scale
          countValue[,2] = countValue[,2] * selectedSF[1, "slope"] + selectedSF[1, "intercept"]
          countValue[countValue[,2] < 0, 2] = 0
          return(countValue)
        })
        
        actigraphCountByActivity = adply(clips, 1, function(clip){
          countValue = SensorData.clip(actigraphCount[[epoch]], clip[1], clip[2])
          countValue = countValue[c(-1,-nrow(countValue)),]
          return(countValue)
        })

        meanValue1 = mean(countByActivity1[,4])
        stdValue1 = sd(countByActivity1[,4])
        boxValue1 = boxplot.stats(countByActivity1[,4])
        boxValue1$stats = boxValue1$stats
        
        meanValue2 = mean(countByActivity2[,4])
        stdValue2 = sd(countByActivity2[,4])
        boxValue2 = boxplot.stats(countByActivity2[,4])
        boxValue2$stats = boxValue2$stats
        
        meanValue3 = mean(countByActivity3[,4])
        stdValue3 = sd(countByActivity3[,4])
        boxValue3 = boxplot.stats(countByActivity3[,4])
        boxValue3$stats = boxValue3$stats
        
        meanValue4 = mean(countByActivity4[,4])
        stdValue4 = sd(countByActivity4[,4])
        boxValue4 = boxplot.stats(countByActivity4[,4])
        boxValue4$stats = boxValue4$stats
        
        actiMeanValue = mean(actigraphCountByActivity[,4])
        actiStdValue = sd(actigraphCountByActivity[,4])
        actiBoxValue = boxplot.stats(actigraphCountByActivity[,4])
        
        met_value = mets[mets[,1] == label,2]
        corrected_met_value = mets[mets[,1] == label,3]
        
        result = data.frame(ACTIVITY_NAME = label, 
                            mean1 = meanValue1, std1 = stdValue1, 
                            ymin1 = boxValue1$stats[1], lower1 = boxValue1$stats[2],
                            middle1 = boxValue1$stats[3], higher1 = boxValue1$stats[4], ymax1 = boxValue1$stats[5], 
                            mean2 = meanValue2, std2 = stdValue2, 
                            ymin2 = boxValue2$stats[1], lower2 = boxValue2$stats[2],
                            middle2 = boxValue2$stats[3], higher2 = boxValue2$stats[4], ymax2 = boxValue2$stats[5], 
                            mean3 = meanValue3, std3 = stdValue3, 
                            ymin3 = boxValue3$stats[1], lower3 = boxValue3$stats[2],
                            middle3 = boxValue3$stats[3], higher3 = boxValue3$stats[4], ymax3 = boxValue3$stats[5], 
                            mean4 = meanValue4, std4 = stdValue4, 
                            ymin4 = boxValue4$stats[1], lower4 = boxValue4$stats[2],
                            middle4 = boxValue4$stats[3], higher4 = boxValue4$stats[4], ymax4 = boxValue4$stats[5], 
                            mean_acti = actiMeanValue, std_acti = actiStdValue,
                            ymin_acti = actiBoxValue$stats[1], lower_acti = actiBoxValue$stats[2],
                            middle_acti = actiBoxValue$stats[3], higher_acti = actiBoxValue$stats[4], ymax_acti = actiBoxValue$stats[5],met = met_value,corrected_met = corrected_met_value, 
                            n = boxValue1$n)
        return(result)
      })
      
      # scale mets value
      shift_value = min(countByActivities[,"corrected_met"])
      scale_factor = max(countByActivities[,"middle2"])/max(countByActivities[,"corrected_met"] - shift_value)
      countByActivities[,"corrected_met"] = (countByActivities[,"corrected_met"] - shift_value)*scale_factor
      countByActivities[,"met"] = (countByActivities[,"met"] - shift_value)*scale_factor
      
      dataChunk$countByActivity[[epoch]] = countByActivities
      filename = paste(dataChunk$sensorId, dataChunk$sensorLocation, "countsByActivities", paste0(epoch,"sec.csv"), sep = "_")
      write.table(x = countByActivities, file = paste0(file.path(folder, subj, summary_folder), "/", filename), sep = ",", na = "NA", col.names = TRUE, row.names = FALSE, quote = FALSE)
      countByActivities = countByActivities[order(countByActivities[,"met"]),]
      levels = as.character(countByActivities[,1])
      countByActivities[,1] = factor(countByActivities[,1], levels = levels)
      alpha = 0.4
      p = ggplot(countByActivities, aes(ACTIVITY_NAME)) + 
        geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin_acti, lower = lower_acti, middle = middle_acti, upper = higher_acti, ymax = ymax_acti, color = "actigraph count"), stat = "identity", fill = NA, alpha = alpha) +
        geom_boxplot(aes(x = ACTIVITY_NAME, lower = corrected_met, ymin = corrected_met, ymax = corrected_met, upper = corrected_met, middle = corrected_met, color = "corrected met"), stat = "identity", fill = NA, alpha = alpha) +
        geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin1, lower = lower1, middle = middle1, upper = higher1, ymax = ymax1, color = "count (0.6~10)"), stat = "identity", fill = NA, alpha = alpha) + 
            
        geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin2, lower = lower2, middle = middle2, upper = higher2, ymax = ymax2, color = "count (0.25~5)"), stat = "identity", fill = NA, alpha = alpha) +
        geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin3, lower = lower3, middle = middle3, upper = higher3, ymax = ymax3, color = "count (0.25~2.5)"), stat = "identity", fill = NA, alpha = alpha) +
        geom_boxplot(aes(x = ACTIVITY_NAME, ymin = ymin4, lower = lower4, middle = middle4, upper = higher4, ymax = ymax4, color = "count (0.25~10)"), stat = "identity", fill = NA, alpha = alpha) +
            # geom_boxplot(aes(x = ACTIVITY_NAME, lower = met, ymin = met, ymax = met, upper = met, middle = met, color = "met"), stat = "identity", fill = NA, alpha = alpha) +
        coord_flip()
      
      p = p + theme_few() + ggtitle(paste(id, location, sep = "_")) + guides(alpha = FALSE) + ylim(0, 9000) + theme(legend.title = element_blank()) + scale_color_manual(values = c("#000000", "purple", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
      dataChunk$countByActivityPlot[[epoch]] = p
      boxplot_filename = paste(id, location, "countByActivities", paste0(epoch, "sec.png"), sep = "_")
      boxplot_filepath = normalizePath(file.path(folder, subj, plot_folder, "countByActivities"))
      dir.create(boxplot_filepath, recursive = TRUE)
      ggsave(filename = boxplot_filename, path = boxplot_filepath, plot = p, width = 8, height = 5, scale = 2)
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
