# Prepare SPADES lab data for counts analysis
# 
# 1. Clean up and merge raw data files
# 2. Clean up and merge annotation files
# 3. Map locations to different data 

require(mhealthformatsupportr)
require(foreach)
require(ggplot2)
require(reshape2)
require(plyr)
require(stringr)

from = "../../CleanLabDataset"
to = "offline_data/spades_lab"
ready_folder = "ready"
counts_folder = "counts"
actigraph_folder = "actigraph"
sensorLocationFile = "Sensor_location_Lab.csv";
sessionsFile = "Sessions.csv";
sessionDateFormat = "%m/%d/%Y %H:%M";
epoch = 5;

mergeSpadesLabSensorFiles = function(subjects){
  for(subj in subjects){
    
    # Read in location and sensor ID mapping file
    
    sensorLocationMapping = read.csv(file = file.path(from, subj, sensorLocationFile), header = TRUE, as.is = TRUE)
    print(paste("Prepare for", subj))
    
    for(i in seq(1, nrow(sensorLocationMapping))){
      
      id = sensorLocationMapping[i, 1]
      location = sensorLocationMapping[i, 2]
      if(str_detect(location, stringr::regex("^*wear*", ignore_case = TRUE))){
        next
      }
      # if(!str_detect(location, stringr::regex("^dominant waist*", ignore_case = TRUE))){
      #   next
      # }
      print(paste("Prepare", id, "at", location))
      dataFiles = normalizePath(
        list.files(
          path = file.path(from, subj, "MasterSynced"), 
          full.names = TRUE, 
          recursive = TRUE, 
          include.dirs = FALSE, 
          no.. = TRUE,
          pattern = paste0(id, ".*sensor.csv.*")))
      
      
      # Read in and clean up data file for each hour
      listOfData = foreach(dataFile = dataFiles, .combine = c) %dopar% {
        hourlyData = SensorData.importCsv(dataFile)
        hourlyData = SensorData.cleanup(hourlyData)
        return(list(hourlyData))
      }
      
      # merge data files
      if(!is.null(listOfData)){
        mergedData = SensorData.merge(listOfData)
        
        # save merged data to actigraph format csv
        dir.create(file.path(to, subj, actigraph_folder), recursive = TRUE)
        headStr = SensorData.createActigraphCsvHeader(startTime = mergedData[1,1],
                                                      downloadTime = mergedData[nrow(mergedData),1],
                                                      samplingRate = round(SensorData.getSamplingRate(mergedData)/10)*10,
                                                      sensorId = id,
                                                      firmVersion = "1.5.0",
                                                      softVersion = "6.13.2")
        SensorData.io.writeAsActigraphRaw(file.path(to, subj, actigraph_folder),sensorData = mergedData, headerStr = headStr, custom_name = paste(id, location, "merged.actigraph.csv", sep = "_"))
        
        # save merged data to ready folder
        dir.create(file.path(to, subj, ready_folder), recursive = TRUE)
        SensorData.io.write(file.path(to, subj, ready_folder),
                            sensorData = mergedData, 
                            append = FALSE, 
                            header = TRUE, 
                            custom_name = paste(id, location, "merged.sensor.csv", sep = "_"), 
                            gzip = TRUE, 
                            flatDir = TRUE, 
                            splitHour = FALSE)
      }else{
        print(paste('No available data for', location))
      }
      
      }
      
  }
}

mergeSpadesLabAnnotationFiles = function(subjects){
  
  for(subj in subjects){
    
    # Annotation file
    annotationFiles = normalizePath(list.files(
      path = file.path(from, subj, "MasterSynced"),
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = FALSE,
      no.. = TRUE,
      pattern = "SPADESInLab.*annotation.csv.*"
    ))


    # Read in and categorize annotation file for each hour
    ontologyId = AnnotationData.io.getOntologyId(basename(annotationFiles[1]))
    annotatorId = AnnotationData.io.getAnnotatorId(basename(annotationFiles[1]))
    print(paste("Prepare", ontologyId, "by", annotatorId, "subject:", subj))
    listOfAnnotations = foreach(annFile = annotationFiles, .combine = c) %dopar% {
      hourlyAnnotation = AnnotationData.importCsv(annFile)
      hourlyAnnotation = hourlyAnnotation[,1:4]
      return(list(hourlyAnnotation))
    }

    # merge annotation files
    mergedAnnotation = AnnotationData.merge(listOfAnnotations)

    # save merged annotation to ready folder
    AnnotationData.io.write(file.path(to, subj, ready_folder),
                            annotationData = mergedAnnotation,
                            append = FALSE,
                            header = TRUE,
                            custom_name = paste(ontologyId, annotatorId, "merged.annotation.csv", sep = "_"),
                            flatDir = TRUE,
                            splitHour = FALSE, 
                            gzip = FALSE)
  }
}

computeCounts = function(subjects){
  for(subj in subjects){
    
    print(paste("Compute activity counts for ", subj))
    
    sensorFiles = normalizePath(list.files(path = file.path(to, subj, ready_folder), pattern = "TAS.*sensor.csv.*", full.names = TRUE))
    
    # summarize sensors
    for(sensorFile in sensorFiles){
      sensorId = str_split(basename(sensorFile), "_")[[1]][1]
      sensorLocation = str_split(basename(sensorFile), "_")[[1]][2]
      
      print(paste("Read in", sensorId, "at", sensorLocation))
      
      if(subj == "SPADES_7" && str_detect(sensorLocation, "^non dominant wrist*")){
        print("This sensor is malfunctioning, skip")
        next
      }
      
      sensorData = SensorData.importCsv(sensorFile)

      count = SensorData.summary.counts.compute(sensorData, breaks = paste(epoch, "secs"), 
                                                range = c(-8, 8), 
                                                noise_std = 0.05, 
                                                k = 0.1, 
                                                spar = 0.4, 
                                                resample = 50, 
                                                filterType = "butter", 
                                                cutoffs = c(0.2, 5), 
                                                integrationType = "trapz")
      dir.create(file.path(to, subj, counts_folder, epoch), recursive = TRUE)
      SensorData.io.write(file.path(to, subj, counts_folder, epoch), count, custom_name = paste(paste(sensorId, sensorLocation, sep = "_"), "count", "csv", sep = "."), append = FALSE, 
                          header = TRUE, 
                          gzip = FALSE, 
                          flatDir = TRUE, 
                          splitHour = FALSE)
    }
  }
}

extractActivities = function(subjects){
  for (subj in subjects) {
    print(paste('extra activities of', subj))
    folder = file.path(to, subj, ready_folder)
    annFile = list.files(path = folder, pattern = "SPADESInLab.*annotation.csv.*", full.names = TRUE, recursive = TRUE)
    if(is.vector(annFile)){
      annFile = annFile[1]
    }
    annotations = AnnotationData.importCsv(annFile)
    annotations = AnnotationData.simplify(annotations)
    # select activities
    markedActivities = list("lying" = c("Lying"),
                            "sitting" = c("Sitting", "not writing", "not web browsing"),
                            "sitting web browsing" = c("sitting", "web browsing"),
                            "sitting writing" = c("sitting", "writing"),
                            "standing" = c("Standing", "not laundry", "not shelf unload", "not shelf reload", "not sweeping", "not writing", "not web browsing"),
                            "standing web browsing" = c("standing", "web browsing"),
                            "standing writing" = c("standing", "writing"),
                            "walking at 1mph arms on desk" = c("Walking", "Treadmill", "1 mph"),
                            "walking at 2mph arms on desk" = c("Walking", "Treadmill", "2 mph"),
                            "walking at 3mph" = c("Walking", "Treadmill", "3 mph", "not Phone Talking", "not carrying drink", "not bag"),
                            "walking at 3mph carrying drink" = c("Walking or 3 mph", "Treadmill", "Carrying Drink"),
                            "walking at 3mph carrying bag" = c("Walking or 3 mph", "Treadmill", "bag"),
                            "walking at 3mph phone talking" = c("Walking or 3 mph", "Treadmill", "Phone Talking or telling story"),
                            "walking at 3.5mph" = c("Walking", "3.5 mph", "Treadmill"),
                            "running at 5.5mph 5% grade" = c("Jog/Running or 5.5 mph"), 
                            "walking outdoor" = c("Walking", "Outdoors or City"),
                            "walking upstairs" = c("Walking", "Stairs", "Up"),
                            "walking donwstairs" = c("Walking", "Stairs", "Down"), 
                            "biking at 300 KPM/Min" = c("Biking", "Stationary or 300 KPM/Min"), 
                            "biking outdoor" = c("Biking", "Outdoors or City"),
                            "sweeping" = c("Sweeping"), 
                            "laundry" = c("Laundry"),
                            "shelf unload" = c("shelf unload", "not shelf reload"),
                            "shelf reload" = c("shelf reload", "not shelf unload"),
                            "frisbee" = c("Frisbee")
    )
    activities = ldply(markedActivities, function(labels){
      or_labels = str_detect(labels, " or ")
      not_labels = str_detect(labels, "not ")
      if (sum(or_labels) > 0) {
        or = str_split(labels[or_labels], " or ")
      }else{
        or = c()
      }
      if(sum(not_labels) > 0) {
        not = str_split(labels[not_labels], "not ")
      }else{
        not = c()
      }
      and = labels[!or_labels & !not_labels]
      
      sub_ann = AnnotationData.simplify.filter(annotations, labels = and, include.labels = TRUE, and.logic = TRUE)
      if (length(or) != 0) {
        for(oo in or){
          sub_ann = AnnotationData.simplify.filter(sub_ann, labels = oo, include.labels = TRUE, and.logic = FALSE)
        }
      }
      if (length(not) != 0){
        for(nn in not){
          sub_ann = AnnotationData.simplify.filter(sub_ann, labels = nn[2], include.labels = FALSE, and.logic = TRUE)
        }
      }
      # connect adjacent annotations
      i = 1
      sub_ann_connected = data.frame()
      while(i <= nrow(sub_ann)){
        if(i == nrow(sub_ann)){ sub_ann_connected = data.frame(HEADER_TIME_STAMP = sub_ann[i, 1], 
                                                               START_TIME = sub_ann[i, 2], 
                                                               STOP_TIME = sub_ann[i, 3],
                                                               LABEL_NAME = paste0('"',sub_ann[i, 4], '"'), stringsAsFactors = FALSE); break;}
        j = i
        while(sub_ann[i, 3] == sub_ann[i+1,2]){
          i = i + 1
          if(i == nrow(sub_ann)) break;
        }
        label_name = paste0('"', paste(unique(unlist(str_split(sub_ann[j:i, 4], ","))), collapse = ","), '"')
        sub_ann_connected = rbind(sub_ann_connected, data.frame(HEADER_TIME_STAMP = sub_ann[j, 1], 
                                            START_TIME = sub_ann[j, 2], 
                                            STOP_TIME = sub_ann[i, 3],
                                            LABEL_NAME = label_name, stringsAsFactors = FALSE))
        if(i == nrow(sub_ann)){ break; }
        else{
          i = i + 1
          }
      }
      
      # exclude annotations that are less than 5 seconds
      sub_ann_connected = adply(sub_ann_connected, 1, function(row){
        if(as.numeric(row[[3]] - row[[2]], units = "secs") < 5){
          return(NULL)
        }else{
          return(row)
        }
      })
      return(sub_ann_connected)
    })
    names(activities)[1] = "ACTIVITY_NAME"
    activities = activities[c(1,3,4,5)]
    
    AnnotationData.io.write(file.path(to, subj, counts_folder), activities, custom_name = str_replace(basename(annFile), "annotation.*", "activity.csv"), gzip = FALSE, flatDir = TRUE, splitHour = FALSE, append = FALSE, header = TRUE)
  }
}

.extractCountsGivenDuration = function(counts, startTime, endTime){
  selected_counts = SensorData.clip(counts, startTime = startTime, endTime = endTime)
  if(nrow(selected_counts) > 6){
    selected_counts = selected_counts[3:(nrow(selected_counts)-2),]
  }else if(nrow(selected_counts) > 3){
    selected_counts = selected_counts[2:(nrow(selected_counts)-1),]
  }else{
    selected_counts = selected_counts[0, ]
  }
  return(selected_counts)
}

getCountsPerActivities = function(subjects, locations = c("dominant wrist", "dominant waist")){
  final_result = data.frame()
  for(subj in subjects){
    print(paste("process", subj))
    actFile = list.files(path = file.path(to, subj, counts_folder), pattern = "SPADESInLab.*activity.*", full.names = TRUE, recursive = FALSE)
    if(length(actFile) == 0){
      print(paste("Not found activity file:", subj))
      next;
    }else{
      actFile = actFile[[1]]
      activities = read.csv(actFile, quote = '"', stringsAsFactors = FALSE)
      activities[,2] = as.POSIXct(activities[,2], format = "%Y-%m-%d %H:%M:%OS")
      activities[,3] = as.POSIXct(activities[,3], format = "%Y-%m-%d %H:%M:%OS")
    }
    
    ownCounts = list()
    actiCounts = list()
    for(loc in locations){
      # our own counts
      ownCountsFile = list.files(path = file.path(to, subj, counts_folder, epoch), pattern = paste0("TAS.*_", loc, ".*count.*"), full.names = TRUE, recursive = FALSE)
      
      if(length(ownCountsFile) == 0){
        print(paste("Not found own counts:", subj, ",", loc))
      }else{
        ownCountsFile = ownCountsFile[[1]]
        ownCounts[[loc]] = read.csv(ownCountsFile, stringsAsFactors = FALSE)
        ownCounts[[loc]][,1] = as.POSIXct(ownCounts[[loc]][,1], format = "%Y-%m-%d %H:%M:%OS")
      }
      
      # actigraph counts
      actiCountsFile = list.files(path = file.path(to, subj, counts_folder, epoch), pattern = paste0("TAS.*_", loc, ".*actigraph.*"), full.names = TRUE, recursive = FALSE)
      if(length(actiCountsFile) == 0){
        print(paste("Not found actigraph counts:", subj, ",", loc))
      }else{
        actiCountsFile = actiCountsFile[[1]]
        actiCounts[[loc]] = read.csv(actiCountsFile, stringsAsFactors = FALSE)
        actiCounts[[loc]][,1] = as.POSIXct(actiCounts[[loc]][,1], format = "%Y-%m-%d %H:%M:%OS")
      }
    }
    
    countsPerActivity = adply(activities, 1, function(row){
      st = row[[2]]
      et = row[[3]]
      result = data.frame()
      for(loc in locations){


        selected_counts_own = .extractCountsGivenDuration(ownCounts[[loc]], st, et)
        selected_counts_actigraph = .extractCountsGivenDuration(actiCounts[[loc]], st, et)

        if(!all(selected_counts_actigraph[,1] == selected_counts_actigraph[,1])){
          print(paste("rerun actilife on this subject:", subj, ",", loc))
        }else{
          if(nrow(selected_counts_own) > 0){
            result_own = data.frame(row, value = selected_counts_own[,2], TYPE = "COUNTS", LOCATION = toupper(loc),stringsAsFactors = FALSE)
            result = rbind(result, result_own)
          }
          if(nrow(selected_counts_actigraph) > 0){
            result_actigraph = data.frame(row, value = selected_counts_actigraph[,2], TYPE = "ACTIGRAPH", LOCATION = toupper(loc), stringsAsFactors = FALSE)
            result = rbind(result, result_actigraph)
          }
        }
      }
      return(result)
    })
    countsPerActivity = data.frame(countsPerActivity, SUBJECT = subj, stringsAsFactors = FALSE)
    final_result = rbind(final_result, countsPerActivity)
  }
  return(final_result)
}

ids = seq(1,51)

# ids = c(7, 43, 45, 47, 48, 49, 50)
# ids = c(4)

exclude_ids = c(5, 33)
# 5 doesn't exist
# 33 has corrupted annotation files
# 4, 6, 7, 8, 9 needs to check and fix the sensor mapping

# annotations
# actFile_ids = c(4,6,7,8,9)
# actFile_ids = setdiff(actFile_ids, exclude_ids)
# actFile_subjects = paste("SPADES", actFile_ids, sep = "_")
# mergeSpadesLabAnnotationFiles(actFile_subjects)
# extractActivities(actFile_subjects)

# sensors
# sensor_ids = c(7)
# sensor_ids = setdiff(sensor_ids, exclude_ids)
# sensor_subjects = paste("SPADES", sensor_ids, sep = "_")
# mergeSpadesLabSensorFiles(sensor_subjects)
# computeCounts(sensor_subjects)


# overall
ids = setdiff(ids, exclude_ids)
subjects = paste("SPADES", ids, sep = "_")
spades_lab_counts = getCountsPerActivities(subjects, locations = c("dominant wrist", "dominant waist"))
devtools::use_data(spades_lab_counts, compress = "bzip2", overwrite = TRUE)




  