require(stringr)
require(R.utils)
require(plyr)
directory_path = "../../CleanLabDataset/"
subject_name_pattern = "SPADES_.*"
report_file = paste0("../../../Desktop/report-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),".txt")

.check_single_file = function(path, file_name_pattern, exact_file_name, connection){
  targetFile = list.files(path, pattern = file_name_pattern, ignore.case = TRUE, full.names = TRUE)
  if(length(targetFile) == 1){
    if(basename(targetFile) != exact_file_name){
      if(renameFile(targetFile, file.path(dirname(targetFile), exact_file_name, overwrite = TRUE, showWarnings = TRUE))){
        writeLines(paste(basename(targetFile), "has been renamed to", exact_file_name), con = connection)
      }else{
        writeLines(paste(basename(targetFile), "needs to be renamed to", exact_file_name), con = connection)
      }
    }
  }else if(length(targetFile) > 1){
    writeLines(paste("Multiple", exact_file_name, "files are found:", paste(basename(targetFile), collapse = ",")), con = connection)
  }else{
    writeLines(paste(exact_file_name,"is not found"), con = connection)
  }
}

.delete_single_file = function(path, file_name_pattern, exact_file_name, connection){
  targetFile = list.files(path, pattern = file_name_pattern, ignore.case = TRUE, full.names = TRUE)
  if(length(targetFile) >= 1){
    for(file in targetFile){
      file.remove(file)
      writeLines(paste(basename(file), "is deleted"), con = connection)
    }
  }else{
    writeLines(paste(exact_file_name,"is not found"), con = connection)
  }
}

.delete_files = function(path, file_name_pattern, connection){
  files = list.files(path, pattern = file_name_pattern, all.files = TRUE, recursive = TRUE, ignore.case = TRUE, full.names = TRUE, no.. = TRUE, include.dirs = TRUE)
  if(length(files) > 0){
    file.remove(files)
    writeLines(paste("Deleted", files), con = connection)
  }
}

.distribute_config_file = function(from, to, subject_name_pattern, exact_file_name, connection){
  fromFiles = list.files(from, full.names = TRUE, recursive = TRUE)
  for(file in fromFiles){
    pattern_folder_name = str_extract(file, regex(subject_name_pattern, ignore_case = TRUE))
    file.copy(from = file, to = file.path(to, pattern_folder_name, exact_file_name), overwrite = TRUE)
    writeLines(paste("Distributed", file, "to", pattern_folder_name), con = connection)
  }
}

.extract_config_file = function(from, to, subj_name_pattern, file_name_pattern, connection){
  subj_dirs = normalizePath(list.files(file.path(from), pattern = paste0(".*", subj_name_pattern, ".*"),recursive = FALSE, full.names = TRUE, include.dirs = TRUE, no.. = TRUE))
  for(subj_dir in subj_dirs){
    fromFiles = list.files(subj_dir, file_name_pattern, full.names = TRUE, ignore.case = TRUE)
    file.copy(fromFiles, file.path(to, paste(basename(subj_dir), basename(fromFiles), sep = "_")))
  }
}

.check_files_presence = function(path, file_name_pattern, file_display_name, connection){
  targetFiles = list.files(path, pattern = file_name_pattern, full.names = TRUE, recursive = TRUE)
  targetPaths = dirname(targetFiles)
  dates = targetPaths
  dates = Map(function(path){
    date = str_extract(dirname(path), "[0-9]{4}/[0-9]{2}/[0-9]{2}")
  }, dates)
  dates = unique(unlist(dates))
  result = c()
  for(date in dates){
    hours = basename(targetPaths[str_detect(targetPaths, date)])
    result = c(result, paste(date, paste(hours, collapse = ","), sep = ": "))
  }
  if(length(result) > 0){
    writeLines(c(paste(file_display_name, "is found in:"), result), con = connection)
  }
  else writeLines(c(paste(file_display_name, "is not found"), result), con = connection)
  return(targetPaths)
}

.check_sensor_ids = function(path, sensor_type, file_display_name, connection){
  targetFiles = list.files(path, pattern = paste0(sensor_type, ".*sensor.*"), full.names = FALSE, recursive = TRUE)
  tokens = str_split(targetFiles, "\\.")
  ids = llply(tokens, function(item){
    return(str_split(item[[2]], "-")[[1]][1])
  })
  ids = unique(unlist(ids))
  writeLines(c(paste(file_display_name, length(ids)), ids), con = connection)
}

connection = file(report_file, open = "at")
dirs = normalizePath(list.dirs(file.path(directory_path), recursive = FALSE))

writeLines(paste("====== Check", normalizePath(directory_path), "======"), con = connection)
# check subject folder pattern and if there is any extra folder
is_subj_dir = str_detect(dirs, subject_name_pattern)
subj_dirs = dirs[is_subj_dir]
extra_dirs = dirs[!is_subj_dir]

if(length(extra_dirs) > 0) writeLines(paste("Found extra directory in mhealth dataset:", paste(basename(extra_dirs), collapse = ",")), con = connection)
# loop over subj_dirs

subj_dirs = sort(subj_dirs, decreasing = FALSE) 
for(subj_dir in subj_dirs){
  writeLines(paste("======", basename(subj_dir), "======"), con = connection)
  # check if Sessions.csv exists and matches filename exactly
  # .check_single_file(subj_dir, ".*Sessions.*.csv", "Sessions.csv", connection = connection)
  # .delete_single_file(subj_dir, ".*Sessions.*.csv", "Sessions.csv", connection = connection)
  # 
  # .delete_files(subj_dir, "^\\..*", connection = connection)
  
  # check if Subject.csv exists and matches filename exactly
  # .check_single_file(subj_dir, ".*Subject.*.csv", "Subject.csv", connection = connection)
  
  # .check_single_file(subj_dir, ".*note.*", "note.txt", connection = connection)
  
  # check if Sensor location file exists and matches filename exactly
  # .check_single_file(subj_dir, ".*Sensor_location_lab.*", "Sensor_location_lab.csv", connection = connection)
  
  # check which date and hours contains certain type of files
  .check_files_presence(subj_dir, "SPADESInLab.*annotation.*", "SPADES Lab annotation files", connection)
  # 
  .check_files_presence(subj_dir, "AndroidPhone.*sensor.*", "Android phone files", connection)
  .check_files_presence(subj_dir, "AndroidWearWatch.*sensor.*", "Android wear files", connection)
  # 
  # .check_sensor_ids(subj_dir, "AndroidPhone", "Android phone ids", connection)
  # .check_sensor_ids(subj_dir, "ActigraphGT9X", "Actigraph ids", connection)
  # .check_sensor_ids(subj_dir, "AndroidWearWatch", "Android Wear watch ids", connection)
}

# .distribute_config_file(from = "../../../Desktop/SPADES_Subject/", to = directory_path, subject_name_pattern = "SPADES_[0-9]+", exact_file_name = "Subject.csv", connection = connection)

# .distribute_config_file(from = "../../../Desktop/SPADES_Session/", to = directory_path, subject_name_pattern = "SPADES_[0-9]+", exact_file_name = "Sessions.csv", connection = connection)

# .extract_config_file(from = "E:/CleanLabDataset_updated/data/", to = "../../../Desktop/SPADES_SensorLocation/", subj_name_pattern = "SPADES_[0-9]+", file_name_pattern = ".*Sensor_location_lab.*", connection = connection)

# .distribute_config_file(from = "../../../Desktop/SPADES_SensorLocation/", to = directory_path, subject_name_pattern = "SPADES_[0-9]+", exact_file_name = "Sensor_location_lab.csv", connection = connection)

close(connection)

