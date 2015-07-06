require("lubridate")
source("io/sensorData.R")
source("postprocess/TimeDomainSummary.R")
source("visualization/SummaryVisualization.R")
# files = list.files(path = "~/Projects/Android/DebugSPADESApp/SPADESTEST_02/FromPhone/data/SPADESTEST_02/", pattern = "watch", ignore.case = TRUE, full.names = TRUE, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, no.. = TRUE)
# onlyNames = list.files(path = "~/Projects/Android/DebugSPADESApp/SPADESTEST_02/FromPhone/data/SPADESTEST_02/", pattern = "watch", ignore.case = TRUE, full.names = FALSE, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, no.. = TRUE)
# 
# filename = files[1]
filename = "~/Desktop/GWatchR8E51-ACCELERATION-CALIBRATED.0C4885238E51.2015-06-09-20-01-00-218-M0400.csv.gz"
# for(filename in files){
  print(paste(filename, "Start process"))
  dat = SensorData.importCsv(filename)
  breaks = "min"
  print(paste(filename, "imported"))
  sr_dat = SamplingRate.summary(dat, breaks = breaks)
  print(paste(filename, "summarized"))
  # dirName = "~/Projects/Android/DebugSPADESApp/SPADESTEST_02/SamplingRateAnalysis/0701/";
  dirName = "~/Desktop/"
  dir.create(dirName, recursive = TRUE)
  newFile = paste(dirName, 
                  gsub(pattern = "csv.gz", replacement = "png", x = basename(filename), perl = TRUE), sep="")
  png(filename = newFile)
  SamplingRate.plot(sr_dat)
  dev.off()
  print(paste(newFile, "got written"))
# }
