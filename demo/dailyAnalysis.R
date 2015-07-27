# daily data loss analysis
path = "/Users/qutang/Projects/Android/DebugSPADESApp/SPADESTEST_01/FromWatch/data/MasterSynced/2015/07/23/"
bafs = list.files(path = path, pattern = ".baf", full.names = TRUE, recursive = TRUE)

datsInList = lapply(bafs, function(input){
  return(SensorData.importBinary(input))
})

dat = SensorData.merge(datsInList)
srDat = SamplingRate.summary(dat, breaks = "hour")

SamplingRate.plot(srDat, unit = "Dataloss", 3000*60)
