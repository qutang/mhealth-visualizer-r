# import hourly data
path = "/Users/qutang/Projects/Android/DebugSPADESApp/SPADESTEST_01/FromPhone/data/MasterSynced/2015/08/07/08/"
path = "/Users/qutang/spades_wockets_server/data/SPADESTEST_07/.SPADESTEST/data/SPADESTEST_07/MasterSynced/2015/09/10/04/"

file = list.files(path, full.names = TRUE, pattern = "Watch")[[1]]
dat = SensorData.importCsv(file)
srDat = SamplingRate.summary(dat, breaks = "min")

SamplingRate.plot(srDat, unit = "Count", 3000)
