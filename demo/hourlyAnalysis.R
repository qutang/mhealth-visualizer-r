# import hourly data
path = "/Users/qutang/Projects/Android/DebugSPADESApp/SPADESTEST_01/FromWatch/data/MasterSynced/2015/07/22/11/GWatchRA326-ACCELERATION-CALIBRATED.34FCEF30A326.2015-07-22-11-00-00-755-M0400.baf"

dat = SensorData.importBinary(path)
srDat = SamplingRate.summary(dat, breaks = "min")

SamplingRate.plot(srDat, unit = "Count", 3000)
