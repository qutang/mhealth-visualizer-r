require(mhealthformatsupportr)
require(plyr)
require(dplyr)
require(ggplot2)

folder = "offline_data/running_maxed_out/"

gt3xFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT3X.*.csv.*", recursive = TRUE)[[1]]

gt3xbtFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT3XBT.*.csv.*", recursive = TRUE)[[1]]

gt3xplusFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT3XPLUS.*.csv.*", recursive = TRUE)[[1]]

gt9xFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT9X.*.csv.*", recursive = TRUE)[[1]]

gt3xData = SensorData.importCsv(gt3xFile, violate = TRUE)
gt3xbtData = SensorData.importCsv(gt3xbtFile, violate = TRUE)
gt3xplusData = SensorData.importCsv(gt3xplusFile, violate = TRUE)
gt9xData = SensorData.importCsv(gt9xFile, violate = TRUE)

clipStart = "2014-12-17 11:08:00"
clipEnd = "2014-12-17 11:09:00"
gt3xData = SensorData.clip(gt3xData, startTime = clipStart, endTime = clipEnd)
gt3xbtData = SensorData.clip(gt3xbtData, startTime = clipStart, endTime = clipEnd)
gt3xplusData = SensorData.clip(gt3xplusData, startTime = clipStart, endTime = clipEnd)
gt9xData = SensorData.clip(gt9xData, startTime = clipStart, endTime = clipEnd)

gt3xbtData_cropped = SensorData.crop(gt3xbtData, range = c(-3,3))
gt3xplusData_cropped = SensorData.crop(gt3xplusData, range = c(-3, 3))

# save them as Actigraph CSV 

# headStr_gt3x = SensorData.createActigraphCsvHeader(gt3xData$HEADER_TIME_STAMP[1], last(gt3xData$HEADER_TIME_STAMP), samplingRate = 30, sensorId = "MAT2C47090050", firmVersion = "4.4.0", softVersion = "6.11.5")
# headStr_gt3xbt = SensorData.createActigraphCsvHeader(gt3xbtData$HEADER_TIME_STAMP[1], last(gt3xbtData$HEADER_TIME_STAMP), samplingRate = 80, sensorId = "MOS2A45130448", firmVersion = "1.3.0", softVersion = "6.11.5")
# headStr_gt3xplus = SensorData.createActigraphCsvHeader(gt3xplusData$HEADER_TIME_STAMP[1], last(gt3xplusData$HEADER_TIME_STAMP), samplingRate = 80, sensorId = "CLE2B41120128", firmVersion = "2.2.1", softVersion = "6.11.5")
# headStr_gt9x = SensorData.createActigraphCsvHeader(gt9xData$HEADER_TIME_STAMP[1], last(gt9xData$HEADER_TIME_STAMP), samplingRate = 100, sensorId = "TAS1C32140067", firmVersion = "1.1.0", softVersion = "6.11.5")
# 
# SensorData.io.writeAsActigraphRaw("inst/publications/.temp/running_maxed_out/", gt3xData, headerStr = headStr_gt3x, custom_name = "gt3x.csv")
# SensorData.io.writeAsActigraphRaw("inst/publications/.temp/running_maxed_out/", gt3xbtData_cropped, headerStr = headStr_gt3xbt, custom_name = "gt3xbt.csv")
# SensorData.io.writeAsActigraphRaw("inst/publications/.temp/running_maxed_out/", gt3xplusData_cropped, headerStr = headStr_gt3xplus, custom_name = "gt3xplus.csv")
# SensorData.io.writeAsActigraphRaw("inst/publications/.temp/running_maxed_out/", gt9xData, headerStr = headStr_gt9x, custom_name = "gt9x.csv")


# read back the computed actigraph count values
gt3xCounts = SensorData.importActigraphCountCsv("inst/publications/.temp/running_maxed_out/counts/gt3x5sec.csv", c(2,5), c("ACTIGRAPH_COUNT_X", "ACTIGRAPH_COUNT_VM"))
gt3xbtCounts = SensorData.importActigraphCountCsv("inst/publications/.temp/running_maxed_out/counts/gt3xbt5sec.csv", c(2,5), c("ACTIGRAPH_COUNT_X", "ACTIGRAPH_COUNT_VM"))
gt3xplusCounts = SensorData.importActigraphCountCsv("inst/publications/.temp/running_maxed_out/counts/gt3xplus5sec.csv", c(2,5), c("ACTIGRAPH_COUNT_X", "ACTIGRAPH_COUNT_VM"))
gt9xCounts = SensorData.importActigraphCountCsv("inst/publications/.temp/running_maxed_out/counts/gt9x5sec.csv", c(2,5), c("ACTIGRAPH_COUNT_X", "ACTIGRAPH_COUNT_VM"))

gt3xCountValue = colMeans(gt3xCounts[,2:3])
gt3xbtCountValue = colMeans(gt3xbtCounts[,2:3])
gt3xplusCountValue = colMeans(gt3xplusCounts[,2:3])
gt9xCountValue = colMeans(gt9xCounts[,2:3])

gt3xData = cbind(gt3xData, SR = 30, GRANGE = 3, TYPE = "GT3X", ACTIGRAPH_COUNT_X = gt3xCountValue[1], ACTIGRAPH_COUNT_VM = gt3xCountValue[2])
gt3xbtData = cbind(gt3xbtData_cropped, SR = 80, GRANGE = 3, TYPE = "GT3XBT", ACTIGRAPH_COUNT_X = gt3xbtCountValue[1], ACTIGRAPH_COUNT_VM = gt3xbtCountValue[2])
gt3xplusData = cbind(gt3xplusData_cropped, SR = 80, GRANGE = 3, TYPE = "GT3XPLUS", ACTIGRAPH_COUNT_X = gt3xplusCountValue[1], ACTIGRAPH_COUNT_VM = gt3xplusCountValue[2])
gt9xData = cbind(gt9xData, SR = 100, GRANGE = 16, TYPE = "GT9X", ACTIGRAPH_COUNT_X = gt9xCountValue[1], ACTIGRAPH_COUNT_VM = gt3xCountValue[2])

running_maxed_out = rbind(gt3xData, gt3xbtData, gt3xplusData, gt9xData)

devtools::use_data(running_maxed_out, compress = "bzip2", overwrite = TRUE)