require(mhealthformatsupportr)
require(plyr)
require(dplyr)
sr = 100
x = seq(as.POSIXct("2010-01-01 18:00:00"), as.POSIXct("2010-01-01 18:01:00"), by = 1/sr)
fs = seq(0.1, 10, by = 0.05)

# Create test files as raw actigraph csv
# for(f in fs){
#   y = sin(2* pi * f * as.numeric(x - x[1]))
#   data = data.frame(x, y, y, y)
#   headStr = SensorData.createActigraphCsvHeader(as.POSIXct("2010/01/01 18:00:00"), as.POSIXct("2010/01/01 19:00:00"), samplingRate = sr, sensorId = "TASACTIGRAPHSPECTRUM", firmVersion = "1.4.0", softVersion = "6.13.2")
#   SensorData.io.writeAsActigraphRaw("inst/publications/.temp/actigraph_spectrum/", data, headStr, custom_name = paste0("f",f, ".csv"))
# }


# load back actigraph count from the created test files

actigraph_spectrum = ldply(fs, function(f){
  data = SensorData.importActigraphCountCsv(paste0("inst/publications/.temp/actigraph_spectrum/counts/f",f,"5sec.csv"),count_col = 2, count_col_name = "ACTIGRAPH_COUNT")
  return(data.frame(freq = f, value = mean(data[,2])))
}, .progress = progress_text())

actigraph_spectrum[,2] = actigraph_spectrum[,2] / max(actigraph_spectrum[,2])

devtools::use_data(actigraph_spectrum, compress = "bzip2", overwrite = TRUE)
