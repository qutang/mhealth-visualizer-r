---
layout: doc
title: API Document
---

#### `SensorData.importGT3X`: Import and decode GT3X files and load into dataframe as mhealth format. ####

#### Usage ####

```r
SensorData.importGT3X(filename, dest = file.path(getwd(), ".fromGT3X"),
split = FALSE)
```

#### Arguments ####

* `filename`: full file path of input gt3x binary data file, should have extension "gt3x".
* `dest`: full directory path of destination folder. Default is ".fromGT3X" folder of current working directory.
* `split`: Whether to split input data into hourly dataframe list.


#### Value ####


 list of dataframes storing decoded gt3x sensor data file.


#### Seealso ####


 [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importBinary`](SensorData.importBinary.html), [`SensorData.importActigraphCsv`](SensorData.importActigraphCsv.html)


#### Note ####


 it will call `SensorData.importCsv` after decoding GT3X binary data.


