---
layout: doc
title: API Document
---

#### `SensorData.importBinary`: Import and decode binary file from the smart watch and load into dataframe as mhealth format. ####

#### Usage ####

```r
SensorData.importBinary(filename, dest = file.path(getwd(), ".fromBinary"))
```

#### Arguments ####

* `filename`: full file path of input smart watch binary data file.
* `dest`: full directory path of destination folder. Default is ".fromBinary" folder of current working directory.


#### Seealso ####


 [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importGT3X`](SensorData.importGT3X.html), [`SensorData.importActigraphCsv`](SensorData.importActigraphCsv.html)


#### Note ####


 It will call `SensorData.importCsv` after decoding.


