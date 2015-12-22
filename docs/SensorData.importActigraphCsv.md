---
layout: doc
title: API Document
---

#### `SensorData.importActigraphCsv`: Import and convert Actigraph raw csv files and load into data frame as in mhealth format. ####

#### Usage ####

```r
SensorData.importActigraphCsv(filename)
```

#### Arguments ####

* `filename`: full file path of input Actigraph raw csv file.


#### Seealso ####


 [`SensorData.importCsv`](SensorData.importCsv.html), [`SensorData.importGT3X`](SensorData.importGT3X.html), [`SensorData.importBinary`](SensorData.importBinary.html)


#### Note ####


 Please make sure the Actigraph raw csv file has timestamp included. The Actigraph raw csv file is not IMU csv file supported by GT9X.


