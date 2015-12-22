---
layout: doc
title: API Document
---

#### `SensorData.io.write`: Write sensor data into mhealth folder structure and with mhealth filename convention. ####

#### Usage ####

```r
SensorData.io.write(folder, sensorData, sensorType, dataType, sensorId,
versionCode = "NA", tz, gzip = TRUE, flatDir = FALSE,
splitHour = TRUE)
```

#### Arguments ####

* `folder`: the output folder
* `sensorData`: the input dataframe that matches mhealth specification.
* `sensorType`: the sensor type string used in filename.
* `dataType`: the data type string used in filename.
* `sensorId`: the sensor ID string used in filename.
* `versionCode`: the version code string used in filename; default is "NA".
* `tz`: the time zone string (P/MHHMM) used in filename.
* `gzip`: whether to gzip the output csv file.
* `flatDir`: whether to use mhealth folder structure or just use flat directory.
* `splitHour`: whether to split input dataframe into hourly csv files.


