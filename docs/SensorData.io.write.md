---
layout: doc
title: API Document
---

#### `SensorData.io.write`: Write sensor data into mhealth folder structure and with mhealth filename convention. ####

#### Usage ####

```r
SensorData.io.write(folder, sensorData, sensorType = NA, dataType = NA,
sensorId = NA, versionCode = "NA", tz, gzip = TRUE, flatDir = FALSE,
splitHour = TRUE, custom_name, append = FALSE, header = TRUE)
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
* `custom_name`: if provided, the file name will be custom_name and all other file name preset parameters such as sensorType will be discarded.
* `append`: whether to append to a file if the file exists
* `header`: whether to add column header or not


