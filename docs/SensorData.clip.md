---
layout: doc
title: API Document
---

#### `SensorData.clip`: Clip sensor data according to the start and end time. ####

#### Usage ####

```r
SensorData.clip(sensorData, startTime, endTime)
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification.
* `startTime`: POSIct date object for start time.
* `endTime`: POSIct date object for end time.


#### Note ####


 Make sure that the data frame is including timestamps.


