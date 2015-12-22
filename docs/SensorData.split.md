---
layout: doc
title: API Document
---

#### `SensorData.split`: Split sensor data into list of smaller data frame with meaningful intervals (e.g. hourly, minutely, secondly or daily) ####

#### Usage ####

```r
SensorData.split(sensorData, breaks = "hour")
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification.
* `breaks`: "sec","min","hour","day","week","month","quarter" or "year"; or preceded by integer and space.


#### Value ####


 list of splitted dataframes


