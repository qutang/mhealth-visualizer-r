---
layout: doc
title: API Document
---

#### `SamplingRate.summary`: Calculate sampling rate for each column over a certain break (e.g. hour, min). ####

#### Usage ####

```r
SamplingRate.summary(sensorData, breaks = "min")
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification.
* `breaks`: "sec","min","hour","day","week","month","quarter" or "year"; or preceded by integer and space.


#### Value ####


 Returned dataframe would have headers: `HEADER_TIME_STAMP, SAMPLING_RATE`.


#### Note ####


 If "breaks" is missing, filter will be applied on the whole sequence and return a list with a single dataframe.


