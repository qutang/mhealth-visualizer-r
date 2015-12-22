---
layout: doc
title: API Document
---

#### `SummaryData.absoluteMean`: Calculate summary value (absolute mean value) for each column over a certain break (e.g. hour, min). ####

#### Usage ####

```r
SummaryData.absoluteMean(sensorData, breaks)
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth sensor data format.
* `breaks`: could be "sec", "min", "hour", "day", "week", "month", "quarter" or "year"; or preceded by an interger and a space.


#### Note ####


 If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME.


