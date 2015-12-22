---
layout: doc
title: API Document
---

#### `SensorData.cleanup`: Clean up sensor dataframe by removing invalid timestamps, according to a certain time level. ####

#### Usage ####

```r
SensorData.cleanup(sensorData, level = "year", ref = NULL)
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification
* `level`: "second", "minute", "hour", "day", "month", or "year"; used to trim data that doesn't match the reference value.
* `ref`: the reference date string for the certain time "level". E.g. "2015-10", for level "month"; the string format should follow " %Y-%m-%d %H:%M:%S".} 


