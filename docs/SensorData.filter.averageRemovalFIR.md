---
layout: doc
title: API Document
---

#### `SensorData.filter.averageRemovalFIR`: Apply average removal FIR filter to the input sensor data frame each column over certain breaks (e.g. hour, sec, min and etc.) ####

#### Usage ####

```r
SensorData.filter.averageRemovalFIR(sensorData, breaks, Fs, order)
```

#### Arguments ####

* `sensorData`: the input dataframe that matches mhealth specification.
* `breaks`: "sec","min","hour","day","week","month","quarter" or "year"; or preceded by integer and space.
* `Fs`: sampling rate of the input signal
* `order`: window size (in seconds) of filter


#### Note ####


 If "breaks" is missing, filter will be applied on the whole sequence and return a list with a single dataframe.


