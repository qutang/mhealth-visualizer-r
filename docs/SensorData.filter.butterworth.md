---
layout: doc
title: API Document
---

#### `SensorData.filter.butterworth`: Apply high pass butterworth filter to the input sensor data frame each column over a certain break (e.g. hour, sec, min and etc.). ####

#### Usage ####

```r
SensorData.filter.butterworth(sensorData, breaks, Fs, Fc, order)
```

#### Arguments ####

* `sensorData`: the input dataframe that matches mhealth specification.
* `breaks`: "sec","min","hour","day","week","month","quarter" or "year"; or preceded by integer and space.
* `Fs`: sampling rate of the input signal
* `Fc`: cut off frequency of butterworth filter
* `order`: formula order of butterworth filter


#### Value ####


 list of filtered dataframes.


#### Note ####


 If "breaks" is missing, filter will be applied on the whole sequence and return a list with a single dataframe.


