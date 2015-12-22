---
layout: doc
title: API Document
---

#### `SensorData.filter.bessel`: Apply low pass bessel filter to the input sensor data frame each column over certain breaks (e.g. hour, sec, min and etc.) ####

#### Usage ####

```r
SensorData.filter.bessel(sensorData, breaks, Fs, Fc, order)
```

#### Arguments ####

* `sensorData`: the input dataframe that matches mhealth specification.
* `breaks`: "sec","min","hour","day","week","month","quarter" or "year"; or preceded by integer and space.
* `Fs`: sampling rate of the input signal
* `Fc`: cut off frequency of bessel filter
* `order`: formula order of bessel filter


#### Note ####


 If "breaks" is missing, filter will be applied on the whole sequence and return a list with a single dataframe.


