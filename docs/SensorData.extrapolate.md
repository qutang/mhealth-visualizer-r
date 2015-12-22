---
layout: doc
title: API Document
---

#### `SensorData.extrapolate`: Apply extrapolate algorithm to sensor data. Algorithm was developed for activity count by NU mhealth group. ####

#### Usage ####

```r
SensorData.extrapolate(sensorData, lambda = 1, interpolate, range, noiseStd)
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification.
* `lambda`: parameter to regularize extrapolation: 0.1-10; default is 1. (the smaller the more confidence on points close to edges).
* `interpolate`: "linear", "spline_fmm", "spline_natural", "aspline_original", "aspline_improved".
* `range`: dynamic range.
* `noiseStd`: standard deviation of the white noise.


#### Author ####


 Qu Tang


