---
layout: doc
title: API Document
---

#### `SensorData.generator.sinusoidal`: Generate simulated sinusoidal signal in mhealth format. ####

#### Usage ####

```r
SensorData.generator.sinusoidal(startTime, endTime, Fs, range, f, amp,
amp0 = 0, phase = 0, noiseStd, seed = 1)
```

#### Arguments ####

* `startTime`: POSIXct date object for start timestamp.
* `endTime`: POSIXct date object for end timestamp, often with format startTime + numeric duration.
* `Fs`: sampling rate of the simulated device.
* `range`: dynamic range of the simulated device.
* `f`: simulated frequency of the generated signal.
* `amp`: amplitude of the generated signal.
* `amp0`: the DC component of the generated signal.
* `phase`: the initial phase of the generated signal.
* `noiseStd`: the standard deviation of the noise of the generated signal.
* `seed`: the random seed to be used when adding noise.


#### Note ####


 If arguments (range, f, amp, phase, noiseStd) are vectors, multiple columns will be generated accordingly. But the vectors should have the same length. Values will be cut off if beyonding the dynamic range.


