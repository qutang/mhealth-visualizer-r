---
layout: doc
title: API Document
---

#### `SensorData.generator.fourierSeries`: Generate simulated fourier series signal in mhealth format to simulate arbitrary periodical signal. ####

#### Description ####


 frequency component will be generated with formula: `n * fstep * fbase`. `n` is nth order. Amplitude and phase will have uniform distribution between min and max values. Values will be cut off if beyonding the dynamic range.


#### Usage ####

```r
SensorData.generator.fourierSeries(startTime, endTime, Fs, range, fbase, fstep,
ampMin = 0, ampMax = 1, amp0 = 0, phaseMin = 0, phaseMax = 1,
order = 3, noiseStd, parallel = FALSE, seed = 1)
```

#### Arguments ####

* `startTime`: POSIXct date object for start timestamp.
* `endTime`: POSIXct date object for end timestamp, often with format startTime + numeric duration.
* `Fs`: sampling rate of the simulated device.
* `range`: dynamic range of the simulated device.
* `fbase`: simulated base frequency of the generated signal.
* `fstep`: simulated frequency step parameter of the generated signal.
* `ampMin`: minimum value of generated amplitude.
* `ampMax`: maximum value of generated amplitude.
* `amp0`: the DC component of the generated signal.
* `phaseMin`: the minimum initial phase of the generated signal.
* `phaseMax`: the maximum initial phase of the generated signal.
* `order`: the order of frequency components of the generated signal.
* `noiseStd`: the standard deviation of the noise of the generated signal.
* `parallel`: whether to use parallel computing when generating signal.
* `seed`: the random seed to be used when adding noise.


#### Note ####


 If arguments (range, fbase, fstep, ampMin, ampMax, phaseMin, phaseMax, noiseStd) are vectors, multiple columns will be generated accordingly. But the vectors should have the same length.


