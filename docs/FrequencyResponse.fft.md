---
layout: doc
title: API Document
---

#### `FrequencyResponse.fft`: Compute frequency response for input sensor data. ####

#### Usage ####

```r
FrequencyResponse.fft(sensorData, Fs, type = "magnitude")
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification.
* `Fs`: sampling rate of the input signal.
* `type`: "magnitude" or "vector". "magnitude": magnitude value of fft results; "vector": vector of fft results.


#### Value ####


 frequency dataframe with HEADER `HEADER_FREQUENCY_STAMP,[FFTMAGNITUDE/FFTVECTOR]_[ORIGINAL_HEADER]`.


