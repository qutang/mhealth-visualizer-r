---
layout: doc
title: API Document
---

#### `FrequencyResponse.spectrum.ggplot`: Plot fft frequency response for input sensor data. ####

#### Usage ####

```r
FrequencyResponse.spectrum.ggplot(frData, scale = "normal",
resolution = 0.01)
```

#### Arguments ####

* `frData`: should be compatible with frequency response data format, with the first column be the frequencies, following by numeric columns
* `scale`: "normal" or "log", plot values in normal scale or log10 scale. Default is "normal".
* `resolution`: plot resolution for frequencies. If resolution is higher than the actual data resolution, it will do nothing, if resolution is lower than the actual data, it will skip some of the data points evenly. Default is 0.01Hz.

