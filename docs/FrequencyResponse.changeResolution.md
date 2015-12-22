---
layout: doc
title: API Document
---

#### `FrequencyResponse.changeResolution`: Change the frequency resolution of the input frequency response data ####

#### Description ####


 If resolution is higher than the actual data resolution, it will do nothing, if resolution is lower than the actual data, it will down sample the frequency data by skipping some of the data points evenly.


#### Usage ####

```r
FrequencyResponse.changeResolution(frData, resolution = 0.01)
```

#### Arguments ####

* `frData`: should be compatible with frequency response data format, with the first column be the frequencies, following by numeric columns.
* `resolution`: new resolution for frequencies. Default is 0.01Hz.


