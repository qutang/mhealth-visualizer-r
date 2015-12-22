---
layout: doc
title: API Document
---

#### `SamplingRate.plot`: Plot sampling rate over certain breaks (e.g. min, hour). ####

#### Usage ####

```r
SamplingRate.plot(sr_dat, unit = "Count", ref)
```

#### Arguments ####

* `sr_dat`: sampling rate dataframe from `SamplingRate.summary`.
* `unit`: "Count" or "Hz" or "Dataloss": "Count" will display the sample counts; "Hz" will display the sampling rate in Hz (count divided by break time)"; Dataloss" will display the sampling rate in data loss percentage (count divided by reference).
* `ref`: reference value to be plotted as dashed line if not missing; or used to calculate data loss percentage.


