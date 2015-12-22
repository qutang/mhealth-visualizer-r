---
layout: doc
title: API Document
---

#### `SensorData.merge`: Merge two or more mhealth data frames by rows and sorted by timestamp, duplicated rows will be removed based on timestamp. ####

#### Usage ####

```r
SensorData.merge(listOfData, ...)
```

#### Arguments ####

* `listOfData`: list of input dataframes that matches mhealth specification.
* `...`: other optional input dataframes that matches mhealth specification.


#### Note ####


 Make sure that the data frame is including timestamps.


