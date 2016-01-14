---
layout: doc
title: API Document
---

#### `SensorData.offset`: offset sensor data's timestamp by an offset value in seconds ####

#### Usage ####

```r
SensorData.offset(sensorData, offsetValue = 0)
```

#### Arguments ####

* `sensorData`: input dataframe that matches mhealth specification.
* `offsetValue`: value in seconds specifies the offset time, could be negative, meaning go back to some time earlier. The default is 0, meaning no offset.


#### Value ####


 dataframe after timestamps being offset


