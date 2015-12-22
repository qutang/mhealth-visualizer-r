---
layout: doc
title: API Document
---

#### `AnnotationData.clip`: Clip annotation data according to the start and end time ####

#### Usage ####

```r
AnnotationData.clip(annotationData, startTime, endTime)
```

#### Arguments ####

* `annotationData`: annotation data frame that matches mhealth specification.
* `startTime`: POSIXct date object for start timestamp.
* `endTime`: POSIXct date object for start timestamp.


#### Note ####


 Make sure that the data frame is compatible with mhealth annotation data file format.


