---
layout: doc
title: API Document
---

#### `AnnotationData.getLabelNames`: get all matched label names given a timestamp, return NULL if no match. ####

#### Usage ####

```r
AnnotationData.getLabelNames(annotationData, currentTime)
```

#### Arguments ####

* `annotationData`: input annotation dataframe that matches mhealth specification
* `currentTime`: POSIXct date object of a timestamp to search for the corresponding labels.


