---
layout: doc
title: API Document
---

#### `AnnotationData.addToGgplot`: add annotation bars to an existing ggplot (most likely a sensor data plot) or if no existing ggplot object is provided, create an annotation graph. ####

#### Usage ####

```r
AnnotationData.addToGgplot(p, annotationData)
```

#### Arguments ####

* `p`: an existing ggplot object. Often to be the one with sensor data.
* `annotationData`: input annotation dataframe that matches mhealth specification.


