---
layout: doc
title: API Document
---

#### `AnnotationData.offset`: offset annotation data's start and stop timestamp by an offset value in seconds ####

#### Usage ####

```r
AnnotationData.offset(annotationData, offsetValue = 0)
```

#### Arguments ####

* `annotationData`: annotation dataframe that matches mhealth specification.
* `offsetValue`: value in seconds specifies the offset time, could be negative, meaning go back to some time earlier. The default is 0, meaning no offset.


#### Value ####


 annotation dataframe after timestamps being offset.


