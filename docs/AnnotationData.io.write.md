---
layout: doc
title: API Document
---

#### `AnnotationData.io.write`: Write annotation data into mhealth folder structure and with mhealth filename convention. ####

#### Usage ####

```r
AnnotationData.io.write(folder, annotationData, ontologyId = NA,
annotatorId = NA, tz, gzip = TRUE, flatDir = FALSE, splitHour = TRUE,
custom_name, append = FALSE, header = TRUE)
```

#### Arguments ####

* `folder`: the output folder
* `annotationData`: the input dataframe that matches mhealth specification.
* `ontologyId`: the ontology ID for this set of annotations
* `annotatorId`: the annotator's ID for this set of annotations
* `tz`: the time zone string (P/MHHMM) used in filename.
* `gzip`: whether to gzip the output csv file.
* `flatDir`: whether to use mhealth folder structure or just use flat directory.
* `splitHour`: whether to split input dataframe into hourly csv files.
* `custom_name`: if provided, the file name will be custom_name and all other file name preset parameters such as sensorType will be discarded.
* `append`: whether to append to a file if the file exists
* `header`: whether to add column header or not


