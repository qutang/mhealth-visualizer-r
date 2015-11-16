---
layout: launch
title: mhealthformat-support-r
subtitle: (mhealthformatsupportr in R repository)
description: An R package that provides IO, preprocessing, manipulation, visualization functions and shiny apps to support data mining for mhealth data stored in mHealth data format.
bug-url: 'https://github.com/qutang/mhealthformat-support-r/issues'
code-url: 'https://github.com/qutang/mhealthformat-support-r'
doc-url:
---

# Installation

1. Make sure to have `R (>= 3.2.1)` and `Java 7 (JVM or JDK)` installed. (Recommand to use lastest version of `RStudio`)

2. Install package `devtools` in `R`

``` r
install_package('devtools')
```

3. Install `mhealthformat-support-r` through __github__

``` r
devtools::install_github('qutang/mhealthformat-support-r')
```

---
# Recap of mhealth format

See more detail in mhealth format document.

### `SensorData`

<table class="table table-bordered table-responsive">
    <tr>
        <th>File name</th>
        <td><code>SensorType.SensorId.yyyy-mm-dd-hh-MM-ss-SSS.sensor.csv.(gz)<code></td>
    </tr>
    <tr>
        <th>CSV format</th>
        <td><pre>HEADER_TIME_STAMP,CUSTOM_HEADER_1,CUSTOM_HEADER_2,...
2015-11-13 13:11:43.231,0.3514,0.3262,0.5432,...
...</pre></td>
    </tr>
    <tr>
        <th>Directory convention</th>
        <td><code>MasterSynced/yyyy/mm/dd/hh/</code>. If storing in mhealth directory, sensor data should be split into hourly files.</td>
    </tr>
    <tr>
		<th>Note</th>
        <td colspan="3"><code>SummaryData</code> follows the same CSV format as <code>SensorData</code>.</td>
    </tr>
</table>

### `AnnotationData`

<table class="table table-bordered table-responsive">
    <tr>
        <th>File name</th>
        <td><code>OntologyId.AnnotatorId.yyyy-mm-dd-hh-MM-ss-SSS.annotation.csv.(gz)<code></td>
    </tr>
    <tr>
        <th>CSV format</th>
        <td><pre>HEADER_TIME_STAMP,START_TIME,STOP_TIME,LABEL,RATING_TIMESTAMP,RATING,...
2015-11-13 13:11:43.231,2015-11-13 13:11:43.231,2015-11-13 13:11:53.000,Walking,2015-11-13 13:11:43.231,,...
...</pre></td>
    </tr>
    <tr>
        <th>Directory convention</th>
        <td><code>MasterSynced/yyyy/mm/dd/hh/</code>. If storing in mhealth directory, annotation data should be split into hourly files.</td>
    </tr>
</table>

### `EventData`

<table class="table table-bordered table-responsive">
    <tr>
        <th>File name</th>
        <td><code>EventType.EventId.yyyy-mm-dd-hh-MM-ss-SSS.event.csv.(gz)<code></td>
    </tr>
    <tr>
        <th>CSV format</th>
        <td><pre>HEADER_TIME_STAMP,START_TIME,STOP_TIME,DETAIL,CUSTOM_FIELDS,...
2015-11-13 13:11:43.231,2015-11-13 13:11:43.231,,Wifi On,...
...</pre>`DETAIL` column is optional, but the first three columns should be fixed.</td>
    </tr>
    <tr>
        <th>Directory convention</th>
        <td><code>MasterSynced/yyyy/mm/dd/hh/</code>. If storing in mhealth directory, event data should be split into hourly files.</td>
    </tr>
</table>

---
# Quick start

## Interactive data visualizer (shiny app)

``` r
Mhealthinteractivevisualizer.run()
```

---
# Code Examples

### Load and summary sensor data

The follow script will load sensor data, compute mean for each column every __minute__ and visualize the result nicely.

``` r
library(mhealthformatsupportr)
# Load sensor data
sensorData <- SensorData.importCsv(sensorFile)
# Compute mean for each column every minute
summaryMean <- SummaryData.simpleMean(sensorData, breaks = "min")
# Plot the computed summary mean
SummaryData.ggplot(summaryMean)
```

### Visualize summary data with annotations

The follow script will load sensor data, compute mean for each column every __minute__ and visualize the result nicely, and load corresponding annotation data and add annotations to the summary data plot.

``` r
library(mhealthformatsupportr)
# Load sensor data
sensorData <- SensorData.importCsv(sensorFile)
# Compute mean for each column every minute
summaryMean <- SummaryData.simpleMean(sensorData, breaks = "min")
# Plot the computed summary mean
p <- SummaryData.ggplot(summaryMean)
# Load annotation data
annotationData = AnnotationData.importCsv(annotationFile)
# Add annotation onto the summary data Plot
AnnotationData.addToGgplot(p, annotationData)
```

