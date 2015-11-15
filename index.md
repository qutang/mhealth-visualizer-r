---
layout: launch
title: mhealthformat-support-r
subtitle: (mhealthformatsupportr in R repository)
description: An R package that provides IO, preprocessing, manipulation, visualization functions and shiny apps to support data mining for mhealth data stored in mHealth data format.
bug-url: 
code-url:
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

# Quick start

## Four data format conventions
See more detail in mhealth format document.

### SensorData

## Interactive data visualizer (shiny app)

``` r
Mhealthinteractivevisualizer.run()
```
![screenshot]()

## Examples

### Load and summary data

The follow script will load sensor data (could be multi-column numeric values), compute mean for each column every __second__ and visualize the result nicely.

``` r
library(mhealthformatsupportr)
sensorData <- SensorData.importCsv(filename)
summaryMean <- SummaryData.simpleMean(sensorData, breaks = "min")
SummaryData.ggplot(summaryMean)
```

Currently, summary data supports 