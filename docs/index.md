---
title: "<code>mhealthformat-support-r</code> Package Document"
layout: doc
author: "Qu Tang"
date: "2016-01-28"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

mhealth Specification
=====================

`SensorData`
------------

Check out [mhealth specification](http://mhealth-specification.s3-website-us-east-1.amazonaws.com/), for detailed description.

<table class="table table-bordered table-responsive">
    <tr>
        <th>File name</th>
        <td><code>SensorType.SensorId.yyyy-mm-dd-hh-MM-ss-SSS.sensor.csv.(gz)<code></td>
    </tr>
    <tr>
        <th>CSV format</th>
        <td><a href="{{ "/data/samples/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150881.2015-11-02-15-00-00-000-M0500.sensor.csv" | prepend: site.baseurl }}">Sample sensor data csv file</a></td>
    </tr>
    <tr>
        <th>Directory convention</th>
        <td><code>MasterSynced/yyyy/mm/dd/hh/</code>. If storing in mhealth directory, sensor data should be split into hourly files.</td>
    </tr>

</table>
### Extensions of `SensorData`

#### `SummaryData`

<table class="table table-bordered table-responsive">
    <tr>
        <th>Description</th>
        <td>Describing time domain characteristics over a period of time for <code>SensorData</code>.</td>
    </tr>
    <tr>
        <th>Recommended headers</th>
        <td><code>HEADER_TIME_STAMP, [SUMMARY_METHOD]_RECOMMENDED_SENSOR_DATA_TYPE_HEADER, ...</code></td>
    </tr>
    <tr>
        <th>Note</th>
        <td>APIs for <code>SensorData</code> can also be applied on <code>SummaryData</code>.</td>
    </tr>

</table>
##### Operations (APIs)

-   Methods: [SummaryData.absoluteMean](SummaryData.absoluteMean.html), [SummaryData.auc](SummaryData.auc.html), [SummaryData.simpleMean](SummaryData.simpleMean.html)
-   Visualization: [SummaryData.ggplot](SummaryData.ggplot.html)

#### `FrequencyResponse`

<table class="table table-bordered table-responsive">
    <tr>
        <th>Description</th>
        <td>Frequency domain representation for <code>SensorData</code></td>
    </tr>
    <tr>
        <th>Recommended headers</th>
        <td><code>HEADER_FREQUENCY_STAMP, [FREQUENCY_TRANSFORM_METHOD]_RECOMMENDED_SENSOR_DATA_TYPE_HEADER, ...</code></td>
    </tr>

</table>
##### Operations (APIs)

-   Methods: [FrequencyResponse.changeResolution](FrequencyResponse.changeResolution.html), [FrequencyResponse.fft](FrequencyResponse.fft.html)
-   Visualization: [FrequencyResponse.spectrum.ggplot](FrequencyResponse.spectrum.ggplot.html)

#### `SamplingRate`

<table class="table table-bordered table-responsive">
    <tr>
        <th>Description</th>
        <td>Representation of sampling rate as a special type of <code>SummaryData</code> of <code>SensorData</code></td>
    </tr>
    <tr>
        <th>Recommended headers</th>
        <td><code>HEADER_TIME_STAMP, SAMPLING_RATE_PER_[INTERVAL]</code></td>
    </tr>
    <tr>
        <th>Note</th>
        <td>APIs for <code>SensorData</code> can also be applied on <code>SamplingRate</code>.</td>
    </tr>

</table>
##### Operations (APIs)

-   Methods: [SamplingRate.summary](SamplingRate.summary.html)
-   Visualization: [SamplingRate.ggplot](SamplingRate.ggplot.html)

#### `Magnitude`

<table class="table table-bordered table-responsive">
    <tr>
        <th>Description</th>
        <td>Representation of vector magnitude as a special type of <code>SensorData</code></td>
    </tr>
    <tr>
        <th>Recommended headers</th>
        <td><code>HEADER_TIME_STAMP, MAGNITUDE_RECOMMENDED_SENSOR_DATA_TYPE_HEADER</code></td>
    </tr>
    <tr>
        <th>Note</th>
        <td>APIs for <code>SensorData</code> can also be applied on <code>SamplingRate</code>.</td>
    </tr>

</table>
##### Operations (APIs)

-   Methods: [Magnitude.compute](Magnitude.compute.html)

### Operations (API documents)

-   IO: [SensorData.importCsv](SensorData.importCsv.html), [SensorData.importActigraphCsv](SensorData.importActigraphCsv.html), [SensorData.importBinary](SensorData.importBinary.html), [SensorData.importGT3X](SensorData.importGT3X.html), [SensorData.io.write](SensorData.io.write.html)
-   Split and merge: [SensorData.split](SensorData.split.html), [SensorData.merge](SensorData.merge.html), [SensorData.clip](SensorData.clip.html)
-   Clean up: [SensorData.cleanup](SensorData.cleanup.html)
-   Process: [SensorData.filter.bessel](SensorData.filter.bessel.html), [SensorData.filter.butterworth](SensorData.filter.butterworth.html), [SensorData.extrapolate](SensorData.extrapolate.html)
-   Generator: [SensorData.generator.sinusoidal](SensorData.generator.sinusoidal.html), [SensorData.generator.fourierSeries](SensorData.generator.fourierSeries.html)
-   Visualization: [SensorData.ggplot](SensorData.ggplot.html)

`AnnotationData`
----------------

<table class="table table-bordered table-responsive">
    <tr>
        <th>File name</th>
        <td><code>OntologyId.AnnotatorId.yyyy-mm-dd-hh-MM-ss-SSS.annotation.csv.(gz)<code></td>
    </tr>
    <tr>
        <th>CSV format</th>
        <td><a href = "{{"/data/samples/SPADESInLab.al.2015-11-02-15-00-00-000-M0500.annotation.csv" | prepend: site.baseurl }}">Sample annotation data csv file</a></td>
    </tr>
    <tr>
        <th>Directory convention</th>
        <td><code>MasterSynced/yyyy/mm/dd/hh/</code>. If storing in mhealth directory, annotation data should be split into hourly files.</td>
    </tr>

</table>
### Operations (API documents)

-   IO: [AnnotationData.importCsv](AnnotationData.importCsv.html)
-   Split and merge: [AnnotationData.merge](AnnotationData.merge.html), [AnnotationData.clip](AnnotationData.clip.html)
-   Process: [AnnotationData.getLabelNames](AnnotationData.getLabelNames.html)
-   Visualization: [AnnotationData.addToGgplot](AnnotationData.addToGgplot.html)

`EventData`
-----------

<table class="table table-bordered table-responsive">
    <tr>
        <th>File name</th>
        <td><code>EventType.EventId.yyyy-mm-dd-hh-MM-ss-SSS.event.csv.(gz)<code></td>
    </tr>
    <tr>
        <th>CSV format</th>
        <td><a href = "{{"/data/samples/Battery.532929050178941.2015-11-02-15-00-26-737-M0500.event.csv" | prepend: site.baseurl }}">Sample event data csv file</a></td>
    </tr>
    <tr>
        <th>Directory convention</th>
        <td><code>MasterSynced/yyyy/mm/dd/hh/</code>. If storing in mhealth directory, event data should be split into hourly files.</td>
    </tr>

</table>
[API index](api.html)
=====================
