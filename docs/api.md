---
layout: doc
title: API Document
---

 * [`AnnotationData.addToGgplot`](AnnotationData.addToGgplot.html) add annotation bars to an existing ggplot (most likely a sensor data plot) or if no existing ggplot object is provided, create an annotation graph.

 * [`AnnotationData.clip`](AnnotationData.clip.html) Clip annotation data according to the start and end time

 * [`AnnotationData.getLabelNames`](AnnotationData.getLabelNames.html) get all matched label names given a timestamp, return NULL if no match.

 * [`AnnotationData.importCsv`](AnnotationData.importCsv.html) Import mhealth annotation data file and load into memory as data frame in mhealth format.

 * [`AnnotationData.merge`](AnnotationData.merge.html) merge two or more annotation data frames and sort according to start time

 * [`AnnotationData.offset`](AnnotationData.offset.html) offset annotation data's start and stop timestamp by an offset value in seconds

 * [`FrequencyResponse.changeResolution`](FrequencyResponse.changeResolution.html) Change the frequency resolution of the input frequency response data

 * [`FrequencyResponse.spectrum.ggplot`](FrequencyResponse.spectrum.ggplot.html) Plot fft frequency response for input sensor data.

 * [`Magnitude.compute`](Magnitude.compute.html) Compute the magnitude value of sensor data.

 * [`MhealthInteractiveVisualizer.getApp`](MhealthInteractiveVisualizer.getApp.html) The shiny app object of interactive visualizer for mhealth format data

 * [`MhealthInteractiveVisualizer.run`](MhealthInteractiveVisualizer.run.html) An interactive visualizer for mhealth format data based on shiny

 * [`SamplingRate.ggplot`](SamplingRate.ggplot.html) Plot sampling rate over certain breaks (e.g. min, hour) using ggplot2.

 * [`SamplingRate.plot`](SamplingRate.plot.html) Plot sampling rate over certain breaks (e.g. min, hour).

 * [`SamplingRate.summary`](SamplingRate.summary.html) Calculate sampling rate for each column over a certain break (e.g. hour, min).

 * [`SensorData.cleanup`](SensorData.cleanup.html) Clean up sensor dataframe by removing invalid timestamps, according to a certain time level.

 * [`SensorData.clip`](SensorData.clip.html) Clip sensor data according to the start and end time.

 * [`SensorData.extrapolate`](SensorData.extrapolate.html) Apply extrapolate algorithm to sensor data. Algorithm was developed for activity count by NU mhealth group.

 * [`SensorData.extrapolate.visualize`](SensorData.extrapolate.visualize.html) visualizing function for viewing the mediate results of the extrapolation algorithm

 * [`SensorData.filter.bessel`](SensorData.filter.bessel.html) Apply low pass bessel filter to the input sensor data frame each column over certain breaks (e.g. hour, sec, min and etc.)

 * [`SensorData.filter.butterworth`](SensorData.filter.butterworth.html) Apply high pass butterworth filter to the input sensor data frame each column over a certain break (e.g. hour, sec, min and etc.).

 * [`SensorData.generator.fourierSeries`](SensorData.generator.fourierSeries.html) Generate simulated fourier series signal in mhealth format to simulate arbitrary periodical signal.

 * [`SensorData.generator.sinusoidal`](SensorData.generator.sinusoidal.html) Generate simulated sinusoidal signal in mhealth format.

 * [`SensorData.ggplot`](SensorData.ggplot.html) Plot sensor raw data using ggplot2.

 * [`SensorData.importActigraphCsv`](SensorData.importActigraphCsv.html) Import and convert Actigraph raw csv files and load into data frame as in mhealth format.

 * [`SensorData.importBinary`](SensorData.importBinary.html) Import and decode binary file from the smart watch and load into dataframe as mhealth format.

 * [`SensorData.importCsv`](SensorData.importCsv.html) Import mhealth sensor data file and load into memory as data frame in mhealth format.

 * [`SensorData.importGT3X`](SensorData.importGT3X.html) Import and decode GT3X files and load into dataframe as mhealth format.

 * [`SensorData.interpolate`](SensorData.interpolate.html) Interpolate the missing points and unify sampling interval for the input sensor data

 * [`SensorData.io.write`](SensorData.io.write.html) Write sensor data into mhealth folder structure and with mhealth filename convention.

 * [`SensorData.merge`](SensorData.merge.html) Merge two or more mhealth data frames by rows and sorted by timestamp, duplicated rows will be removed based on timestamp.

 * [`SensorData.offset`](SensorData.offset.html) offset sensor data's timestamp by an offset value in seconds

 * [`SensorData.plot`](SensorData.plot.html) Plot nicely the raw sensor data data frame.

 * [`SensorData.split`](SensorData.split.html) Split sensor data into list of smaller data frame with meaningful intervals (e.g. hourly, minutely, secondly or daily)

 * [`SummaryData.absoluteMean`](SummaryData.absoluteMean.html) Calculate summary value (absolute mean value) for each column over a certain break (e.g. hour, min).

 * [`SummaryData.auc`](SummaryData.auc.html) Calculate summary value (area under curve) for each column over a certain break (e.g. hour, min).

 * [`SummaryData.ggplot`](SummaryData.ggplot.html) Plot summary data using ggplot2

 * [`SummaryData.simpleMean`](SummaryData.simpleMean.html) Calculate summary value (mean value) for each column over a certain time break (e.g. hour, min)

