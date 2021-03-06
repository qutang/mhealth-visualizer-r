% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SensorData.R
\name{SensorData.clip}
\alias{SensorData.clip}
\title{Clip sensor data according to the start and end time.}
\usage{
SensorData.clip(sensorData, startTime, endTime)
}
\arguments{
\item{sensorData}{input dataframe that matches mhealth specification.}

\item{startTime}{POSIct date object for start time.}

\item{endTime}{POSIct date object for end time.}
}
\note{
Make sure that the data frame is including timestamps.
}
