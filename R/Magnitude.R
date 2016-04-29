#' @name Magnitude.compute
#' @title Compute the magnitude value of sensor data.
#' @export
#' @import plyr
#' @param sensorData input dataframe that matches specification.
#' @return dataframe with headers: `HEADER_CSV_TIMESTAMP,MAGNITUDE_[ORIGINAL_HEADER]`
Magnitude.compute = function(sensorData){
  nCols = ncol(sensorData)
  magnitudeData = sensorData
  stampName = colnames(sensorData)[1]
  tokens = str_split(names(sensorData)[2], "_")[[1]]
  labelName = paste(c("MAGNITUDE",tokens[2:length(tokens)]),collapse="_")
  magnitudeData[labelName] = rowSums(sensorData[,2:nCols]^2)
  magnitudeData[,labelName] = sqrt(magnitudeData[,labelName])
  magnitudeData = magnitudeData[, c(stampName, labelName)]
  return(magnitudeData)
}
