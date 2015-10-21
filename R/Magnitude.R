#' @name Magnitude.compute
#' @title Compute the magnitude value of sensor data
#' @export
#' @import plyr

Magnitude.compute = function(sensorData){
  nCols = ncol(sensorData)
  magnitudeData = sensorData
  tokens = str_split(names(sensorData)[2], "_")[[1]]
  labelName = paste(c("MAGNITUDE",tokens[2:length(tokens)]),collapse="_")
  magnitudeData[labelName] = rowSums(sensorData[,2:nCols]^2,na.rm = TRUE)
  magnitudeData[,labelName] = sqrt(magnitudeData[,labelName])
  magnitudeData = magnitudeData[, c(MHEALTH_CSV_TIMESTAMP_HEADER, labelName)]
  return(magnitudeData)
}
