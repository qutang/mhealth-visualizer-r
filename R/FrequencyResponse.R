MHEALTH_CSV_FREQUENCY_HEADER = "HEADER_FREQUENCY_STAMP"
MHEALTH_CSV_FFT_MAGNITUDE_HEADER_PREFIX = "FFTMAGNITUDE"
MHEALTH_CSV_FFT_VECTOR_HEADER_PREFIX = "FFTVECTOR"

#' @name FrequencyResponse.fft
#' @title Compute frequency response for input sensor data.
#' @export
#' @import matlab
#' @param sensorData input dataframe that matches mhealth specification.
#' @param Fs sampling rate of the input signal.
#' @param type "magnitude" or "vector". "magnitude": magnitude value of fft results; "vector": vector of fft results.
#' @param normalize "summed", "average", "normalized", "unnormalized".
#' @return frequency dataframe with HEADER `HEADER_FREQUENCY_STAMP,[FFTMAGNITUDE/FFTVECTOR]_[ORIGINAL_HEADER]`.
FrequencyResponse.fft = function(sensorData, Fs, type = "magnitude", normalize = "normalized"){
  # input frequency response
  nRows = nrow(sensorData)
  nCols = ncol(sensorData)
  NFFT = 2^nextpow2(nRows);
  f = Fs/2*seq(0,1,length.out = NFFT/2+1);
  if(normalize == "summed"){
    fftData = mvfft(as.matrix(sensorData[,2:nCols]))/nRows;
  }else if(normalize == "average"){
    fftData = mvfft(as.matrix(sensorData[,2:nCols]))/nRows/nRows;
  }else if(normalize == "normalized"){
    fftData = mvfft(as.matrix(sensorData[,2:nCols]))/nRows*1/Fs / diff(f)[1];
  }else if(normalize == "unnormalized"){
    fftData = mvfft(as.matrix(sensorData[,2:nCols]));
  }
  
  switch(type,
         magnitude = {
           magfftData = abs(fftData[1:(NFFT/2+1),])
           result = data.frame(f, magfftData)
           for(i in 2:nCols){
             names(result)[i] = paste(MHEALTH_CSV_FFT_MAGNITUDE_HEADER_PREFIX, names(sensorData)[i],sep="_")
           }
         },
         vector = {
           vectorfftData = fftData[1:(NFFT/2+1),]
           result = data.frame(f, vectorfftData)
           for(i in 2:nCols){
             names(result)[i] = paste(MHEALTH_CSV_FFT_VECTOR_HEADER_PREFIX, names(sensorData)[i],sep="_")
           }
         }
  )
  names(result)[1] = MHEALTH_CSV_FREQUENCY_HEADER
  return(result)
}

#' @name FrequencyResponse.changeResolution
#' @title Change the frequency resolution of the input frequency response data
#' @description If resolution is higher than the actual data resolution, it will do nothing, if resolution is lower than the actual data, it will down sample the frequency data by skipping some of the data points evenly.
#' @export
#' @param frData should be compatible with frequency response data format, with the first column be the frequencies, following by numeric columns.
#' @param resolution new resolution for frequencies. Default is 0.01Hz.
FrequencyResponse.changeResolution = function(frData, resolution = 0.01){
  nRows = nrow(frData)
  actualResolution = frData[2, MHEALTH_CSV_FREQUENCY_HEADER] - frData[1, MHEALTH_CSV_FREQUENCY_HEADER]
  if(actualResolution < resolution){
    newFreqs = seq(1, nRows, by = floor(resolution / actualResolution))
    result = frData[newFreqs,]
  }else{
    warning("The actual resolution is lower than the set one, so no need to skip points")
    result = frData
  }
  return(result)
}

#' @name FrequencyResponse.spectrum.ggplot
#' @title Plot fft frequency response for input sensor data.
#' @export
#' @import ggplot2 stringr reshape2 ggthemes
#' @param frData should be compatible with frequency response data format, with the first column be the frequencies, following by numeric columns
#' @param scale "normal" or "log" or "db", plot values in normal scale or log10 or dB scale. Default is "normal".
#' @param resolution plot resolution for frequencies. If resolution is higher than the actual data resolution, it will do nothing, if resolution is lower than the actual data, it will skip some of the data points evenly. Default is 0.01Hz.
#' @param label if TRUE, show dominant frequency values.
FrequencyResponse.spectrum.ggplot = function(frData, scale = "normal", resolution = 0.01, label = TRUE){
  data = frData
  nCols = ncol(data)
  nRows = nrow(data)

  data = FrequencyResponse.changeResolution(data, resolution = resolution)

  labelNames = names(data[2:nCols])
  labelNames = c(str_match(labelNames, "[A-Za-z0-9]+_[A-Za-z0-9]+"))
  xlab = "frequency (Hz)"
  ylab = "value"

  titleText = paste("Frequency Response")

  data = melt(data, id = c(MHEALTH_CSV_FREQUENCY_HEADER))
  
  if(scale == "db"){data$value = 10 * log10(data$value)}

  p = ggplot(data = data, aes_string(x = MHEALTH_CSV_FREQUENCY_HEADER, y = "value", colour = "variable"))

  p = p + geom_line() + geom_point() +
    labs(title = titleText, x = xlab, y = ylab, colour = "type")

  switch(scale,
         log = {p = p + scale_y_log10()})

  p = p + theme_bw() + theme(legend.position="bottom")

  p

  return(p)
}
