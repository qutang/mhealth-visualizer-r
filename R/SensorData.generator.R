MHEALTH_CSV_GENERATOR_SINUSOIDAL_SUFFIX = "SINUSOIDAL_SIMULATED_VALUE"

#' @name SensorData.generator.sinusoidal
#' @title Generate simulated sinusoidal signal in mhealth format
#' @description If the signal arguments are vector, multiple columns will be generated
#' @export
#' @import plyr
SensorData.generator.sinusoidal = function(startTime,
                                           endTime,
                                           Fs,
                                           range,
                                           f,
                                           amp,
                                           phase = 0,
                                           noiseStd,
                                           seed = 1){

  t = seq(from = startTime, to = endTime, by = 1/Fs);
  signalParams = as.data.frame(rbind(f, amp, phase, noiseStd, range))

  if(!missing(seed) && !is.null(seed)){
    set.seed(seed)
  }

  normalizedT = seq(from=0, to = length(t)/Fs, length.out = length(t))

  colGenerator = colwise(function(param){
    stream = param[2] * sin(2*pi*param[1]*normalizedT + param[3])
    stream[stream >= param[5]] = param[5]
    stream[stream <= -param[5]] = -param[5]
    stream = stream + rnorm(length(t), mean = 0, sd = param[4])
    return(stream)
  })

  result = colGenerator(signalParams)
  result = cbind(t, result)

  names(result) = c(MHEALTH_CSV_TIMESTAMP_HEADER,
                    paste(LETTERS[1:(ncol(result)-1)],
                          MHEALTH_CSV_GENERATOR_SINUSOIDAL_SUFFIX, sep = "_"))

  return(result)
}
