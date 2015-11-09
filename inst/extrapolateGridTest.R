require(reshape2)
require(ggplot2)
startTime = Sys.time()
duration = 30
Fs = 40
range = 6
freqs = seq(1, 10, by = 0.5)
freqs = 2
amp = seq(3, 16, by = 1)
amp = c(4,8)
noiseStds = 0
seed = 5
lambda = 2
result = data.frame(freq = c(), noiseStd = c(), extrapolationError = c(), inputError = c())
for(f in freqs){
  for(noiseStd in noiseStds){
    # Generate batch of sin signals
    inputBatch = SensorData.generator.sinusoidal(startTime = startTime,
                                    endTime = startTime + duration,
                                    Fs = Fs,
                                    range = range,
                                    f = f,
                                    amp = amp,
                                    phase = 0,
                                    noiseStd = noiseStd,
                                    seed = 5
    )
    gtBatch = SensorData.generator.sinusoidal(startTime = startTime,
                                              endTime = startTime + duration,
                                              Fs = Fs,
                                              range = amp,
                                              f = f,
                                              amp = amp,
                                              phase = 0,
                                              noiseStd = noiseStd,
                                              seed = 5
    )
    extrapolatedBatch = SensorData.extrapolate(inputBatch,
                           lambda = lambda,
                           interpolate = "spline_natural",
                           range = range,
                           noiseStd = noiseStd)
    extrapolatedCounts = as.numeric(SummaryData.absoluteMean(extrapolatedBatch))
    inputCounts = as.numeric(SummaryData.absoluteMean(inputBatch))
    gtCounts = as.numeric(SummaryData.absoluteMean(gtBatch))
    extrapolationError = abs(extrapolatedCounts - gtCounts)/gtCounts
    inputError = abs(inputCounts- gtCounts)/gtCounts
    result = rbind(result, data.frame(freq = f, noiseStd = noiseStd, extrapolationError = extrapolationError, inputError = inputError))
  }
}
print(result)
melted = melt(result, c("freq", "noiseStd"))
ggplot(melted, aes(x=freq, y=noiseStd)) + geom_tile(aes(fill=value)) + scale_fill_gradient(low = "white",high = "steelblue")
