
startTime = Sys.time()
duration = 30
Fs = 40
range = 6
freqs = seq(1, 10, by = 0.5)
amp = seq(3, 16, by = 0.5)
noiseStds = seq(0.01, 0.5, by = 0.05)
seed = 5
lambda = 2
result = data.frame(freq = c(), noiseStd = c(), error = c())
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
    gtCounts = as.numeric(SummaryData.absoluteMean(gtBatch))
    error = (extrapolatedCounts - gtCounts)/gtCounts
    meanErr = mean(abs(error))
    result = rbind(result, data.frame(freq = f, noiseStd = noiseStd, error = meanErr))
  }
}
print(result)

