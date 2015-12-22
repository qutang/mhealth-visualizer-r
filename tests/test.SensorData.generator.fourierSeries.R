
startTime = Sys.time()
endTime = startTime + 10
Fs = 40
range = 6
fbase = 1
ampMin = 1
ampMax = 1
phaseMin = 6
phaseMax = 6
seed = 6
fstep = 0.2
order = 20

result = SensorData.generator.fourierSeries(startTime = startTime,
                                   endTime = endTime,
                                   Fs = Fs,
                                   range = range,
                                   fbase = fbase,
                                   fstep = fstep,
                                   ampMin = ampMin,
                                   ampMax = ampMax,
                                   amp0 = 0,
                                   phaseMin = phaseMin,
                                   phaseMax = phaseMax,
                                   order = order,
                                   noiseStd = 0,
                                   parallel = FALSE,
                                   seed = seed
                                   )
SensorData.ggplot(result)
fftData = FrequencyResponse.fft(result, Fs = 40)