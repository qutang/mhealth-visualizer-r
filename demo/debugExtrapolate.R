startTime = Sys.time()
testData = SensorData.generator.sinusoidal(startTime, startTime + 30, Fs = 40, range = 6, f = 5, amp = 14, phase = 0, noiseStd = 0)
SensorData.extrapolate(testData, lambda = 3, interpolate = "spline_natural", range = 6, noiseStd = 0)