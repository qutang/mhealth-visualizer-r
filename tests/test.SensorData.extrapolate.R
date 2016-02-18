require(plyr)
# Generate input signal and groundtruth
set.seed(5)
duration = 2
Fs = 100
range = 3
freqs = 5
amp = 10
noiseStds = 0
phase = runif(1,min=0,max = 10)
seed = 5
lambda = 1
parallel = TRUE
interpolate = "spline_natural"
startTime = strptime("2015-01-01 00:00:30", format = "%Y-%m-%d %H:%M:%OS")

input = SensorData.generator.sinusoidal(startTime, startTime + duration, Fs = Fs,range = range, f = freqs,
                                amp = amp, phase = phase, amp0 = 0, noiseStd = noiseStds, seed = seed)

gt = SensorData.generator.sinusoidal(startTime, startTime + duration, Fs = Fs, range = amp, f = freqs,
                                     amp = amp, phase = phase, amp0 = 0, noiseStd = noiseStds, seed = seed)

result = SensorData.extrapolate.singleColumn(input,c(lambda, interpolate, range, noiseStds))
extra = result$output[[2]]
extra = cbind(input[[1]], extra)
names(extra) = names(input)

markedMaxedOut = result$markedOriginal$maxedOutPoints
maxedOutRegion = {}
maxedOutRegion$markedMaxedOutPoints = markedMaxedOut
maxedOutRegion$threshold = c(result$markedOriginal$lbound, result$markedOriginal$ubound)
maxedOutRegion$edgePairs = result$edgePairs
regressionResults = {}
regressionResults$intersectPoints = result$intersectPoints
regressionResults$regressionNeighbors = result$allNeighbors
regressionResults$regressionLines = result$fittedResults

#' STEP 1: Find the rough maxed out region and its middle points
SensorData.extrapolate.visualize(extra, input, gt, maxedOutRegion=maxedOutRegion,
                                 regressionResults = regressionResults,
                                 showInput = TRUE, showGroundTruth = FALSE,
                                showOutput = FALSE, showMarker = TRUE, showThreshold = TRUE,
                                showCenter = TRUE, showPoints = FALSE)

#' STEP 2: Track from the middle point to find the neighbor points on both sides
SensorData.extrapolate.visualize(extra, input, gt, maxedOutRegion=maxedOutRegion,
                                 regressionResults = regressionResults,
                                 showInput = TRUE, showGroundTruth = FALSE,
                                 showOutput = FALSE, showMarker = FALSE, showThreshold = TRUE,
                                 showCenter = TRUE, showPoints = FALSE, showNeighbors = c(5, 7))

#' STEP 3: Weighing neighbor points
#' STEP 4: Do weighted linear regression on neighbor points on each side
SensorData.extrapolate.visualize(extra, input, gt, maxedOutRegion=maxedOutRegion,
                                 regressionResults = regressionResults,
                                 showInput = TRUE, showGroundTruth = TRUE,
                                 showOutput = FALSE, showMarker = FALSE, showThreshold = TRUE,
                                 showCenter = TRUE, showPoints = TRUE, showNeighbors = c(15), showLines = c(15))

