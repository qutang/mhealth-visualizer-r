#' ---
#' title: SensorData.extrapolate.visualize
#' layout: launch
#' ---

#' @name SensorData.extrapolate.visualize
#' @title visualizing function for viewing the mediate results of the extrapolation algorithm
#' @export
#' @import ggplot2 lubridate grid

SensorData.extrapolate.visualize = function(extrapolated,
                                 input,
                                 groundtruth,
                                 dynamicRange = NULL,
                                 maxedOutRegion = NULL,
                                 regressionResults = NULL,
                                 showInput = TRUE,
                                 showGroundTruth = TRUE,
                                 showOutput = TRUE,
                                 showMarker = FALSE,
                                 showThreshold = FALSE,
                                 showEdges = FALSE,
                                 showCenter = FALSE,
                                 showNeighbors = FALSE,
                                 showLines = FALSE,
                                 showPoints = FALSE){
maxedOutThreshold = maxedOutRegion$threshold
maxedOutMarkers = maxedOutRegion$markedMaxedOutPoints
maxedOutEdges = maxedOutRegion$edgePairs

regressionNeighbors = regressionResults$regressionNeighbors
regressionLines = regressionResults$regressionLines
intersectPoints = regressionResults$intersectPoints

# Build a dataframe for use in ggplot2
colnames(input) = c(MHEALTH_CSV_TIMESTAMP_HEADER, paste("INPUT_DATA", colnames(input)[2], sep = "_"))
colInput = colnames(input)[2]
colnames(groundtruth) = c(MHEALTH_CSV_TIMESTAMP_HEADER, paste("GROUNDTRUTH", colnames(input)[2], sep = "_"))
colTruth = colnames(groundtruth)[2]
colnames(extrapolated) = c(MHEALTH_CSV_TIMESTAMP_HEADER, paste("EXTRAPOLATED", colnames(extrapolated)[2], sep="_"))
colExtra = colnames(extrapolated)[2]
plotData = Reduce(function(x, y){ merge(x, y, by = MHEALTH_CSV_TIMESTAMP_HEADER)}, list(input, groundtruth, extrapolated))
if(!is.null(maxedOutMarkers)){
  plotData = cbind(plotData, "MAXED_OUT_MARKER" = maxedOutMarkers)
}
# Plot the original and groundtruth and extrapolated

p = ggplot(data = plotData, aes_string(x = MHEALTH_CSV_TIMESTAMP_HEADER))

if(showInput){
  p = p + geom_line(aes_string(y = colInput), linetype = "dashed", colour = "red") + geom_point(aes_string(y = colInput), shape = 16, colour = "red", size = 3)
}
if(showGroundTruth){
  p = p + geom_line(aes_string(y = colTruth), linetype = "dashed")
}
if(showOutput){
  p = p + geom_line(aes_string(y = colExtra), linetype = "solid") + geom_point(aes_string(y = colExtra), shape = 16)
}
# Plot maxed out markers
if(showMarker){
  p = p + geom_point(aes_string(y = "MAXED_OUT_MARKER"), shape = 18, size = 3, colour = "blue")
}

# Plot maxed out threshold horizontal line
if(showThreshold && !is.null(maxedOutThreshold)){
  p = p + geom_hline(yintercept = maxedOutThreshold[1], color = "green", linetype = "dashed")
  p = p + geom_hline(yintercept = maxedOutThreshold[2], color = "green", linetype = "dashed")
}

# Plot maxed out region center
if(showCenter && !is.null(maxedOutThreshold) && !is.null(maxedOutEdges)){
  yend = maxedOutThreshold[2]*maxedOutEdges[,4] + maxedOutThreshold[1]*abs(maxedOutEdges[,4] - 1)
  yend = yend * 1.05
  ystart = yend * 1.05
  xpos = (as.numeric(input[maxedOutEdges[,1],1]) + as.numeric(input[maxedOutEdges[,2],1]))/2
  xpos = as.POSIXct(xpos, origin = origin)
  regionData = data.frame(xpos, ystart, yend)
  p = p + geom_segment(data = regionData, aes(x = xpos, xend = xpos,
                            yend = yend, y = ystart),
                       arrow = arrow(end = "last", length = unit(0.2, units = "lines")))
}

# Plot neighbors

# Plot intersect points
if(showPoints && !is.null(intersectPoints)){
  p = p + geom_point(data = intersectPoints, aes(x = intersectPointsX, y = intersectPointsY), shape = 17, colour = "blue", size = 3)
}
return(p)
}
