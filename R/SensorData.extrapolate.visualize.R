#' ---
#' title: SensorData.extrapolate.visualize
#' layout: launch
#' ---

#' @name SensorData.extrapolate.visualize
#' @title visualizing function for viewing the mediate results of the extrapolation algorithm
#' @export
#' @import ggplot2 lubridate grid ggthemes RColorBrewer

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
                                 showNeighbors = NULL,
                                 showLines = NULL,
                                 showPoints = NULL){

colorList = brewer.pal(10, "Paired")
colorList2 = brewer.pal(10, "Set2")

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
  p = p + geom_line(aes_string(y = colInput), linetype = "dashed", colour = colorList2[3], size = 1.2) + geom_point(aes_string(y = colInput), shape = 16, colour = colorList2[3], size = 3)
}
if(showGroundTruth){
  p = p + geom_line(aes_string(y = colTruth), linetype = "dashed")
}
if(showOutput){
  p = p + geom_line(aes_string(y = colExtra), linetype = "solid", colour = colorList[1]) + geom_point(aes_string(y = colExtra), shape = 16, colour = colorList[1])
}
# Plot maxed out markers
if(showMarker){
  p = p + geom_point(aes_string(y = "MAXED_OUT_MARKER"), shape = 18, size = 3, colour = colorList[2])
}

# Plot maxed out threshold horizontal line
if(showThreshold && !is.null(maxedOutThreshold)){
  p = p + geom_hline(yintercept = maxedOutThreshold[1], color = colorList[4], linetype = "dashed")
  p = p + geom_hline(yintercept = maxedOutThreshold[2], color = colorList[4], linetype = "dashed")
}

# Plot maxed out region center
if(showCenter && !is.null(maxedOutThreshold) && !is.null(maxedOutEdges)){
  yend = maxedOutThreshold[2]*maxedOutEdges[,4] + maxedOutThreshold[1]*abs(maxedOutEdges[,4] - 1)
  yend = yend * 0.95
  ystart = yend * 0.95
  xpos = (as.numeric(input[maxedOutEdges[,1],1]) + as.numeric(input[maxedOutEdges[,2],1]))/2
  xpos = as.POSIXct(xpos, origin = origin)
  regionData = data.frame(xpos, ystart, yend)
#   p = p + geom_segment(data = regionData, aes(x = xpos, xend = xpos,
#                             yend = yend, y = ystart),
#                        arrow = arrow(end = "last", length = unit(0.2, units = "lines")))
  p = p + geom_text(data = regionData, aes(x = xpos, y = ystart), label = seq(1, nrow(regionData)))
}

# Plot neighbors
if(!is.null(showNeighbors) && !is.null(regressionNeighbors)){
  for(regionIndex in showNeighbors){
    leftIndices = regressionNeighbors[[regionIndex]]$leftNeighbors
    rightIndices = regressionNeighbors[[regionIndex]]$rightNeighbors
    leftNeighbors = plotData[leftIndices,]
    rightNeighbors = plotData[rightIndices,]
    p = p + geom_point(data = leftNeighbors, aes_string(y = colInput, x = MHEALTH_CSV_TIMESTAMP_HEADER), colour = colorList[2], size = 3)
    p = p + geom_point(data = rightNeighbors, aes_string(y = colInput, x = MHEALTH_CSV_TIMESTAMP_HEADER), colour = colorList[10], size = 3)
  }
}

# Plot regression lines
if(!is.null(showLines) && !is.null(regressionLines)){
  for(regionIndex in showLines){
    leftLine = regressionLines[[regionIndex]]$leftLine
    rightLine = regressionLines[[regionIndex]]$rightLine
    leftIndices = regressionNeighbors[[regionIndex]]$leftNeighbors
    rightIndices = regressionNeighbors[[regionIndex]]$rightNeighbors
    leftFirst = plotData[leftIndices[1],1]
    p = p + geom_abline(intercept = coef(leftLine)[[1]] - coef(leftLine)[[2]]*as.numeric(leftFirst), slope = coef(leftLine)[[2]], colour = colorList2[2], linetype = "dashed", size = 1.2)
    p = p + geom_abline(intercept = coef(rightLine)[[1]] - coef(rightLine)[[2]]*as.numeric(leftFirst), slope = coef(rightLine)[[2]], colour = colorList2[2], linetype = "dashed", size = 1.2)
  }
}

# Plot intersect points
if(showPoints && !is.null(intersectPoints)){
  p = p + geom_point(data = intersectPoints, aes(x = intersectPointsX, y = intersectPointsY), colour = colorList2[2], size = 3.5)
}



# set up theme
p = p + scale_color_few() + theme_bw()
return(p)
}
