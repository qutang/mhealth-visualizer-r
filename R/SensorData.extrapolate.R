#' @name SensorData.extrapolate
#' @title Apply extrapolate algorithm to sensor data. Algorithm was developed for activity count by NU mhealth group
#' @author Qu Tang
#' @export
#' @import plyr MASS akima
SensorData.extrapolate = function(sensorData,
                                  lambda,
                                  interpolate,
                                  range,
                                  noiseStd
){
  colExtrapolate = colwise(function(t, sensorData, param){
    sensorData = cbind(t, sensorData)
    output = .SensorData.extrapolate.singleColumn(sensorData, param)
    result = output$output[[2]]
    return(result)
  }, param = c(lambda, interpolate, range, noiseStd), t = sensorData[MHEALTH_CSV_TIMESTAMP_HEADER], .cols = is.numeric)
  result = as.data.frame(cbind(sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]], colExtrapolate(sensorData)))
  names(result) = names(sensorData)
  return(result)
}

.SensorData.extrapolate.singleColumn = function(sensorData,
                                                param
                                                ){
  originData = sensorData[,2]
  t = sensorData[,1]
  N_before = length(originData)

  lambda = as.numeric(param[1])
  interpolate = param[2]
  range = as.numeric(param[3])
  noiseStd = as.numeric(param[4])

  markedOriginal = .SensorData.extrapolate.mark(t, originData, -range, range, noiseStd)
  maxedOutPoints = markedOriginal$maxedOutPoints
  withoutMaxedOutPoints = markedOriginal$withoutMaxedOutPoints
  edgePairs = .SensorData.extrapolate.getEdges(t, maxedOutPoints)

  # iterate over the pairs of positive/negative maxed out edge points
  if(length(edgePairs) == 0){
    intersectPointsX = NULL
    intersectPointsY = NULL
    allNeighbors = NULL
    fittedResults = NULL

  }else{
    intersectPointsX = c()
    intersectPointsY = c()
    allNeighbors = .SensorData.extrapolate.getNeighors(t, originData, edgePairs, noise_sd = noiseStd)
    fittedResults = .SensorData.extrapolate.fitLines(x = t,
                                          input = originData,
                                          allNeighbors = allNeighbors,
                                          dynamic_range = range,
                                          noise_sd = noiseStd,
                                          lambda = lambda)
    for(fitted in fittedResults){
      intersectPointsX = c(intersectPointsX, fitted$intersectPoint[1])
      intersectPointsY = c(intersectPointsY, fitted$intersectPoint[2])
    }
  }

  if(!is.null(intersectPointsX)){
    if(class(t[1]) == "POSIXlt" || class(t[1]) == "POSIXct"){
      intersectPointsX = as.POSIXct(intersectPointsX, origin = "1970-01-01")
    }
  }

  outputY = c(withoutMaxedOutPoints, intersectPointsY[!is.na(intersectPointsY)])
  outputX = c(t, intersectPointsX[!is.na(intersectPointsX)])
  output = data.frame(outputX, outputY)
  # sort by time
  output = output[order(output[,1]),]
  # exclude NA data
  output = .SensorData.extrapolate.interpolate(output[[1]], output[[2]], method=interpolate, xout = t);
  output = as.data.frame(output)

  if(class(t[1]) == "POSIXlt" || class(t[1]) == "POSIXct"){
    output$x = as.POSIXlt(output$x, origin = "1970-01-01")
  }

  result = list(output = output, markedOriginal = markedOriginal, edgePairs = edgePairs, allNeighbors = allNeighbors, fittedResults = fittedResults)
  return(result)
}

.SensorData.extrapolate.mark = function(x, input, range_low, range_high , noise_sd){

  #  Inner functions
  constructOutput = function(x, maxedOutPoints, withoutMaxedOutPoints, ubound, lbound){
    output = {}
    output$x = x
    output$maxedOutPoints = maxedOutPoints
    output$withoutMaxedOutPoints = withoutMaxedOutPoints
    output$ubound = ubound
    output$lbound = lbound
    return(output)
  }
  accordingToThreshold = function(x, input, range_low, range_high, noise_sd = noise_sd){
    # Compute the upper and lower threshold bound according to the range and threshold
    uBound = range_high - 5*noise_sd;
    lBound = range_low + 5*noise_sd;

    # initialize output variables
    withoutMaxedOutPoints = input;
    maxedOutPoints = input;

    # maxed out criterion
    maxedOutCriterion = input > uBound | input < lBound;

    # make maxed out points NA
    withoutMaxedOutPoints[maxedOutCriterion] = NA;

    # not maxed out criterion
    notMaxedOutCriterion = input <= uBound & input >= lBound;

    # make not maxed out points NA
    maxedOutPoints[notMaxedOutCriterion] = NA;

    # Construct output object
    output = constructOutput(x, maxedOutPoints, withoutMaxedOutPoints, uBound, lBound)
    return(output)
  }

  output = accordingToThreshold(x, input, range_low, range_high, noise_sd);
  return(output)
}

.SensorData.extrapolate.getEdges = function(x, maxedOutPoints){

  # divide maxed-out points into positives and negatives, non-maxed-out points will be false
  positiveMaxedOut = maxedOutPoints > 0
  negativeMaxedOut = maxedOutPoints < 0
  positiveMaxedOut[is.na(positiveMaxedOut)] = FALSE
  negativeMaxedOut[is.na(negativeMaxedOut)] = FALSE

  # find out the edges of the maxed out points, divided by left and right side
  leftPositiveMax = which(diff(positiveMaxedOut) > 0) + 1
  rightPositiveMax = which(diff(positiveMaxedOut) < 0)
  leftNegativeMax = which(diff(negativeMaxedOut)  > 0) + 1
  rightNegativeMax = which(diff(negativeMaxedOut) < 0)

  # deal with tail or head condition
  if(length(rightPositiveMax) > 0 & length(leftPositiveMax) > 0){
    # when the most left positive maxed out point is on the right side of the hill
    if(rightPositiveMax[1] < leftPositiveMax[1]){
      leftPositiveMax = c(1, leftPositiveMax)
    }
    # when the most right positive maxed out point is on the left side of the hill
    if(leftPositiveMax[length(leftPositiveMax)] > rightPositiveMax[length(rightPositiveMax)]){
      rightPositiveMax = c(rightPositiveMax, length(maxedOutPoints))
    }
  }

  if(length(rightNegativeMax) > 0 & length(leftNegativeMax) > 0){
    # when the most left negative maxed out point is on the right side of the valley
    if(rightNegativeMax[1] < leftNegativeMax[1]){
      leftNegativeMax = c(1, leftNegativeMax)
    }
    # when the most right negative maxed out point is on the left side of the valley
    if(leftNegativeMax[length(leftNegativeMax)] > rightNegativeMax[length(rightNegativeMax)]){
      rightNegativeMax = c(rightNegativeMax, length(maxedOutPoints))
    }
  }

  # Prepare edge maxed out matrix
  #
  #   posMaxLeft posMaxRight middleOfRegion 1
  #   ...
  #   negMaxLeft negMaxRight middleOfRegion 0
  #   ...
  if(length(leftPositiveMax)>0){
    l = min(length(leftPositiveMax), length(rightPositiveMax))
    posMaxPairs = cbind(leftPositiveMax[1:l], rightPositiveMax[1:l], (leftPositiveMax[1:l] + rightPositiveMax[1:l])/2, TRUE)
  }else{
    posMaxPairs = NULL
  }
  if(length(leftNegativeMax)>0){
    l = min(length(leftNegativeMax), length(rightNegativeMax))
    negMaxPairs = cbind(leftNegativeMax[1:l], rightNegativeMax[1:l], (leftNegativeMax[1:l] + rightNegativeMax[1:l])/2, FALSE)
  }else{
    negMaxPairs = NULL
  }
  maxPairs = rbind(posMaxPairs, negMaxPairs)

  return(maxPairs)
}

.SensorData.extrapolate.getNeighors = function(x, input, edgePairs, noise_sd = NULL){
  # require(doParallel)
  # cl <- makeCluster(4)
  # registerDoParallel(cl)
  # allNeighbors = foreach(m = 1:nrow(edgePairs), .export = ".slopeBasedAlgForNeighboring") %dopar% {
  allNeighbors = list()
  for(m in 1:nrow(edgePairs)){
    neighbors = .slopeBasedAlgForNeighboring(x = x, input = input, edgePair = edgePairs[m,], noise_sd = noise_sd)
    allNeighbors = c(allNeighbors, list(neighbors))
  }
  # stopCluster(cl)
  return(allNeighbors)
}

.SensorData.extrapolate.fitLines = function(x,
                                            input,
                                            allNeighbors,
                                            dynamic_range,
                                            weight_method = "noise",
                                            lambda = 1,
                                            noise_sd = noise_sd){
  # require(doParallel)
  # cl <- makeCluster(4)
  # registerDoParallel(cl)
  # result = foreach(m = 1:length(allNeighbors), .export = c(".fitLineWithNeighbors", ".computeWeightsForLm", ".findIntersectPointWithFittedLines")) %dopar% {
  fittedResults = list()
  for(m in 1:length(allNeighbors)){
    fittedLine = .fitLineWithNeighbors(x = x,
                                       input = input,
                                       neighbors = allNeighbors[[m]],
                                       dynamic_range = dynamic_range,
                                       weight_method = weight_method,
                                       lambda = lambda,
                                       noise_sd = noise_sd)
    fittedResults = c(fittedResults, list(fittedLine = fittedLine))
  }
  # }

  # stopCluster(cl)

  return(fittedResults)
}

.SensorData.extrapolate.interpolate = function(x, input, method = "aspline_original", xout){
  output = switch(method,
                  linear = approx(x, input, method="linear", xout = xout),
                  spline_fmm = spline(x, input, xout = xout, method="fmm"),
                  spline_natural = spline(x, input,xout = xout, method="natural"),
                  aspline_original = {aspline(x, input, xout = xout, method="original")},
                  aspline_improved = {aspline(x, input,xout = xout, method="improved", degree=3)});

  return(output);
}


.naiveAlgForNeighboring = function(leftIndex, rightIndex, include = FALSE){
  # naive algorithm is to just get one more neighbors at the edge of the maxed out regions
  # include: whether to include the edge itself
  if(include){
    leftNeighbor = c(leftIndex - 1, leftIndex)
    rightNeighbor = c(rightIndex, rightIndex + 1)
  }else{
    leftNeighbor = c(leftIndex - 2, rightIndex - 1)
    rightNeighbor = c(rightIndex + 1, rightIndex + 2)
  }
  result = list(leftNeighbors = leftNeighbor, rightNeighbors = rightNeighbor)
  return(result)
}

.slopeBasedAlgForNeighboring = function(x, input, edgePair, noise_sd){
  # Apply median filter first to smooth out the signal, so that the trend is easy and more robust to detect on each side
  # The median filter window size would be as small as 3 to adapt to the most complex case

  positive = edgePair[4]
  middleIndex = edgePair[3]
  leftIndex = edgePair[1]
  rightIndex = edgePair[2]

  input_y = input
  input_x = x

  # get the time interval between adjacent samples
  if(is(input_x[1], "POSIXct")){
    # If x axis is date object, the interval is calculated in milliseconds
    a = print(as.numeric(input_x[1]), digits = 13)
    b = print(as.numeric(input_x[2]), digits = 13)
    interval = b - a
  }else{
    interval = input_x[2] - input_x[1]
  }

  # default slope_threshold
  if(!is.null(noise_sd)){
    slope_threshold = 3*noise_sd / interval
  }else{
    slope_threshold = 0
  }

  # initialize neighbor sequence
  leftSides = input_y
  leftSides[seq(floor(leftIndex)+1, length(input_y))] = NA # make right side NA
  rightSides = input_y
  rightSides[seq(1, ceiling(rightIndex) - 1)] = NA # make left side NA

  # Get the slope of each sides
  leftSlopes = diff(leftSides) / interval
  rightSlopes = diff(rightSides) / interval

  print(paste(slope_threshold, max(leftSlopes, na.rm = TRUE), min(rightSlopes, na.rm = TRUE), sep = ","))

  if(positive){
    # If it's hill, left side slope should be > 0, and right side slope should be < 0
    # slope_threshold is the stop criteria when extending the neighborhood

    # left bound is where abs(slope) is greater than threshold and becomes negative
    # leftBound = max(intersect(which(leftSlopes < 0), which(abs(leftSlopes) > slope_threshold))) + 1
    leftBound = max(which(leftSlopes < -slope_threshold)) + 1
    leftBound = min(leftBound, leftIndex - 2)
    if(is.infinite(leftBound)){ # edge condition
      leftBound = 1
    }

    # indices of left neighbors
    leftNeighbors = seq(leftBound, middleIndex)

    # right bound is where abs(slope) is greater than threshold and becomes positive
    # rightBound = min(intersect(which(rightSlopes > 0), which(abs(rightSlopes) > slope_threshold)))
    rightBound = min(which(rightSlopes > slope_threshold))
    rightBound = max(rightBound, rightIndex + 2)
    # deal with edge condition
    if(is.infinite(rightBound)){
      rightBound = length(input)
    }
    # indices of right neighbors
    rightNeighbors = seq(ceiling(middleIndex), rightBound)

    #     maxAmount = min(length(leftNeighbors), length(rightNeighbors))
    #     leftNeighbors = leftNeighbors[1:maxAmount]
    #     rightNeighbors = rightNeighbors[1:maxAmount]
  }
  else{
    # If it's valley, left side slope should be < 0, and right side slope should be > 0
    # slope_threshold is the stop criteria when extending the neighborhood

    # left bound is where abs(slope) is greater than threshold and becomes positive
    # leftBound = max(intersect(which(leftSlopes > 0), which(abs(leftSlopes) > slope_threshold))) + 1
    leftBound = max(which(leftSlopes > slope_threshold))
    leftBound = min(leftBound, leftIndex-2)
    # deal with edge condition
    if(is.infinite(leftBound)){
      leftBound = 1
    }
    # indices of left neighbors
    leftNeighbors = seq(leftBound, middleIndex)

    # left bound is where abs(slope) is greater than threshold and becomes negative
    # rightBound = min(intersect(which(rightSlopes < 0), which(abs(rightSlopes) > slope_threshold)))
    rightBound = min(which(rightSlopes < -slope_threshold))
    rightBound = max(rightBound, rightIndex + 2)
    # deal with edge condition
    if(is.infinite(rightBound)){
      rightBound = length(input)
    }
    # indices of right neighbors
    rightNeighbors = seq(ceiling(middleIndex), rightBound)

    #     maxAmount = min(length(leftNeighbors), length(rightNeighbors))
    #     leftNeighbors = leftNeighbors[1:maxAmount]
    #     rightNeighbors = rightNeighbors[1:maxAmount]
  }

  # return indices of left and right neighbors
  result = list(leftNeighbors = leftNeighbors, rightNeighbors = rightNeighbors, positive = positive)
  return(result)
}


.computeWeightsForLm = function(x,
                                input,
                                method = "noise",
                                lambda = 1,
                                noise_sd = 0.1,
                                dynamic_range){
  # initial equal weights
  weights = rep(1, length(input))
  #   leftW[input > 0] = exp(lambda*abs(input[input>0] - range_high))
  #   leftW[input < 0] = exp(lambda*abs(input[input<0] - range_low))
  if(length(input) > 3){
    # at least three points in the neighborhood
    if(method == "min_points"){
      middleY = (max(input) + min(input))/2
      middleX = x[which(abs(diff(input - middleY > 0)) > 0)]
      distances = -abs(x - middleX)
      distances[abs(distances) < 2] = 0
      weights = exp(lambda*distances)
    }else if(method == "noise"){
      maxY = max(input, na.rm = TRUE)
      minY = min(input, na.rm = TRUE)
      middleY = (maxY + minY)/2
      middleX = x[which(abs(diff(input - middleY > 0)) > 0)]
      distances = rep(0, length(x))

      posIndices = which(abs(maxY - input) <= 4*noise_sd)
      negIndices = which(abs(minY - input) <= 4*noise_sd)
      normalIndices = which((abs(maxY - input) > 4*noise_sd & input >= 0) | (abs(minY - input) > 4*noise_sd & input < 0))


      if(length(posIndices) > 0){
        # edge points
        if(middleX < x[posIndices[1]]){
          posIndices = seq(posIndices[1], length(x))
          distances[posIndices] = seq(-1, -length(posIndices))

          if(length(negIndices > 0)){
            negIndices = seq(1, negIndices[length(negIndices)])
            distances[negIndices] = seq(-length(negIndices), -1)

          }
        }else{
          if(length(negIndices > 0)){
            negIndices = seq(negIndices[1], length(x))
            distances[negIndices] = seq(-1, -length(negIndices))
          }

          posIndices = seq(1, posIndices[length(posIndices)])
          distances[posIndices] = seq(-length(posIndices), -1)

        }
        #       lambda = min(length(posIndices), length(negIndices))*2 / length(normalIndices)
        # lambda = 2.5 / min(length(posIndices), length(negIndices))
        #       lambda = 1.5

      }
      distances[posIndices] = -input[posIndices] + (dynamic_range - 3*noise_sd)  # here uses the distance between max and point
      distances[negIndices] = input[negIndices] - (-dynamic_range + 3*noise_sd) # here uses the distance between min and point
      weights = exp(lambda*distances)
      #       print(posIndices)
      #       print(negIndices)
      #       print(middleX)
      #       print(distances)
    }else if(method == "only_noise"){
      print(lambda)
      maxY = max(input)
      minY = min(input)
      middleY = (maxY + minY)/2
      middleX = x[which(abs(diff(input - middleY > 0)) > 0)]
      distances = rep(0, length(x))
      posIndices = which(abs(maxY - input) <= 3*noise_sd)
      negIndices = which(abs(minY - input) <= 3*noise_sd)
      if(middleX < x[posIndices[1]]){
        posIndices = seq(posIndices[1], length(x))
        negIndices = seq(1, negIndices[length(negIndices)])
      }else{
        negIndices = seq(negIndices[1], length(x))
        posIndices = seq(1, posIndices[length(posIndices)])
      }
      posRef = abs(input[posIndices] - maxY)
      negRef = abs(input[negIndices] - minY)
      posCandidates = cbind(posIndices, posRef)
      negCandidates = cbind(negIndices, negRef)
      posCandidates = posCandidates[order(-posRef),]
      negCandidates = negCandidates[order(-negRef),]
      distances[posCandidates[,1]] = -seq(1, nrow(posCandidates))
      distances[negCandidates[,1]] = -seq(1, nrow(negCandidates))
      weights = exp(lambda*distances)
      #       print(posCandidates)
      #       print(negCandidates)
      #       print(distances)
      #       print(weights)
    }
  }
  return(weights)
}

.fitLineWithNeighbors = function(x,
                                 input,
                                 neighbors,
                                 weight_method = "noise",
                                 lambda = 1,
                                 noise_sd = 0.1,
                                 dynamic_range){

  firstPoint = NULL
  x_value = as.numeric(x[neighbors$leftNeighbors])
  firstPoint = x_value[1]
  x_value0 = x_value - firstPoint
  y_value = input[neighbors$leftNeighbors]
  leftW = .computeWeightsForLm(x_value,
                               y_value,
                               method = weight_method,
                               lambda = lambda,
                               noise_sd = noise_sd,
                               dynamic_range = dynamic_range)

  leftlm = NULL
  tryCatch({leftlm = lm(y_value~x_value0, weights = leftW)}, error = function(e){
    print(x_value);
    print(y_value);
  });

  x_value = as.numeric(x[neighbors$rightNeighbors])
  x_value0 = x_value - firstPoint
  y_value = input[neighbors$rightNeighbors]
  rightW = .computeWeightsForLm(x_value,
                                y_value,
                                method = weight_method,
                                lambda = lambda,
                                noise_sd = noise_sd,
                                dynamic_range = dynamic_range)

  rightlm = NULL
  tryCatch({rightlm = lm(y_value~x_value0, weights = rightW)}, error = function(e){
    print(x_value);
    print(y_value);
  });

  cm = rbind(coef(leftlm),coef(rightlm)) # Coefficient matrix

  intersectPoint = NA
  tryCatch({
    intersectPoint = c(solve(cbind(cm[,2],-1)) %*% -cm[,1])
    intersectPoint[1] = intersectPoint[1] + firstPoint
    },
    error = function(e){
    intersectPoint = NA
    print(cm);
  });

  return(list(leftLine = leftlm, rightLine = rightlm, intersectPoint = intersectPoint))
}

