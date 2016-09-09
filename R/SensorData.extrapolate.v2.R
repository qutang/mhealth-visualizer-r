#' @name SensorData.extrapolate.v2
#' @title Apply extrapolate algorithm to sensor data. Algorithm was developed for activity count by NU mhealth group.
#' @author Qu Tang
#' @export
#' @import plyr MASS akima dplyr
#' @param sensorData input dataframe that matches mhealth specification.
#' @param range dynamic range, a vector with two values.
#' @param noise_std estimated noise standard deviation
#' @param sr sampling rate
#' @param k duration of neighborhood to be used in local spline regression for each side, in seconds
#' @param spar between 0 and 1, to control how smooth we want to fit local spline regression, 0 is linear and 1 matches all local points. A good choice is 0.3 to penalize the maxed out points. 
#' @param confident
#' @param fit_model "spline", "linear"
SensorData.extrapolate.v2 = function(sensorData,...
){
  t = sensorData[[MHEALTH_CSV_TIMESTAMP_HEADER]]
  values = sensorData[2:ncol(sensorData)]
  result = adply(values, .margins = 2, function(colData){
    output = extrapolate(t, colData[[1]], ...)
    t_out <<- output[,1]
    return(output[,2])
  }, .progress = progress_text(), .id = NULL)
  result = data.frame(t(result), row.names = NULL)
  result = cbind(t_out, result)
  names(result) = names(sensorData)
  names(result[2:ncol(result)]) = paste("EXTRAPOLATED", names(result[2:ncol(result)]), sep = "_")
  result[,1] = as.POSIXct(result[,1], origin = "1970-01-01")
  return(result)
}

#' @name extrapolate
#' @title Apply extrapolate algorithm to a single stream of data. This is used as the inner function for `SensorData.extrapolate`.
#' @note If wanting to obtain mediate results for plotting and debugging, please use this function.
#' @author Qu Tang
#' @export
#' @import MASS akima
#' @param t input index sequence, start from 1
#' @param k duration of neighborhood to be used in local spline regression for each side, in seconds
#' @param spar between 0 and 1, to control how smooth we want to fit local spline regression, 0 is linear and 1 matches all local points. A good choice is 0.3 to penalize the maxed out points. 
#' @param confident
#' @param fit_model "spline", "linear"
extrapolate = function(t, value, range, noise_std, k = 0.1, spar = 0.3, confident = 0.5, fit_model = "spline"){
  # over sampling to 100Hz with resampling filter
  t_over = seq(t %>% first, t %>% last, by = 1/100)
  
  # over sampling to 100Hz with linear interpolation
  dat_over = approx(x = t, y = value, xout = t_over)
  t = dat_over$x
  value = dat_over$y
  
  
  # mark maxed out region using gamma distribution or threshold
  marker.fun = .extrapolate.mark("gamma")
  marker = marker.fun(t, value, range[1], range[2], noise_std)
  
  # mark neighbors
  neighbors = .extrapolate.neighbor(marker, 100, k, confident)
  
  # fit local spline regression
  points_ex = .extrapolate.fit(t, value, neighbors, marker, spar, 100, k, model = fit_model)
  
  # interpolate with the original
  dat_interp = .extrapolate.interpolate(t, value, marker, points_ex, 100, confident)
  
  return(dat_interp)
}

.extrapolate.mark = function(method = "gamma"){
   return(switch(method,
                 gamma = .mark.gamma,
                 threshold = .mark.threshold))
}

.mark.gamma = function(t, value, range_low, range_high, noise_sd = noise_sd){
  # init the mark vector
  marker = rep(0, length(t))
  
  # model of 3sd and shape para with confident probability at 0.95
  noise_sd = noise_sd + 10e-5
  shape = .optimize.gamma(3 * noise_sd)
  
  # mark using gamma distribution
  marker[value >= 0] = pgamma(value[value >= 0] - (range_high - 3 * noise_sd), shape = shape, scale = 1)
  marker[value < 0] = -pgamma(- value[value < 0] + (range_low + 3 * noise_sd), shape = shape, scale = 1)
  return(marker)
}

.optimize.gamma = function(value){
  i = seq(0.5, 0.001, by = -0.001)
  result = 0;
  previous = 1;
  previous_ii = 0;
  for(ii in i){
    current = pgamma(value, shape = ii, scale = 1)
    if(previous < 0.95 & current >= 0.95){
      if(abs(0.95 - previous) > abs(current - 0.95)){
        result = ii
      }else{
        result = previous_ii
      }
      break;
    }
    previous = current
    previous_ii = ii
  }
  return(result)
}

.mark.threshold = function(t, value, range_low, range_high, noise_sd = noise_sd){
  # init the mark vector
  marker = rep(0, length(t))
  
  # Compute the upper and lower threshold bound according to the range and threshold
  uBound = range_high - 5*noise_sd;
  lBound = range_low + 5*noise_sd;
  
  marker[input >= uBound] = 1
  marker[input <= lBound] = -1
  return(marker)
}

.extrapolate.neighbor = function(marker, sr, k, confident){
  n_neighbor = k * sr 
  marker_diff_left = c(0, diff(marker))
  marker_diff_right = c(diff(marker), 0)
  
  # hills
  positive_left_end = which(marker_diff_left > confident & marker > 0)
  positive_right_start = which(marker_diff_right < -confident & marker > 0)
  n_trunc = min(length(positive_left_end), length(positive_right_start))
  positive_left_end = positive_left_end[1:n_trunc] %>% na.omit
  positive_right_start = positive_right_start[1:n_trunc] %>% na.omit
  if(any(positive_right_start - positive_left_end < 0)) {
    positive_left_end = .shift(positive_left_end, 1)
    positive_right_start = .shift(positive_right_start, -1)
    }
  positive_edges = data.frame(left_end = positive_left_end, right_start = positive_right_start, stringsAsFactors = FALSE)
  
  # valleys
  negative_left_end = which(marker_diff_left < -confident & marker < 0)
  negative_right_start = which(marker_diff_right > confident & marker < 0)
  n_trunc = min(length(negative_left_end), length(negative_right_start))
  negative_left_end = negative_left_end[1:n_trunc] %>% na.omit
  negative_right_start = negative_right_start[1:n_trunc] %>% na.omit
  if(any(negative_right_start - negative_left_end < 0)) {
    negative_left_end = .shift(negative_left_end, 1)
    negative_right_start = .shift(negative_right_start, -1)
  }
  if(length(negative_left_end) != length(negative_right_start)){
    print("pause")
  }
  negative_edges = data.frame(left_end = negative_left_end, right_start = negative_right_start, stringsAsFactors = FALSE)
  neighbors = rbind(positive_edges, negative_edges) %>% tbl_df() %>% mutate(left_start = left_end - n_neighbor + 1, right_end = right_start + n_neighbor - 1) %>% as.data.frame
  return(neighbors)
}

.shift = function(v, lag, truncate = TRUE){
  n = length(v)
  if(lag > 0) v = v[-((n - lag + 1):n)]
  else v = v[-(1:(-lag))]
  return(v)
}

.extrapolate.fit = function(t, value, neighbors, marker, spar, sr, k, model = "spline"){
  point_ex = neighbors %>% adply(1, function(neighbor){
      fitted_left = .fit.weighted(t, value, marker, neighbor$left_start, neighbor$left_end, spar, sr, k, model)
      fitted_right = .fit.weighted(t, value, marker, neighbor$right_start, neighbor$right_end, spar, sr, k, model)
      
      st = t[neighbor$left_end]
      left_end = 0
      right_start = as.numeric(t[neighbor$right_start] - st, units = "secs")
      
      middle_t = (left_end + right_start) / 2
      middle_t = st + middle_t
      switch(model,
             linear = {
               left_ex = fitted_left %>% predict(data.frame(over_t = as.numeric(middle_t)))
               right_ex = fitted_right %>% predict(data.frame(over_t = as.numeric(middle_t)))
               point_ex = (left_ex + right_ex) / 2
             },
             spline = {
               left_ex = fitted_left %>% predict(x = as.numeric(middle_t))
               right_ex = fitted_right %>% predict(x = as.numeric(middle_t))
               point_ex = (left_ex$y + right_ex$y) / 2
             })
      
      return(data.frame(t_ex = middle_t, value_ex = point_ex))
  }, .inform = TRUE, .id = NULL, .expand = FALSE)
  return(point_ex)
}

.fit = function(t, value, marker, start, end, spar, sr, k, model = "spline"){
  if(start < 0) start = 1
  if(end > length(t)) end = length(t)
  # over sampling so that we have enough points
  n_over = k * sr
  sub_t  = t[start:end]
  sub_value = value[start:end]
  sp = approx(x = sub_t, y = sub_value, n = n_over)
  over_t = sp$x[-n_over]
  over_value = sp$y[-n_over]
  
  # fit locally with spline smoothing
  fitted = switch(model, 
                  spline = smooth.spline(over_t, over_value, spar = spar),
                  linear = lm(over_value ~ over_t, na.action = na.omit))
  return(fitted)
}

.fit.weighted = function(t, value, marker, start, end, spar, sr, k, model = "spline"){
  if(start < 0) start = 1
  if(end > length(t)) end = length(t)
  # over sampling so that we have enough points
  n_over = k * sr
  sub_t  = t[start:end]
  sub_value = value[start:end]
  weight = 1- marker[start:end]
  sp = approx(x = sub_t, y = sub_value, n = n_over)
  weight = approx(x = sub_t, y = weight, n = n_over)
  over_t = sp$x
  over_value = sp$y
  weight = weight$y
  # fit locally with spline smoothing
  fitted = switch(model,
                  spline = smooth.spline(over_t, over_value, weight, spar = spar),
                  linear = lm(over_value ~ over_t, weights = weight, na.action = na.omit))
  return(fitted)
}

.extrapolate.interpolate = function(t, value, marker, points_ex, sr, confident){
  t_mark = t[abs(marker) < confident]
  value_mark = value[abs(marker) < confident]
  # t_mark = t
  # value_mark = value
  dat = rbind(data.frame(t = t_mark, value = value_mark), data.frame(t = points_ex$t_ex, value = points_ex$value_ex)) %>% 
      arrange(t)
  
  t_interp = seq(t %>% first, t %>% last, by = 1/sr)
  dat_interp = spline(dat$t, y = dat$value, xout = t_interp) %>% as.data.frame %>% na.omit
  names(dat_interp) = c("t", "value")

  return(dat_interp)
}