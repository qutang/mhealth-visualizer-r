#' @name shaker.plot
#' @title function to plot the output of walkrun.count function for walkrun data
#' @note This function would be only feasible for the output of walkrun.count function of walkrun type of data. There are two dataset attached with the package "walkrun1" and "walkrun2". Please use `?walkrun1` or `?walkrun2` to check its format.
#' @param shaker_count output dataframe of walkrun.count function
#' @param layout formula to specify the layout of subplots using column names, left side is rows and right side is columns. Please check the output of walkrun.count function to see all possible columns
#' @param series string to specify the column name to be used as color series on the same subplot
#' @param sample_group string to specify the column name to be used as shape series for sample points on the same subplot
#' @param scale scaling method to be used for each count type: "z-score" or "regression", or "none". "regression" is not supported by now.
#' @param subset_condition logical condition to subset the input dataframe before plotting it, same as the input to subset function of base R
#' @import plyr dplyr reshape2 ggthemes ggplot2
#' @export
shaker.plot = function(shaker_count,
                        layout =  SR ~ GRANGE,
                        series = "DEVICE",
                        sample_group = NULL,
                        scale = "z-score",
                        subset_condition = EPOCH == "5 sec") {
  
  shaker_count$LOW_BW = shaker_count$LOW_BW %>% as.character
  shaker_count$HIGH_BW = shaker_count$HIGH_BW %>% as.character
  shaker_count$RESAMPLE = shaker_count$RESAMPLE %>% as.character
  
  # prepare melted data frame
  shaker_scaled = shaker_count %>%
    subset(select = -1) %>% ddply(.(EPOCH, RESAMPLE, FILTER_TYPE, LOW_BW, HIGH_BW, INTEGRATION, GRANGE), function(seg) {
      if (scale == "z-score") {
        seg$COUNT = seg$COUNT %>% scale %>% as.numeric
        if("ACTIGRAPH_COUNT" %in% colnames(seg)){
          seg$ACTIGRAPH_COUNT = seg$ACTIGRAPH_COUNT %>% scale %>% as.numeric
        }
      }
      return(seg)
    })
  
  
  shaker_melted = shaker_scaled %>%
    melt(id = 1:13,
         value.name = "COUNT",
         variable.name = "COUNT_TYPE")
  
  # plot
  
  subset_condition = deparse(substitute(subset_condition))
  
  shaker_melted %>%
    subset(eval(parse(text = subset_condition))) %>%
    ggplot(aes_string(x = "RPM", y = "COUNT", color = series)) +
    geom_point(aes_string(shape = sample_group), size = 2) +
    stat_summary(
      fun.y = mean,
      geom = "line",
      size = 1,
      lty = "dashed"
    ) +
    stat_smooth() +
    facet_grid(layout) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# by subjects and locations, merged sampling rate, sessions
# walkrun1.plot()
# 
# # by subjects and count types, merged sampling rate, sessions
# walkrun1.plot(layout = SUBJECT ~ COUNT_TYPE, series = "LOCATION")
# 
# # by locations, merged sampling rate, seesions, and subjects
# walkrun1.plot(layout = . ~ LOCATION, series = "COUNT_TYPE")
# 
# # by count types, merged sampling rate, seesions, and subjects
# walkrun1.plot(layout = . ~ COUNT_TYPE, series = "LOCATION")
