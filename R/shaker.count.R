#' @name shaker.count
#' @title function to calculate the count values over series of epoches with specific count input parameters for shaker data
#' @note This function would be only feasible for shaker type of data. There are two dataset attached with the package "shaker1" and "shaker2". Please use `?shaker1` or `?shaker2` to check its format.
#' @param raw_data the dataset name, if it's a string, the function will try to load it using `data()` function otherwise will take it directly as a shaker dataframe
#' @param epoches epoches in character vector that has format "num timeunit", such as "5 sec".
#' @param filterTypes filters to be used when computing count
#' @param low_bandwidths low end bandwidths of the filter
#' @param high_bandwidths high end bandwidths of the filter
#' @param intTypes integration types of the count algorithm: "trapz" or "power"
#' @import plyr dplyr
#' @export
shaker.count = function(raw_data = "shaker1", 
                         epoches = c("2 sec", "5 sec", "10 sec", "15 sec"),
                         resamples = c(0),
                         filterTypes = c("butter"),
                         low_bandwidths = c(0.25),
                         high_bandwidths = c(5),
                         intTypes = c("trapz")){
  require(mhealthformatsupportr)
  require(plyr)
  require(dplyr)
  data(list = raw_data, envir = environment())
  shaker_data = eval(parse(text = raw_data))
  
  # prepare expanded data frame of parameter combinations
  paras = expand.grid(EPOCH = epoches, FILTER_TYPE = filterTypes, LOW_BW = low_bandwidths, HIGH_BW = high_bandwidths, INTEGRATION = intTypes, RESAMPLE = resamples, stringsAsFactors = FALSE)
  
  # compute count over epoches
  shaker_count = shaker_data %>% ddply(.(RPM, DEVICE, GRANGE, LOCATION, SR), function(segment) {
    result = paras %>% adply(1, function(para) {
      count = segment %>% 
        subset(select = 1:4) %>% 
        SensorData.summary.counts.compute(
          breaks = para$EPOCH,
          resample = para$RESAMPLE, 
          filterType = para$FILTER_TYPE,
          cutoffs = c(para$LOW_BW, para$HIGH_BW),
          integrationType = para$INTEGRATION
        ) %>% 
        na.omit
      result = count %>% cbind(
        segment %>% subset(select = -(1:4)) %>% unique,
        INDEX = 1:nrow(count),
        stringsAsFactors = FALSE
      )
      return(result)
    },
    .progress = "none",
    .inform = TRUE)
    return(result)
  }, .progress = "text", .inform = TRUE, .parallel = FALSE)
  
  countCol = shaker_count %>% colnames %>% str_detect(pattern = "ACCELATION") %>% which
  names(shaker_count)[countCol] = "COUNT"
  
  shaker_count = shaker_count %>% subset(
    select = c(
      HEADER_TIME_STAMP,
      DEVICE,
      GRANGE,
      SR,
      LOCATION,
      RPM,
      EPOCH,
      RESAMPLE,
      FILTER_TYPE,
      LOW_BW,
      HIGH_BW,
      INTEGRATION,
      INDEX,
      COUNT
    )
  )
  
  # join with actigraph count if exists
  
  actigraph_data = raw_data %>% paste("actigraph", sep = "_")
  if(exists(x = actigraph_data)){
    data(list = actigraph_data, envir = environment())
    shaker_actigraph = eval(parse(text = actigraph_data))
    # select epoches
    shaker_actigraph = shaker_actigraph %>% subset(EPOCH %in% epoches)
    
    shaker_count = shaker_count %>% 
      subset(select = -1) %>%
      join(shaker_actigraph, type = "inner") %>%
      subset(
        select = c(
          HEADER_TIME_STAMP,
          DEVICE,
          GRANGE,
          SR,
          LOCATION,
          RPM,
          EPOCH,
          RESAMPLE,
          FILTER_TYPE,
          LOW_BW,
          HIGH_BW,
          INTEGRATION,
          INDEX,
          COUNT,
          ACTIGRAPH_COUNT
        )
      )
  }
  return(shaker_count)
}