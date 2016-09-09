#' @name Extrapolate.sinusoidal.grid
#' @title test program that runs over grids of different parameters to create simulated sinusoidal signal and generate heatmap of extrapolation error
#' @export
#' @import mhealthformatsupportr plyr reshape2 doParallel matlab
Extrapolate.sinusoidal.grid = function(duration = 120,
                                 Fs = 40,
                                 range = 6,
                                 freqs = 10,
                                 amp = c(15,16),
                                 noiseStds = 0,
                                 phase = 0,
                                 seed = 1,
                                 lambda = 1,
                                 interpolate = "spline_natural",
                                 parallel = TRUE
                                 ){

  startTime = Sys.time()
  workers=makeCluster(4, outFile = "")
  registerDoParallel(workers, cores=4)

  clusterExport(cl=workers,varlist=list("duration", "Fs", "range", "amp", "phase", "seed", "lambda", "interpolate"),envir=environment())

  paramArr = expand.grid(freqs, amp, phase, noiseStds, lambda)
  nameList = c("freq", "amp", "phase","noiseStd", "lambda")
  names(paramArr) = nameList

  result = ddply(paramArr, nameList, .parallel = parallel,
        .paropts = list(.export=c("SensorData.generator.sinusoidal",
                                  "SensorData.extrapolate",
                                  "SummaryData.absoluteMean")),function(row){
          # Generate batch of sin signals
      tryCatch({
      result = data.frame(extrapolateError = c(), inputError = c())
      f = row[[1]]
      amp = row[[2]]
      phase = row[[3]]
      noiseStd = row[[4]]
      lambda = row[[5]]
      inputBatch = SensorData.generator.sinusoidal(startTime = startTime,
                                                       endTime = startTime + duration,
                                                       Fs = Fs,
                                                       range = range,
                                                       f = f,
                                                       amp = amp,
                                                       noiseStd = noiseStd,
                                                       seed = seed,
                                                       phase = phase
        )
      gtBatch = SensorData.generator.sinusoidal(startTime = startTime,
                                                    endTime = startTime + duration,
                                                    Fs = Fs,
                                                    range = amp,
                                                    f = f,
                                                    amp = amp,
                                                    noiseStd = 0,
                                                    seed = seed,
                                                    phase = phase
          )
      extrapolatedBatch = SensorData.extrapolate(inputBatch,
                                                     lambda = lambda,
                                                     interpolate = interpolate,
                                                     range = range,
                                                     noiseStd = noiseStd)

      options(digits.secs=3)
#       write.csv(gtBatch, file = paste0("inst/groundtruth.", as.numeric(Sys.time()),".csv"), quote = FALSE, sep = ",", na = "", row.names = FALSE)
#       write.csv(inputBatch, file = paste0("inst/input.", as.numeric(Sys.time()),".csv"), quote = FALSE, sep = ",", na = "", row.names = FALSE)
#       write.csv(extrapolatedBatch, file = paste0("inst/extrapolateBatch.", as.numeric(Sys.time()),".csv"), quote = FALSE, sep = ",", na = "", row.names = FALSE)

      extrapolatedCounts = SummaryData.absoluteMean(extrapolatedBatch)[-1]
      inputCounts = as.numeric(SummaryData.absoluteMean(inputBatch))[-1]
      gtCounts = as.numeric(SummaryData.absoluteMean(gtBatch))[-1]
      extrapolateError = as.numeric(abs(extrapolatedCounts - gtCounts)/gtCounts)
      inputError = as.numeric(abs(inputCounts - gtCounts)/gtCounts)


      result = rbind(result,data.frame(extrapolateError = extrapolateError,
                                          inputError = inputError))

      return(result)
      }, error = function(e){
          print(e)
          result = rbind(result,data.frame(extrapolateError = NA,
                                           inputError = NA))
          return(result)
        }
      )
  })
  stopCluster(workers)
  return(result)
}

#' @name Extrapolate.sinusoidal.grid.report
#' @title test program that runs over grids of different parameters to create simulated sinusoidal signal and generate heatmap of extrapolation error
#' @export
#' @import rmarkdown
Extrapolate.sinusoidal.grid.report = function(
  duration = 600,
  Fs = 40,
  range = 6,
  freqs = 2,
  amp = seq(5,16),
  noiseStds = 0,
  phase = 0,
  seed = 1,
  parallel = FALSE,
  lambda = 1,
  interpolate = "spline_natural",
  outputDir = getwd(),
  outputFilename = "result.html",
  outputFormat = "html_document",
  csvResultName = "../result.csv",
  logFilename = "../result.log"){
  render(input = "vignettes/Extrapolate.sinusoidal.grid.Rmd", output_dir = outputDir, output_file = outputFilename, output_format = outputFormat, envir = new.env())
}
