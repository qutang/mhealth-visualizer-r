library(mhealthactivitycountsr)

fileName = paste("ExtrapolateSinGridTest", round(as.numeric(Sys.time())), "html", sep=".")
csvResultName = paste("../inst/ExtrapolateSinGridTest", round(as.numeric(Sys.time())), "csv", sep=".")
logResultName = paste("../inst/ExtrapolateSinGridTest", round(as.numeric(Sys.time())), "log", sep=".")
logResultName = ""
seed = 5
set.seed(seed)
Extrapolate.sinusoidal.grid.report(duration = 30,
                                   Fs = 40,
                                   range = 6,
                                   freqs = seq(1,12),
                                   amp = seq(7,16),
                                   noiseStds = 0,
                                   phase = runif(20, 0, 10),
                                   parallel = TRUE,
                                   seed = seed,
                                   lambda = 3,
                                   outputDir = "inst/",
                                   outputFilename = fileName,
                                    outputFormat = "html_document",
                                   csvResultName = csvResultName,
                                   logFilename = logResultName)
