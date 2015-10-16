require("rChoiceDialogs")
filename = rchoose.files(default = getwd(), caption = "Select csv or csv.gz file", multi = FALSE)
if(length(filename) == 0) stop("No file selected, program terminated")
dir = rchoose.dir(default = getwd(), caption = "Select destination Directory")
if(length(dir) == 0) stop("No folder selected, program terminated")

print(paste(filename, "Start process"))
dat = SensorData.importCsv(filename)
breaks = "min"
print(paste(filename, "imported"))
summaryData = SummaryData.simpleMean(dat, breaks = breaks)
print(paste(filename, "summarized"))

dir.create(dirName, recursive = TRUE)
if(grepl("csv.gz", x = filename)){
  newName = gsub(pattern = "csv.gz", replacement = "png", x = basename(filename), perl = TRUE)
}else if(grepl("csv", x = filename)){
  gsub(pattern = "csv", replacement = "png", x = basename(filename), perl = TRUE)
}
newFile = paste(dirName, newName, sep="")
SummaryData.plot(summaryData)
png(filename = newFile)
SummaryData.plot(summaryData)
dev.off()
print(paste(newFile, "got written"))
