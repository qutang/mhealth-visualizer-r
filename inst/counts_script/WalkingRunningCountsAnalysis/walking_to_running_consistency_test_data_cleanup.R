# Walking to Running Consistency test data clean up
# Clip into 30 seconds segments
# Save as mhealth and actigraph csv file
#
require(stringr);
require(ggplot2);
folder = "../../MATLAB/counts/consistency_test/data/";

path = paste0(folder, "ready/");
path2 = paste0(folder, "actigraphcsv/")

files = list.files(path = folder, pattern = "*.csv", full.names = FALSE);

locations = c("ANKLE", "THIGH", "WAIST", "WRIST");

sr = c(40, 100);

noweight = list();
withweight = list();

filenames = paste0("mph", seq(1,9));
filenames1 = paste(filenames, "noweight", sep = "_")
filenames2 = paste(filenames, "withweight", sep = "_")
filenames = c(filenames1, filenames2)

sts = as.POSIXct(c("2016-02-26 14:06:30", "2016-02-26 14:07:45", 
        "2016-02-26 14:09:15", "2016-02-26 14:11:15",
        "2016-02-26 14:12:45", "2016-02-26 14:14:15",
        "2016-02-26 14:16:10", "2016-02-26 14:17:40",
        "2016-02-26 14:19:15", "2016-02-26 14:25:15",
        "2016-02-26 14:26:45", "2016-02-26 14:28:15",
        "2016-02-26 14:29:45", "2016-02-26 14:31:15",
        "2016-02-26 14:32:45", "2016-02-26 14:34:45",
        "2016-02-26 14:36:45", "2016-02-26 14:38:45"));

for(s in sr){
  for(l in locations){
    if(s == 40){
      f = str_subset(files, paste0("^",l, " (2016-02-26)*"));
    }else{
      f = str_subset(files, paste0("^",l, "_100 (2016-02-26)*"));
    }
    
    d = SensorData.importActigraphCsv(paste0(folder, f));
    headerInfo = SensorData.parseActigraphCsvHeader(paste0(folder, f));
    i = 1;
    for(st in sts){
      clipped = SensorData.clip(d, st, st + 30);
      p = SensorData.ggplot(clipped)
      p
      name = paste(l, s, filenames[i], sep = "_")
      ggsave(paste(name, "png", sep="."), plot = p, path = path, width = 11, height = 8)
      SensorData.io.write(path, sensorData = clipped, gzip = FALSE, flatDir = TRUE, splitHour = FALSE, custom_name = paste(name, "csv", sep="."))
      headerStr = SensorData.createActigraphCsvHeader(headerInfo$st, headerInfo$et, headerInfo$sr, headerInfo$sn, headerInfo$fw, headerInfo$sw)
      SensorData.io.writeAsActigraphRaw(path2, clipped, headerStr, custom_name = paste(name, "csv", sep="."))
      print(paste("Processed", name))
      i = i + 1
    }
  }
}
