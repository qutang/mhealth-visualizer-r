testData = SensorData.importActigraphCsv("../../MATLAB/counts/consistency_test/data/P1-2/DIEGO_DOMINANT_WAIST_40 (2016-03-25)RAW.csv")
clipped = SensorData.clip(testData, startTime = "2016-03-25 16:00:00", endTime = "2016-03-25 16:10:30")
SensorData.ggplot(clipped)
