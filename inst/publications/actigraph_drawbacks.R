require(mhealthformatsupportr)
require(reshape2)
require(plyr)
require(dplyr)
require(ggplot2)

data("running_maxed_out")
data("walkrun1")
data("actigraph_spectrum")
# problem 1 maxed out problem
startTime = running_maxed_out[1,1]
clipped_running = ddply(running_maxed_out, .variables = "TYPE", function(data){
  return(data %>% SensorData.clip(startTime = startTime + 20, endTime = startTime + 25))
})
label_data = data.frame(x = c(4.5, 4.2), y = c(3, 6), label = unique(clipped_running$ACTIGRAPH_COUNT_X))
melted = melt(clipped_running, id = c(1,5,6,7,8,9))
melted = melted %>% filter(variable == "RAWX")
melted$HEADER_TIME_STAMP = as.numeric(melted$HEADER_TIME_STAMP - melted$HEADER_TIME_STAMP[1])
p = ggplot(data = label_data) + 
  geom_label( aes(x = x, y = y, label = label, color = c("GT3X", "GT9X")), size = 7) + 
  geom_line(data = melted, aes(x = HEADER_TIME_STAMP, y = value, colour = TYPE)) + 
  geom_hline(yintercept = 3, lty = "dashed", colour = "gray") + 
  annotate("text", 0.5, 3.2, label = "GT3X dynamic range at 3g", colour = "gray")+ 
  scale_color_discrete(labels = c("GT3X (30Hz, 3g)", "GT9X (100Hz, 16g)")) + 
  guides(colour = guide_legend(title = "")) + 
  theme_minimal(base_size = 16) + 
  xlab("time (s)") + 
  ylab("X-axis acceleration (g)") + 
  theme(legend.position = "bottom") 


ggsave(path = file.path("inst/publications/"), filename = "actigraph_drawbacks_1.png", plot = p, scale = 3, width = 4, height = 2)

# problem 2 beyond bandwidth
run_beyond_bandwidth = walkrun1 %>% filter(LOCATION == "NondominantWrist" & SUBJECT == "P2" & MPH == 7.5 & WEIGHT == "0" & SR == "100")

run_fft = run_beyond_bandwidth[,1:4] %>% FrequencyResponse.fft(Fs = as.numeric(run_beyond_bandwidth$SR[1]), normalize = "normalized", type = "magnitude")

new_spectrum = data.frame(spline(x = actigraph_spectrum$freq, y = actigraph_spectrum$value, xout = run_fft[,1]))
new_spectrum[new_spectrum$x > 10,2] = NA

fftData = run_fft[-1,] %>% Magnitude.compute 

fftData = fftData %>% mutate(spectrum = new_spectrum[-1,2])

p = fftData %>% FrequencyResponse.spectrum.ggplot(scale = "db")

p = p + scale_x_log10(breaks = c(0.1,1,2.5,4,5,7,10,20,35,50), limits = c(0.1, 50)) + scale_color_discrete(labels = c("running at 7.5mph", "Actigraph proprietary filter")) + guides(colour = guide_legend(title = "")) + theme_minimal( base_size = 16) + ylab("intensity (dB)") + xlab("frequency (log(Hz))") + ggtitle("") + theme(legend.position = "bottom")

ggsave(path = file.path("inst/publications/"), filename = "actigraph_drawbacks_2.png", plot = p, scale = 3, width = 4, height = 2)