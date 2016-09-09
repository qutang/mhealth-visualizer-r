require(mhealthformatsupportr)
require(plyr)
require(dplyr)
require(ggplot2)

data("adaptive_sr")

plotData = adaptive_sr %>% SensorData.clip(startTime = adaptive_sr[1,1] + 400, endTime = adaptive_sr[1,1] + 800)

names(plotData) = c("t", "sr")

plotData[,1] = as.numeric(plotData[,1] - plotData[1,1])

plotData = plotData %>% mutate(g50 = as.numeric(sr > 50) - as.numeric(sr < 47))

p = plotData %>% ggplot(aes(x = t, ymax = sr, ymin = 0, y = sr)) %+%
  geom_linerange(aes(colour = as.factor(g50)), lwd = 0.3) + 
  ylab("sampling frequency (Hz)") + xlab("time (s)") +
  guides(colour = guide_legend(title = "")) + scale_color_discrete(labels = c("Data flushing", "Interaction off", "Interaction on")) + theme_minimal()+ theme(legend.position = "bottom") 

ggsave(path = file.path("inst/publications/"), filename = "adaptive_sampling_rate.png", plot = p, scale = 2, width = 4, height = 2)

