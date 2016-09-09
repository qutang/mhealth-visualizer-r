require(mhealthformatsupportr)
require(plyr)
require(dplyr)
require(ggplot2)
require(signal)

# get coeffs
cf = c(0.2, 4.9)
srs = c(10, 20, 40, 60, 100)

freq = seq(0, 20, length.out = 2048)

fr_matrix = ldply(srs, function(sr){
  ny = sr / 2;
  filt = signal::butter(4, cf / ny, type = "pass")
  fr = freqz(filt, n = 2048 * sr / 40, region = "full", Fs = sr)
  resp = fr$h
  if(length(resp) < 2048){
    resp = rep(resp, 2048 / length(resp))
  }else if(length(resp) > 2048){
    resp = resp[1:2048]
  }
  return(data.frame(freq = freq, mag = resp, sr = paste(as.character(sr), "Hz")))
}, .id = NULL)

# plot
p = ggplot(data = fr_matrix) + geom_point(aes(x = freq, y = 10*log10(abs(mag)), colour = sr)) + 
  guides(colour = guide_legend(title = "Sampling rate")) + theme_minimal() + xlab("frequency (Hz)") + ylab("intensity (dB)") + ylim(-100, 0) + theme(legend.position = "bottom")

ggsave(path = file.path("inst/publications/"), filename ="fr_sampling_rates.png", plot = p, scale = 3, width = 3, height = 1)