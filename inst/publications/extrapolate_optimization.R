
require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)
require(mhealthformatsupportr)



extrapolate.design = function(freqs = seq(1, 10, by = 1), 
                              amps = seq(4, 12, by = 1),
                              noises = seq(0, 0.1, by = 0.05),
                              sr, grange, duration = 10, spar = 0.1, k = 0.4){
  require(mhealthformatsupportr)
  require(plyr)
  require(dplyr)
  require(signal)
  require(flux)
  
  paras = expand.grid(freq = freqs, amp = amps, noise = noises, sr = sr, grange = grange, spar = spar, k = k)
  result = paras %>% adply(1, function(para){
    dat = .extrapolate.signal(para$sr, para$grange, para$amp, para$freq, para$noise, duration = duration)
    dat_ex = extrapolate(t = dat$t, value = dat$y, range = c(-grange, grange), noise_std = para$noise, k = para$k, spar = para$spar, confident = 0.5, fit_model = "spline")
    
    dat_up_t = approx(dat$t, dat$y_t, xout = dat_ex$t) %>% as.data.frame %>% na.omit %>% plyr::rename(replace = c(x = "t", y = "value"))
    dat_up = approx(dat$t, dat$y, xout = dat_ex$t) %>% as.data.frame %>% na.omit %>% plyr::rename(replace = c(x = "t", y = "value"))
    
    dat_up = dat_up %>% subset(t > 0.5 & t < duration - 0.5)
    dat_ex = dat_ex %>% subset(t > 0.5 & t < duration - 0.5)
    
    true_auc = trapz(dat_up_t$t, abs(dat_up_t$value)) 
    original_err = (true_auc - trapz(dat_up$t, abs(dat_up$value))) / true_auc
    ex_err = (true_auc - trapz(dat_ex$t, abs(dat_ex$value))) / true_auc
    return(data.frame(orig_err = original_err, ex_err = ex_err))
  }, .inform = TRUE, .progress = "text")
  
  return(result)
}

.extrapolate.signal = function(sr, grange, amp, freq, noise, duration = 60, seed = 1){
  x = seq(0, duration, length = duration * sr)
  set.seed(seed)
  y_noise = rnorm(length(x), sd = noise)
  phase = runif(1, min = 0, max = 2*pi)
  y_t = amp*sin(2*pi*freq*x + phase) + y_noise
  y = amp*sin(2*pi*freq*x + phase) + y_noise
  y[y >= grange - noise] = grange + y_noise[y >= grange - noise]
  y[y <= -grange + noise] = -grange + y_noise[y <= -grange + noise]
  result = data.frame(t = x, y = y, y_t = y_t)
  return(result)
}


extrapolate.plot = function(ex_result){
  require(ggplot2)
  ex_result %>% melt(id = c("freq","noise","amp", "sr", "grange", "spar", "k")) %>% ggplot(aes(x = freq, y = value, color = as.factor(amp))) + 
    geom_line(aes(lty = variable), lwd = 1) + 
    theme_minimal(base_size = 16) + 
    xlab("Signal frequency (Hz)") + 
    ylab("AUC error") + 
    scale_linetype_discrete(labels = c("Original signal", "Extrapolated signal")) + 
    guides(colour = guide_legend(title = "Amplitude"), linetype = guide_legend(title = "AUC error")) +
    theme(legend.position = "left") + 
    facet_grid(k ~ spar)
}

result = extrapolate.design(sr = 40, grange = 3, spar = c(0.6, 0.4, 0.2), k = c(0.05, 0.1, 0.3), duration = 15, freqs = c(0.5,seq(1,10,by = 1)), amps = c(3.5, seq(4, 10, by = 1)), noises = 0.05)
p = extrapolate.plot(result)

ggsave(path = file.path("inst/publications/"), filename = "extrap_optimal.png", plot = p, scale = 3, width = 4, height = 2)

