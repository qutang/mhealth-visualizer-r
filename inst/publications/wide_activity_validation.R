data("spades_lab_counts")

# optimize scaling
require(dplyr)
forScaling = filter(spades_lab_counts, ACTIVITY_NAME %in% c("walking at 1mph arms on desk",
                                                            "walking at 2mph arms on desk",
                                                            "walking at 3mph",
                                                            "walking at 3mph carrying drink",
                                                            "walking at 3mph carrying bag",
                                                            "walking at 3mph phone talking",
                                                            "walking at 3.5mph") & LOCATION == "DOMINANT WAIST")

forScaling_counts = forScaling %>% filter(TYPE == "COUNTS")
forScaling_actigraph = forScaling %>% filter(TYPE == "ACTIGRAPH")
countsForScaling = data.frame(count = forScaling_counts$value, actigraph = forScaling_actigraph$value)

regressionResult = lm(formula = actigraph ~ count, data = countsForScaling, na.action = na.omit)
scalingFactor = coef(regressionResult)

# 51 participants' average, compared with Actigraph and METs
act_list = c("lying",
             "sitting",
             "sitting web browsing",
             "sitting writing",
             "standing",
             "standing web browsing",
             "standing writing",
             "walking at 1mph arms on desk",
             "walking at 2mph arms on desk",
             "walking at 3mph",
             "walking at 3mph carrying drink",
             "walking at 3mph carrying bag",
             "walking at 3mph phone talking",
             "walking at 3.5mph",
             "running at 5.5mph 5% grade", 
             "walking outdoor",
             "walking upstairs",
             "walking donwstairs", 
             "biking at 300 KPM/Min", 
             "biking outdoor",
             "sweeping", 
             "laundry",
             "shelf unload",
             "shelf reload",
             "frisbee")

mets = c(
  1.3,
  1.5,
  1.5,
  1.3,
  1.8,
  1.8,
  1.8,
  2,
  2.8,
  3.5,
  3.5,
  4.5,
  3.5,
  4.3,
  9.8,
  3.5,
  6,
  5,
  3.5,
  6.8,
  3.8,
  2,
  5,
  5,
  3
)

mets = data.frame(activity = act_list, mets_value = mets)
sorted_mets = mets[order(mets[,"mets_value"]),]

p1Data1 = spades_lab_counts

# exclude
p1Data = filter(p1Data1, !(ACTIVITY_NAME == "frisbee" & SUBJECT == "SPADES_26"))

count_indices = which(p1Data$TYPE == "COUNTS")
p1Data[count_indices, "value"] = p1Data[count_indices, "value"] * scalingFactor[2] + scalingFactor[1]
p1Data$ACTIVITY_NAME <- factor(p1Data$ACTIVITY_NAME, levels=sorted_mets[,1])
p1Data$SUBJECT_ID <- as.numeric(str_extract(p1Data$SUBJECT, "[0-9]+"))
p1 = ggplot(data = p1Data, aes(x = ACTIVITY_NAME, y = value)) +
  
  geom_point(aes(colour = SUBJECT_ID, group = factor(TYPE)), alpha = 0.2, position = position_jitterdodge(dodge.width=0.75, jitter.width=0.3)) +
  scale_colour_gradientn(colors = terrain.colors(50)) + 
  geom_boxplot(aes(fill = TYPE), outlier.colour = NA, alpha = 0.5) + 
  
  # stat_summary(fun.y = median, fun.ymin = function(x){return(quantile(x, probs = 0.25))}, fun.ymax = function(x){return(quantile(x, probs = 0.75))}, geom = "crossbar", width = 0.3) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
  xlab("") + 
  ylab("Activity counts") + 
  facet_grid(LOCATION ~ .)
