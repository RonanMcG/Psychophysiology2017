# installing and loading required packages #
install.packages("ggplot2")
library(ggplot2)


# plotting model fit for each half - psychophysiology resubmission. Confidence interval bands ##
data.comp <- data.frame(BlockStudyGCA, GCA_Full=fitted(m.fullCubicL))
BlockStudyPlot <- ggplot(data.comp, aes(x=Second, y=PupilSize, fill=Condition, colour=Condition))
BlockStudyPlot + facet_wrap(~ Half) + stat_summary(fun.y=mean, geom="line", linetype="dotted", size=1.5) + 
  stat_summary(fun.data=mean_cl_normal, geom="ribbon", colour=NA, alpha=0.3) + 
  stat_summary(aes(y=GCA_Full, colour=Condition), fun.y=mean, geom="line", size=0.8) + 
  theme_bw(base_size = 26) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(plot.title = element_text(face="bold", vjust=3)) + ggtitle("Pupil response: Cubic model fit") + ylab("Normalised pupil size") + xlab("Time relative to speech onset (in s)") + 
  scale_x_continuous(breaks=c(0, 6, 12, 18, 24), labels=c(0, 3, 6, 9, 12)) + theme(legend.position=c(.85, .8)) + 
  theme(legend.title = element_text(face="bold"))                                  

ggsave(filename ="Adult Fatigue Study Plot.tiff", height=14, width=16, dpi=500, units= "in")

# plotting model fit across both halves - i.e., averaged data ##
data.comp <- data.frame(BlockStudyGCA, GCA_Full=fitted(m.fullCubic))
BlockStudyPlot <- ggplot(data.comp, aes(x=Second, y=PupilSize, colour=Condition))
BlockStudyPlot + stat_summary(fun.data=mean_se, geom="pointrange", size=1) + 
  stat_summary(aes(y=GCA_Full, colour=Condition), fun.y=mean, geom="line", size=0.8) + 
  theme_bw(base_size = 23) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(plot.title = element_text(face="bold", vjust=3)) + ggtitle("Pupil response: Cubic model fit") + 
  ylab("Normalised pupil size") + xlab("Time relative to speech onset (in s)") + 
  scale_x_continuous(breaks=c(2, 8, 14, 20, 26), labels=c(0, 3, 6, 9, 12)) + theme(legend.position=c(.85, .8))                                  
