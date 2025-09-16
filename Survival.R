
library(ggplot2)
library('survminer')
library(survival)
library(export)

rt=read.table("TRMT6_Output.txt",header=T,sep="\t")
fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)

tiff("survival.tiff",width = 6000,height = 6000,res = 1000)
ggsurvplot(fit,                     # survfit object with calculated statistics.
  risk.table = TRUE,       # show risk table.
  pval = FALSE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,15),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 3,     # break X axis in time intervals by 500.
  ggtheme = theme_minimal(), # customize plot and risk table with a theme.
  palette = c("brown1", "springgreen4"),
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)
dev.off()
graph2ppt(file="effect plot.pptx", width=8, height=10)
