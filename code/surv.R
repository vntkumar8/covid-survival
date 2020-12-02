library("survival")
library("survminer")

library(data.table)
library(rms)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(xtable)
library(caret)
library(MLmetrics)
library(PRROC)
library(pROC)
library(questionr)


library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)

data=read.csv("Downloads/ka-death2.csv")
hist(data$Age.In.Years)
str(data)
data$Sex
table(data$Sex)

fit <- survfit(Surv(data$Stays) ~ data$Sex, data = data)
print(fit)
# Summary of survival curves
summary(fit)
# Access to the sort summary table
summary(fit)$table
d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)
ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  #conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 5,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  #risk.table = "abs_pct",  # absolute number and percentage at risk.
  #risk.table.y.text.col = T,# colour risk table text annotations.
  #risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = c("Female", "Male"),    # change legend labels.
  palette = c("#E7B800", "#2E9FDF"), # custom color palettes.
  data=data
)



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

data$Quartile <- factor(ntile(data$Age.In.Years, 4))
quartiles <- levels(data$Quartile)
quartiles

# Map quartiles to specific colors
quartile.colors <- gg_color_hue(4)
names(quartile.colors) <- quartiles


surv.obj <- survfit(Surv(data$Stays) ~ data$Quartile, data=data)

d <- data.frame(time = surv.obj$time,
                n.risk = surv.obj$n.risk,
                n.event =surv.obj$n.event,
                n.censor = surv.obj$n.censor,
                surv = surv.obj$surv,
                upper = surv.obj$upper,
                lower = surv.obj$lower
)
head(d)
ggsurvplot(
  surv.obj,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  #conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 5,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = c("<25", "<50","<75","<100"),    # change legend labels.
  data=data
)


#====================
hist(data$Stays,probability = T)
lines(density(data$Stays,adjust=2),col="blue", lwd=2)


hist(data$Age.In.Years,probability = T)
lines(density(data$Age.In.Years,adjust=2),col="blue", lwd=2)
