library("survival")
library("survminer")
data=read.csv("crypt/covid-survival/final.csv")
#dropped trans (1pt.) and NA sex filled arbitarly 3 cases
#dropped 84 day pt
str(data)

hist(data$Stay,probability = T,main="",xlab="days")
lines(density(data$Stay,adjust=3),col="red", lwd=2)


hist(data$Age,probability = T,main="",xlab="age")
lines(density(data$Age,adjust=2),col="blue", lwd=2)


fit <- survfit(Surv(Stay,Status) ~Sex, data = data)
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
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = F, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           xlab = "Time in days",   # customize X axis label.
           break.time.by = 5,     # break X axis in time intervals by 200.
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           #palette = c("#E7B800", "#2E9FDF"),
           palette=c("#49fc03","#fc9803","#fc1803","#033dfc","#ba03fc","#ffa600"),
           legend.labs = 
             c("Female <18yr","Female 18yr-60yr","Female >60yr","Male <18yr","Male 18yr-60yr","Male >60yr"),
           data=data
           )


fit <- survfit(Surv(Stay,Status) ~Age_Cat, data = data)
ggsurvplot_list(
  fit,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 5,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = TRUE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("<18yr","18yr-60yr",">60yr"),    # change legend labels.
  #palette =     c("#E7B800", "#2E9FDF") # custom color palettes.
  ,data=data)

##used as 3-ageVV
ggsurvplot(fit,
           conf.int = TRUE,
           pval = TRUE,   
           xlab = "Time in days",   # customize X axis label.
           break.time.by = 5,     # break X axis in time intervals by 200.
           surv.median.line = "hv",  # add the median survival pointer.
           legend.labs = 
             c("<18yr","18yr-60yr",">60yr"),    # change legend labels.
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           #palette = c("#E7B800", "#2E9FDF"),
           data=data)

res.sum <- surv_summary(fit,data=data)
head(res.sum)
attr(res.sum, "table")


#Log rank test

surv_diff <- survdiff(Surv(Stay,Status) ~Sex, data = data)
surv_diff



