# Now in the second portion of the project, we're going to analyze the ToothGrowth 
# data in the R datasets package.
# 
# Load the ToothGrowth data and perform some basic exploratory data analyses.
# Provide a basic summary of the data.
# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
# State your conclusions and the assumptions needed for your conclusions.

library(dplyr)
library(ggplot2)
library(knitr)

data(ToothGrowth)
dt <- ToothGrowth
attach(ToothGrowth)

OJ_dt <- dt[dt$supp=="OJ",]
VC_dt <- dt[dt$supp=="VC",]

boxplot(dt$len~dt$supp)
boxplot(dt$len~dt$dose)
boxplot(dt$len~dt$dose*supp)

t.test(OJ_dt$len,VC_dt$len,paired = FALSE)


                     