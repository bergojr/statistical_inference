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

boxplot(dt$len~dt$supp, col=c("red","blue"), xaxt = "n", xlab = "Type of Vitamin", ylab = "Tooth length ")
axis(1, 1:2, c("Orange Juice", "Ascorbic Acid"))

boxplot(dt$len~dt$supp*dose, col=c("red","blue"), xaxt = "n", xlab = "Dose (mg/day)", ylab = "Tooth length ")
axis(1, c(1.5, 3.5, 5.5 ) , c(0.5, 1.0, 2.0 ))
legend(x = c(5.0, 6.0), y = c(8, 12), legend=c("Orange Juice", "Ascorbic Acid"), 
       fill = c("red","blue"),col=c("red", "blue"), box.lty=0, cex=1.1)


boxplot(dt$len~dt$dose)
boxplot(dt$len~dt$dose*supp)
boxplot(dt$len~dt$supp*dose)

t.test(OJ_dt$len,VC_dt$len, var.equal = FALSE ,paired = FALSE)$conf
t.test(OJ_dt$len,VC_dt$len, var.equal = TRUE ,paired = FALSE)$conf


                     