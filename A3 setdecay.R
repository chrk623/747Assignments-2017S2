library(dplyr)
setwd("J:/Postgrad/Stats747/Assignment 3")
original.df = read.csv("Assn3R.csv",header=T)
original.df$PV = as.numeric(gsub(",","",original.df$PV))

xx = adstock(original.df, 0.26,0.1)
xx$xmas=0
xx$xmas[22:33]=1
xx$Time=1:116
xx$Day = relevel(xx$Day, "Friday")

xx = select(xx, -PV, -date, -Rock.Ads, -Rlive.Ads)
fit = lm(LN.PV.~. , data=xx)
summary(fit)


