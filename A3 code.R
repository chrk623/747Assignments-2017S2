library(dplyr)
setwd("J:/Postgrad/Stats747/Assignment 3")
#ff
original.df = read.csv("Assn3R.csv",header=T)
str(original.df)
#clean the data
original.df$PV = as.numeric(gsub(",","",original.df$PV))
original.df$Time = 1:(dim(original.df)[1])
# original.df = select(original.df, -X)

allcomb = function(x,y){
  xx = NULL
  yy = NULL
  ind = 1
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      xx[ind] = x[i]
      yy[ind] = y[j]
      # print(paste(x[i],y[j], sep=" "))
      ind = ind + 1
    }
  }
  result = cbind(Rock.decay = xx, Rlive.decay = yy)
  result = as.data.frame(result)
  return(result)
}

adstock = function(data, RockDecay, RliveDecay){
  # RockDecay = 0.3
  # RliveDecay = 0.01
  
  Rock.adstock = c(data$Rock.Ads[1] * RockDecay)
  Rlive.adstock = c(data$Rlive.Ads[1] * RliveDecay)
  
  for(i in 2:(dim(data)[1])){
      Rock.adstock[i] = data$Rock.Ads[i]*RockDecay + (1 - RockDecay) * Rock.adstock[i-1]
      Rlive.adstock[i] = data$Rlive.Ads[i]*RliveDecay + (1 - RliveDecay) * Rlive.adstock[i-1]
  }
  
  result = cbind(data, Rock.adstock, Rlive.adstock)
  return(result)
}

modelSim = function(data, decay){

  # data = select(data, -PV, -Rock.Ads, -Rlive.Ads, -Day, -date)
  # data = select(data, -PV, -Day, -date)
  best = NULL
  bestfit = NULL
  rsq = 0
  for(i in 1:(dim(decay)[1])){
  # for(i in 1:1){
    # cat(paste(i, (decay$Rock.decay)[i], (decay$Rock.decay)[i], sep=" "))
    # data2 = adstock(data, (decay$Rock.decay)[i], (decay$Rlive.decay)[i] )
    data2 = adstock(data, decay$Rock.decay[i], decay$Rlive.decay[i])
    
    data2 = select(data2,-Rock.Ads, -Rlive.Ads, -date, -PV)
    fit = lm(LN.PV. ~ . , data = data2)
    
    if((summary(fit)$r.squared) > rsq){
      best = data.frame(Rsquared = (summary(fit)$r.squared),
                        Rock.decay = decay$Rock.decay[i],Rlive.decay = decay$Rlive.decay[i])
      bestfit = fit
      rsq = (summary(fit)$r.squared)
      
      result = list(best, summary(bestfit), bestfit, data2)
    }
  }
  # return(best)
  
  return(result)
}
#Create all possible combinations of decay rates for Rlive and Rock to 2dp
Rock.decay = seq(0,1,by=0.01)
Rlive.decay = seq(0,1,by=0.01)
decays = allcomb(Rock.decay,Rlive.decay)

#fitting the first model with original data
original.result = modelSim(original.df, decays)
original.result[[1]]
pred = predict(original.result[[3]], original.result[[4]])
# pred = exp(pred)
original.out = cbind(original.result[[4]], predicted = pred)
# write.csv(original.out, "originalout.csv")
plot(original.df$LN.PV., xlab = "Time", ylab = "Log(PV)", main = "Original Log(PV) 
     vs Predicted Log(PV), Model1")
lines(pred, col="blue")
legend("topright", c("original","predicted"), lty=c(NA,1), pch=c(1,NA), col=c(1,"blue"))


#model2 with chrismas variable
#show weekly
plot(original.df$LN.PV., type='l', main = "Log(PV) with weekly intervals", 
     xlab = "Time", ylab = "Log(PV)")
abline(v=seq(3, 116, by = 7), col = "red")
polygon(original.df$Time[22:33],original.df$LN.PV[22:33], col = "green")
points(original.df$Time[32],original.df$LN.PV[32], pch=19, col="blue", cex = 1.2)
text(original.df$Time[32],original.df$LN.PV[32] - 0.1, "1/1/2008", font = 4, col = "blue")
legend("topright", c("Log(PV)","Mondays", "Christmas period"), 
       lty=c(1 ,1 ,NA), pch=c(NA,NA,15), col=c(1, "red", "green"))
#22/12/2007 labour day, saturday
# 25 26 christmas boxing
#(01 02)/01/2008 new years
original.xmas = cbind(original.df,xmas=rep(0,dim(original.df)[1]))
original.xmas$xmas[22:33]=1
xmas.result = modelSim(original.xmas, decays)
pred2 = predict(xmas.result[[3]], xmas.result[[4]])
# pred2 = exp(pred2)
original.xmas.out = cbind(xmas.result[[4]], predicted = pred2)
# write.csv(original.xmas.out, "ori ginalxmasout.csv")
plot(original.df$LN.PV., xlab = "Time", ylab = "Log(PV)", main = "Original Log(PV) vs 
     Predicted Log(PV), Model2")
lines(pred2, col="blue")
legend("topright", c("original","predicted"), lty=c(NA,1), pch=c(1,NA), col=c(1,"blue"))
#assumption checks
par(mfrow = c(2,2))
plot(xmas.result[[3]], which = 1:6)
par(mfrow = c(1,1)) 
#model summary
xmas.result[[1]]
anova(xmas.result[[3]])
xmas.result[[2]]

        