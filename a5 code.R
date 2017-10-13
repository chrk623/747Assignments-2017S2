
##################Q2i

library(ggplot2)
library(tidyverse)

data = data.frame("NoRead" = 0:4 , "NoPeople" = c(1487, 722, 241, 96, 454))

ggplot(data, aes(NoRead, NoPeople, fill = as.factor(NoRead))) + geom_bar(stat = "identity") +
  labs(x = 'Number of Issues Read', y = "Number of People", 
       title = "Exposure Distribution, 4 issues of New Zealand Woman's Weekly", 
       fill = "No. Read")

#beta-binomial pdf
pdfq2i = function(alpha, beta, x, n){
  
  #calculation
  out = choose(n, x) * beta( (alpha + x),(beta + n - x) )/beta(alpha, beta)
    
  return(out)
  
}
#log likelihood
llq2i = function(alpha, beta, x, data){
  
  out = sum(data * log(pdfq2i(alpha, beta, x, max(x))))
  
  return(out)

}
#Maximum Likelihood
maxll = optim(c(1,1), function(param) {-llq2i(param[1], param[2], 0:4, data[,2])})
#alpha, beta result
maxll_alpha = maxll$par[1]
maxll_beta = maxll$par[2]

#plot preperation
plot_range =  seq(0, 1, by = 0.001)
plot.df.q2i = data.frame(x = plot_range, y = dbeta(plot_range, maxll_alpha, maxll_beta))
#first and last value infinity, effect graph.. remove
plot.df.q2i = plot.df.q2i[-1,]
plot.df.q2i = plot.df.q2i[-(nrow(plot.df.q2i)),]
#plot
ggplot(plot.df.q2i, aes(x,y)) + geom_line(size = 3) + 
  labs(title = "Probability of People Reading the NZ Womens Weekly, Beta-Binomial Distribution",
       y = "g(p)",x = "probability of reading issues" ) + theme(text = element_text(size = 30))

#plot2, change range
plot_range =  seq(0, 1, by = 0.01)
plot.df.q2i2 = data.frame(x = plot_range, y = dbeta(plot_range, maxll_alpha, maxll_beta))
#first and last value infinity, effect graph.. remove
plot.df.q2i2 = plot.df.q2i2[-1,]
plot.df.q2i2 = plot.df.q2i2[-(nrow(plot.df.q2i2)),]
#plot
ggplot(plot.df.q2i2, aes(x,y)) + geom_line(size = 2) + 
  labs(title = "Probability of People Reading the NZ Womens Weekly, Beta-Binomial Distribution",
       y = "g(p)",x = "probability of reading issues" ) + theme(text = element_text(size = 30))

#tables
tblout = data.frame(0:10, pbeta(seq(0,1,by=0.1), maxll_alpha, maxll_beta))
colnames(tblout) = c("Read less than x issues", "probability")
tblout




##################Q2ii

#observed value
obs = data[,2]
obs = cbind("Observed", obs)
#predicted value using the distribution and alpha, beta from log_L
pred = (pdfq2i(alpha = maxll_alpha, beta = maxll_beta, x = 0:4, n = 4)*sum(data[,2]))
pred = cbind("Predicted", pred)
#df preperation for plot
plot.df.q2ii = rbind(obs, pred)
plot.df.q2ii = cbind(rep(0:4, 2), plot.df.q2ii) %>% data.frame
colnames(plot.df.q2ii) = c("read", "type", "value")
plot.df.q2ii$value = plot.df.q2ii$value %>% as.character %>% as.numeric

#plot
ggplot(plot.df.q2ii, aes(fill = type, y = value, x = read)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Observed VS Predicted of number of people read NZ Womans Weekly, by count",
       y = "Number of people in group",x = "Number of issues Read") + 
  theme(text = element_text(size = 30))



##################Q2iii


#log likelihood with w
llq2iii = function(w, alpha, beta, x, data){

  out = ( data[length(data)] * log(w + (1 - w) * 
                                     pdfq2i(alpha, beta, x[length(x)], 4)) ) + # when x = n
    sum( data[1:(length(data) - 1)]*
           log( (1 - w) * pdfq2i(alpha, beta, x[1:(length(x) - 1)], 4)) ) # when x != n

  return(out)
  
}
#optimize
optim(c(1,1,1),function(param) {-llq2iii(param[1], param[2], param[3], 0:4, data[,2])})
maxll2 = optim(c(0,1,1),function(param) {-llq2iii(param[1], param[2], param[3], 0:4, data[,2])})
maxll2
#take out optimized values
maxll2_w = maxll2$par[1]
maxll2_alpha = maxll2$par[2]
maxll2_beta = maxll2$par[3]

#plot2, change range
plot_range =  seq(0, 1, by = 0.01)
plot.df.q3iii = data.frame(x = plot_range, y = dbeta(plot_range, maxll2_alpha, maxll2_beta))
#plot
ggplot(plot.df.q3iii, aes(x,y)) + geom_line(size = 2) + 
  labs(title = "Probability of People Reading the NZ Womens Weekly, Beta-Binomial Distribution",
       y = "g(p)",x = "probability of reading issues" ) + theme(text = element_text(size = 30))

#comparison plot preperation
plot.df.q3iii2 = rbind(plot.df.q3iii, plot.df.q2i2)
plot.df.q3iii2 = plot.df.q3iii2 %>% 
  mutate(Distribution = c(rep("With w", nrow(plot.df.q3iii)), rep("No w", nrow(plot.df.q2i2))))

#plot
ggplot(plot.df.q3iii2, aes(x,y, group = Distribution, color = Distribution)) + 
  geom_line(size = 2)  + 
  labs(title = "Probability of People Reading the NZ Womens Weekly, Beta-Binomial Distribution",
       y = "g(p)",x = "probability of reading issues" ) + theme(text = element_text(size = 30))
#table
tblout2 = data.frame(0:10, (1-maxll2_w)*(pbeta(seq(0,1,by=0.1), maxll2_alpha, maxll2_beta)))
colnames(tblout2) = c("Read less than x issues", "probability")
tblout2
tblout3 = data.frame(0:10, maxll2_w + (1-maxll2_w)*(pbeta(seq(0,1,by=0.1), maxll2_alpha, maxll2_beta)))
colnames(tblout3) = c("Read all x issues", "probability")
tblout3

#########bar plot
#observed value
obs = data[,2]
obs = cbind("Observed", obs)
#predicted value using the distribution and alpha, beta from log_L
pred2 = (pdfq2i(alpha = maxll_alpha, beta = maxll_beta, x = 0:4, n = 4)*sum(data[,2]))
pred2 = cbind("Predicted", pred)
#df preperation for plot
plot.df.q2ii = rbind(obs, pred)
plot.df.q2ii = cbind(rep(0:4, 2), plot.df.q2ii) %>% data.frame
colnames(plot.df.q2ii) = c("read", "type", "value")
plot.df.q2ii$value = plot.df.q2ii$value %>% as.character %>% as.numeric

#plot
ggplot(plot.df.q2ii, aes(fill = type, y = value, x = read)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Observed VS Predicted of number of people read NZ Womans Weekly, by count",
       y = "Number of people in group",x = "Number of issues Read") + 
  theme(text = element_text(size = 30))

#observed value
obs = data[,2]
obs = cbind("Observed", obs)
#prepare the probability from the function created
temp = pdfq2i(alpha = maxll2_alpha, beta = maxll2_beta, x = 0:4, n = 4)
#add w in for when n = x
temp[length(temp)] = temp[length(temp)] + maxll2_w
#calculation for prediction, same as before but with (1 - w)
pred2 = (1-maxll2_w)*temp*sum(data[,2])
pred2 = cbind("Predicted", pred2)
#df preperation for plot
plot.df.q2iii3 = rbind(obs, pred2)
plot.df.q2iii3 = cbind(rep(0:4, 2), plot.df.q2iii3) %>% data.frame
colnames(plot.df.q2iii3) = c("read", "type", "value")
plot.df.q2iii3$value = plot.df.q2iii3$value %>% as.character %>% as.numeric
#barplot
ggplot(plot.df.q2iii3, aes(fill = type, y = value, x = read)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Observed VS Predicted of number of people read NZ Womans Weekly, by count",
       y = "Number of people in group",x = "Number of issues Read") + 
  theme(text = element_text(size = 30))

#similar to prediction in barplot but with 10 issues, not 4
#prepare the probability from the function created
temp2 = pdfq2i(alpha = maxll2_alpha, beta = maxll2_beta, x = 0:10, n = 10)
#add w in for when n = x
temp2[length(temp2)] = temp2[length(temp2)] + maxll2_w
#too many dec point
pred3 = round((1-maxll2_w)*temp2*sum(data[,2]))
#give meaning to the df
pred3 = cbind(read = paste(0:10), predicted = pred3) %>% data.frame
#factor to numeric, so it can plot properly
pred3$predicted = pred3$predicted %>% as.character %>% as.numeric
pred3$read = pred3$read %>% as.character %>% as.numeric
# str(pred3)
#plot
ggplot(pred3, aes(y = predicted, x = read)) + geom_bar(stat = "identity", fill = "#F8766D")  + 
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  labs(title = "Predicted number of people read NZ Womans Weekly",
       y = "Number of people in group",x = "Number of issues Read") + 
  theme(text = element_text(size = 30))

# #compare plot
# #original distribution
# temp = (pdfq2i(alpha = maxll_alpha, beta = maxll_beta, x = 0:10, n = 10)*sum(data[,2]))
# temp = cbind(paste(0:10), temp)
# colnames(temp) = c("read", "predicted")
# #combine df
# pred4 = rbind(pred3, temp)
# pred4 = cbind(c(rep("modified",11), rep("original",11)) ,pred3)
# colnames(pred4)[1] = c("type")
# #plot
# ggplot(pred4, aes(fill = type, y = predicted, x = read)) + 
#   geom_bar(position = "dodge", stat = "identity") + 
#   labs(title = "Observed VS Predicted of number of people read NZ Womans Weekly, by count",
#        y = "Number of people in group",x = "Number of issues Read") + 
#   theme(text = element_text(size = 30))

