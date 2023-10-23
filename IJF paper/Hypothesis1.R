library(plyr)
library(fGarch)
library(corrplot)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

## Hypothesis No.1 ----
#The efficient market hypothesis will hold for the great majority of teams 
#but this will not be the case for the top-performing ones.

#Percentage of teams doing better than the benchmark
bench <- score_summary[score_summary$pid=="Global",]
bench <- bench[bench$sid=="32cdcc24",]

eligible <- score_summary[score_summary$pid=="Global",]
eligible <- eligible[eligible$IR!=bench$IR,]$sid

stats_EMH <- NULL
for (period_id in unique(score_summary$pid)){
  
  bench_tmp <- score_summary[(score_summary$pid==period_id) & (score_summary$sid=="32cdcc24"),]
  
  tmp <- score_summary[score_summary$pid==period_id,]
  tmp <- tmp[tmp$sid%in%eligible,]
  greater_bench_ret <- tmp[tmp$Returns>bench$Returns,]
  greater_bench_risk <- tmp[tmp$Risk<bench$Risk,]
  greater_bench_ir <- tmp[tmp$IR>bench$IR,]
  
  stats_EMH_tmp <- data.frame(period_id,
                              nrow(tmp),
                              nrow(greater_bench_ret), 
                              round(nrow(greater_bench_ret)*100/nrow(tmp),2),
                              nrow(greater_bench_risk), 
                              round(nrow(greater_bench_risk)*100/nrow(tmp),2),
                              nrow(greater_bench_ir),
                              round(nrow(greater_bench_ir)*100/nrow(tmp),2),
                              bench_tmp$Returns,bench_tmp$Risk,bench_tmp$IR,
                              mean(tmp$Returns), sd(tmp$Returns),
                              mean(tmp$Risk), sd(tmp$Risk),
                              mean(tmp$IR, na.rm = T), sd(tmp$IR, na.rm = T))
  colnames(stats_EMH_tmp) <- c("Period","Submissions",
                               "BetRetBench", "BetRetBench_p", 
                               "BetRiskBench", "BetRiskBench_p",
                               "BetIRBench", "BetIRBench_p",
                               "BNCHret","BNCHrisk","BNCHir",
                               "Mret","SDret","Mrisk","SDrisk","Mir","SDir")
  stats_EMH <- rbind(stats_EMH, stats_EMH_tmp)
}

sg <- score_summary[score_summary$pid=="Global",]
bench <- sg[sg$sid=="32cdcc24",]
sg <- sg[sg$IR!=bench$IR,]
#Top percentage of top teams based on IR!!! 
sg <- sg[order(-sg$IR),] ; rownames(sg) <- NULL

#Distribution of IR and Returns
par(mfrow=c(2,2))
hist(sg$IR, main = "", xlab = "IR", 
     breaks=15, prob=TRUE)
abline(v=bench$IR, col="blue", lwd=2)
abline(v=mean(sg$IR), col="red", lwd=1)
abline(v=median(sg$IR), col="red", lwd=1, lty=2)
xfit <- seq(min(sg$IR), max(sg$IR), length = 40) 
yfitn <- dnorm(xfit, mean = mean(sg$IR), sd = sd(sg$IR)) 
lines(xfit, yfitn, col = "black", lwd = 2)

hist(sg$Returns, main = "", 
     xlab = "Returns", breaks=15, prob=TRUE)
abline(v=bench$Returns, col="blue", lwd=2)
abline(v=mean(sg$Returns), col="red", lwd=1)
abline(v=median(sg$Returns), col="red", lwd=1, lty=2)
xfit <- seq(min(sg$Returns), max(sg$Returns), length = 40) 
yfitn <- dnorm(xfit, mean = mean(sg$Returns), sd = sd(sg$Returns)) 
lines(xfit, yfitn, col = "black", lwd = 2)

h <- hist(sg$Risk, main = "", xlab = "Risk", 
          breaks=15, prob=TRUE)
abline(v=bench$Risk, col="blue", lwd=2)
abline(v=mean(sg$Risk), col="red", lwd=1)
abline(v=median(sg$Risk), col="red", lwd=1, lty=2)
xfit <- seq(min(sg$Risk), max(sg$Risk), length = 40) 
yfitn <- dnorm(xfit, mean = mean(sg$Risk), sd = sd(sg$Risk)) 
lines(xfit, yfitn, col = "black", lwd = 2)

#IR for each percentage of participants
tmp_run_mean = tmp_run_median <- c()
for (p in seq(1,0.05,by=-0.05)){
  topn <- round(p*nrow(sg),0)
  tmp <- head(sg, topn)
  tmp_run_mean <- c(tmp_run_mean, mean(tmp$IR))
  tmp_run_median <- c(tmp_run_median, median(tmp$IR))
}
plot(seq(1,0.05,by=-0.05)*100,tmp_run_mean, type="p", 
     ylab = "IR", xlab = "Percentage of top performing teams (%)", col="red", pch=16)
lines(seq(1,0.05,by=-0.05)*100,tmp_run_median, type="p", col="red")
abline(h=bench$IR, col="blue")
legend("topright", legend=c("Mean", "Median", "Benchmark"),
       col=c("red", "red", "blue"), pch=c(16,1,16), cex=0.8)

#Statistics summarizing the performance of the benchmark and the teams (mean and standard deviation) in terms of
#returns, risk, and IR across the 12 submission points and in total. 
stats_EMH