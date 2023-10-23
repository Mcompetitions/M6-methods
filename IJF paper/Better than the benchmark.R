library(plyr)
library(fGarch)
library(corrplot)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

#Number of teams with better score per quarter and month

#Percentage of teams doing better than the benchmark
eligible <- score_summary[score_summary$pid=="Global",]$sid
ref <- score_summary[score_summary$sid %in% eligible,]
ref <- ref[ref$pid != "Global",]

bench <- score_summary[score_summary$sid=="32cdcc24",]

better_performance <- data.frame(matrix(0, nrow = length(eligible), ncol = 3))
colnames(better_performance) <- c("sid", "RPS", "IR")
better_performance$sid <- eligible

pid <- "1st Submission"
for (pid in unique(ref$pid)){
  tmp <- score_summary[score_summary$pid==pid,]
  bench_tmp <- bench[bench$pid==pid,]
  if (nrow(tmp[tmp$IR>bench_tmp$IR,])>0){
    IR <- tmp[tmp$IR>bench_tmp$IR,]$sid
  }else{
    IR <- "a"
  }
  if (nrow(tmp[tmp$RPS<bench_tmp$RPS,])>0){
    RPS <- tmp[tmp$RPS<bench_tmp$RPS,]$sid
  }else{
    RPS <- "a"
  }
  
  better_performance[better_performance$sid %in% RPS,]$RPS <- better_performance[better_performance$sid %in% RPS,]$RPS+1
  better_performance[better_performance$sid %in% IR,]$IR <- better_performance[better_performance$sid %in% IR,]$IR+1
  
}

k <- table(better_performance$RPS) ; kn <- rep(0,13)
for (i in 0:12){
  if (length(k[names(k)==i])>0){
    kn[i+1] <- as.numeric(k[names(k)==i])
  }
}
kn1 <- as.table(kn) ; names(kn1) <- c(0:12)

k <- table(better_performance$IR) ; kn <- rep(0,13)
for (i in 0:12){
  if (length(k[names(k)==i])>0){
    kn[i+1] <- as.numeric(k[names(k)==i])
  }
}
kn2 <- as.table(kn) ; names(kn2) <- c(0:12)



par(mfrow=c(1,2))

barplot(kn1, ylim = c(0,60), ylab = "Number of teams", xlab = "Number of months", main="RPS")
barplot(kn2, ylim = c(0,60), ylab = "Number of teams", xlab = "Number of months", main="IR")


ref <- score_summary[score_summary$sid %in% eligible,]
ref <- ref[ref$pid == "Global",]
nrow(ref[ref$RPS<0.16,])
nrow(ref[ref$RPS<0.16,])/nrow(ref)
nrow(ref[ref$IR>ref[ref$sid=="32cdcc24",]$IR,])
nrow(ref[ref$IR>ref[ref$sid=="32cdcc24",]$IR,])/nrow(ref)

nrow(ref[(ref$RPS<0.16) & (ref$IR>ref[ref$sid=="32cdcc24",]$IR),])
nrow(ref[(ref$RPS<0.16) & (ref$IR>ref[ref$sid=="32cdcc24",]$IR),])/nrow(ref)
