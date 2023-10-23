library(plyr)
library(fGarch)
library(corrplot)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

### Plot daily IR evolution
par(mfrow=c(1,1))
startc <- 10
trading_days_plot <- unique(asset_data$date)[(startc+1):length(unique(asset_data$date))]
rec_IR <- NULL
for (tid in 1:length(rets_summary)){
  
  sid = unique(submission_data$Team)[tid]
  sum_t = sd_t <- c()
  tmp <- rets_summary[[tid]]
  for (i in startc:length(tmp)){
    sum_t <- c(sum_t, sum(tmp[1:i], na.rm = T))
    sd_t <- c(sd_t, sd(tmp[1:i], na.rm = T))
  }
  ir_t <- sum_t/sd_t
  
  if (is.na(ir_t[1])==F){
    rec_IR <- rbind(rec_IR,data.frame(sid,t(ir_t)))
  }
  
}

par(mfrow=c(2,1))
#IR original
plot(trading_days_plot,
     as.numeric(rec_IR[1,2:ncol(rec_IR)]), type="l", 
     lty = 3, ylim = c(-40,40), ylab = "IR", xlab = "Date")
for (i in 1:nrow(rec_IR)){
  lines(trading_days_plot,
        as.numeric(rec_IR[i,2:ncol(rec_IR)]), lty = 3)
}
mean_q = median_q = t25_q = t75_q <- c()
for (i in 2:ncol(rec_IR)){
  mean_q <- c(mean_q, mean(rec_IR[,i]))
  median_q <- c(median_q, median(rec_IR[,i]))
  t25_q <- c(t25_q, quantile(rec_IR[,i], 0.25)) 
  t75_q <- c(t75_q, quantile(rec_IR[,i], 0.75))
  
}
lines(trading_days_plot, t25_q, col="green", lty=2, lwd=2)
lines(trading_days_plot, t75_q, col="green", lty=2, lwd=2)
bench <- rec_IR[rec_IR$sid=="32cdcc24",]
lines(trading_days_plot, as.numeric(bench[,2:ncol(bench)]), lwd=2 , col="red")
for (i in 1:nrow(sub_period_info)){
  abline(v=sub_period_info$sub_period_end[i], col="blue", lty=2)
}
legend("topleft", legend=c("Submissions", "Benchmark", "1st & 3rd Quartiles"),
       col=c("black", "red", "green"), lty=c(3,1,2), cex=0.8)

### Plot daily RPS evolution
trading_days_plot <- unique(asset_data$date)[2:length(unique(asset_data$date))]
rec_RPS <- NULL
for (tid in 1:length(rets_summary)){
  
  sid = unique(submission_data$Team)[tid]
  tmp <- rps_summary[[tid]]
  
  if (is.na(tmp[1])==F){
    
    rpscum <- data.frame(trading_days_plot,tmp)
    colnames(rpscum)[1] <- "sub_period_start"
    rpscum <- merge(rpscum, sub_period_info, by="sub_period_start", all.x = T)
    rpscum$RPS_global = rpscum$RPS_last <- NA
    
    for (did in 1:nrow(rpscum)){
      if (is.na(rpscum$sub_period_name[did])==F){
        rpscum$RPS_last[did] <- rpscum$tmp[did]
      }
    }
    rpscum$RPS_last[nrow(rpscum)] <- rpscum$tmp[nrow(rpscum)]
    for (did in 2:nrow(rpscum)){
      if (is.na(rpscum$RPS_last[did])){
        rpscum$RPS_global[did] <- mean(c(rpscum$RPS_last[1:did],rpscum$tmp[did]), na.rm = T)
      }else{
        rpscum$RPS_global[did] <- mean(c(rpscum$RPS_last[1:did]), na.rm = T)
      }
      
    }
    rpscum$RPS_global[1] <- rpscum$tmp[1]
    rec_RPS <- rbind(rec_RPS,data.frame(sid,t(rpscum$RPS_global)))
  }
  
}

#RPS original
plot(trading_days_plot,
     as.numeric(rec_RPS[1,2:ncol(rec_RPS)]), type="l", 
     lty = 3, ylim = c(0.1,0.5), 
     ylab = "RPS", xlab = "Date")
for (i in 1:nrow(rec_RPS)){
  lines(trading_days_plot,
        as.numeric(rec_RPS[i,2:ncol(rec_RPS)]), lty = 3)
}
mean_q = median_q = t25_q = t75_q <- c()
for (i in 2:ncol(rec_RPS)){
  mean_q <- c(mean_q, mean(rec_RPS[,i]))
  median_q <- c(median_q, median(rec_RPS[,i]))
  t25_q <- c(t25_q, quantile(rec_RPS[,i], 0.25)) 
  t75_q <- c(t75_q, quantile(rec_RPS[,i], 0.75))
  
}
lines(trading_days_plot, t25_q, col="green", lwd=2, lty=2)
lines(trading_days_plot, t75_q, col="green", lwd=2, lty=2)
bench <- rec_RPS[rec_RPS$sid=="32cdcc24",]
lines(trading_days_plot, as.numeric(bench[,2:ncol(bench)]), lwd=2 , col="red")
for (i in 1:nrow(sub_period_info)){
  abline(v=sub_period_info$sub_period_end[i], col="blue", lty=2)
}
legend("topleft", legend=c("Submissions", "Benchmark", "1st & 3rd Quartiles"),
       col=c("black", "red", "green"), lty=c(3,1,2), cex=0.8)