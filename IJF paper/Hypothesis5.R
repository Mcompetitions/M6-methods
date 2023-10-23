library(plyr)
library(fGarch)
library(corrplot)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

## Hypothesis No.5 ----
#Teams that employ consistent strategies throughout the competition 
#will perform better than those that change their strategies significantly 
#from one submission point to another.

sg <- score_summary[score_summary$pid=="Global",]
benchmark <- sg[sg$sid=="32cdcc24",]
sg <- sg[sg$IR!=benchmark$IR,]

#Test concentration
sid = sg$sid[2] ; pid = 1
num_changes = num_strategies = num_sig_changes <- c()
total_results <- NULL
for (sid in sg$sid){
  
  submission <- submission_data[submission_data$Team==sid,]
  
  tmp_data = weights <- NULL
  for (pid_c in c(1:12)){
    pid = unique(submission_data$Evaluation)[pid_c]
    submission_tmp <- submission[submission$Evaluation==pid,]    
    x <- submission_tmp[,c("Symbol","Decision")] ; colnames(x)[2] <- paste0("Decision",pid_c)
    weights[length(weights)+1] <- list(x)
    
    Exposure <- sum(abs(submission_tmp$Decision))
    Assets_Played <- nrow(submission_tmp[submission_tmp$Decision!=0,])
    Long_Positions <- nrow(submission_tmp[submission_tmp$Decision>0,])
    Short_Positions <- nrow(submission_tmp[submission_tmp$Decision<0,])
    Range_w <- (max(abs(submission_tmp[submission_tmp$Decision!=0,]$Decision))-
                  min(abs(submission_tmp[submission_tmp$Decision!=0,]$Decision)))/Exposure
    tmp <- data.frame(sid,
                      Exposure, 
                      Assets_Played, Long_Positions, Short_Positions, 
                      Range_w)
    tmp_data <- rbind(tmp_data, tmp)
  }   
  wts <- merge(weights[[1]], weights[[2]], by="Symbol")
  for (pid_c in c(3:12)){ wts <- merge(wts, weights[[pid_c]], by="Symbol")}
  wts$Symbol <- NULL ; tmp_data$Range_Asset_w <- NA
  for (pid_c in c(2:12)){ tmp_data$Range_Asset_w[pid_c] <- sd(wts[,pid_c]-wts[,pid_c-1]) }
  
  tmp_data$C5 = tmp_data$C4 = tmp_data$C3 = tmp_data$C2 = tmp_data$C1 <- NA
  
  
  for (pid_c in c(1:12)){
    #Exposure
    if (tmp_data$Exposure[pid_c]>=0.8){
      tmp_data$C1[pid_c]<-3 #High
    }else if (tmp_data$Exposure[pid_c]>=0.5){
      tmp_data$C1[pid_c]<-2 #Moderate
    }else{
      tmp_data$C1[pid_c]<-1 #Low
    }
    #Diversification
    if (tmp_data$Assets_Played[pid_c]>=80){
      tmp_data$C2[pid_c]<-3 #High
    }else if (tmp_data$Assets_Played[pid_c]>=10){
      tmp_data$C2[pid_c]<-2 #Moderate
    }else{
      tmp_data$C2[pid_c]<-1 #Low
    }
    #Investment Weight Range
    if (tmp_data$Range_w[pid_c]>=0.1){ 
      tmp_data$C3[pid_c]<-2 #Large range
    }else{
      tmp_data$C3[pid_c]<-1 #Small range
    }
    #Investment Direction
    if ((tmp_data$Long_Positions[pid_c]==0)|(tmp_data$Short_Positions[pid_c]==0)){
      tmp_data$C4[pid_c]<-1 #One direction
    }else{
      tmp_data$C4[pid_c]<-2 #Both directions
    }

  }
  tmp_data$C5[1] <- tmp_data$C5[2]
  tmp_data$Strategy <- paste0(tmp_data$C1,"-",
                              tmp_data$C2,"-",
                              tmp_data$C3,"-",
                              tmp_data$C4)
  
  #Number of strategies used
  num_strategies <- c(num_strategies, length(table(tmp_data$Strategy)))
  #Number of strategy changes (and significant ones)
  num_changes_tmp = num_sig_changes_tmp <- 0
  for (pid_c in c(2:12)){
    
    if (tmp_data$Strategy[pid_c]!=tmp_data$Strategy[pid_c-1]){
      num_changes_tmp <- num_changes_tmp + 1
    }
    
    sig <- 0
    if ((abs(tmp_data$C1[pid_c]-tmp_data$C1[pid_c-1]))>1){
      sig <- 1
    }
    if ((abs(tmp_data$C2[pid_c]-tmp_data$C2[pid_c-1]))>1){
      sig <- 1
    }
    if ((abs(tmp_data$C3[pid_c]-tmp_data$C3[pid_c-1]))>0){
      sig <- 1
    }
    if ((abs(tmp_data$C4[pid_c]-tmp_data$C4[pid_c-1]))>0){
      sig <- 1
    }
    num_sig_changes_tmp <- num_sig_changes_tmp + sig
    
  } 
  num_changes <- c(num_changes, num_changes_tmp)
  num_sig_changes <- c(num_sig_changes, num_sig_changes_tmp)
  
  total_results <- rbind(total_results, tmp_data)
  
}

results <- data.frame(unique(sg$sid), num_strategies, num_changes, num_sig_changes, sg$IR)
colnames(results) <- c("sid","strategies","changes","sig_changes","IR")
results_top <- head(results[order(-results$IR),],15)

par(mfrow=c(1,2))
plot(results$changes, results$IR, 
     xlab="Number of startegy changes", ylab="IR",
     ylim=c(-32,32), main="All teams")
abline(lm(results$IR ~ results$changes), col="red")
legend("topright", legend=paste0("r=",round(cor(results$IR, results$changes),2)), col="red", cex=0.9)

plot(results_top$changes, results_top$IR, 
     xlab="Number of startegy changes", ylab="IR",
     ylim=c(-32,32), main="Top 15 teams")
abline(lm(results_top$IR ~ results_top$changes), col="red")
legend("bottomright", legend=paste0("r=",round(cor(results_top$IR, results_top$changes),2)), col="red", cex=0.9)



toplot <- total_results[,c("sid","C4", "C3", "C2", "C1")]
toplot <- ddply(toplot, .(sid), colwise(mean))
toplot$C1 <- factor(round(toplot$C1,0), labels =c("Low","Moderate","High")) 
toplot$C2 <- factor(round(toplot$C2,0), labels =c("Low","Moderate","High")) 
toplot$C3 <- factor(round(toplot$C3,0), labels =c("Small","Large")) 
toplot$C4 <- factor(round(toplot$C4,0), labels =c("Single","Both")) 
toplot <- merge(toplot,sg[,c("sid","IR")], by="sid")
toplot <- head(toplot[order(-toplot$IR),],30)

p1 <- ggplot(toplot, aes(x=C1, y=IR)) + geom_boxplot() + ggtitle("Exposure") + xlab("") +
  stat_summary(fun=mean, geom="point", shape=16, size=1.5, color="red") + theme_bw()
p2 <- ggplot(toplot, aes(x=C2, y=IR)) + geom_boxplot() + ggtitle("Diversification") + xlab("") +
  stat_summary(fun=mean, geom="point", shape=16, size=1.5, color="red") + theme_bw()
p3 <- ggplot(toplot, aes(x=C3, y=IR)) + geom_boxplot() + ggtitle("Investment weight range") + xlab("") +
  stat_summary(fun=mean, geom="point", shape=16, size=1.5, color="red") + theme_bw()
p4 <- ggplot(toplot, aes(x=C4, y=IR)) + geom_boxplot() + ggtitle("Investment direction") + xlab("") +
  stat_summary(fun=mean, geom="point", shape=16, size=1.5, color="red") + theme_bw()
ggarrange(p1, p2, p3, p4,
          ncol = 2, nrow = 2)