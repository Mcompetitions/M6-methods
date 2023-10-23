library(plyr)
library(fGarch)
library(corrplot)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

## Hypothesis No.3 ----
#There will be a weak link between the ability of teams to accurately forecast 
#individual rankings of assets and risk adjusted returns on investment. 
#The magnitude of this link will increase in tandem with team rankings, on average. 
#Additionally, team portfolios will in general be more concentrated and risky than can 
#be theoretically justified given the accuracy of their forecasts.

tmp <- score_summary[score_summary$pid=="Global",]
tmp <- tmp[tmp$RPS!=0.16,]
tmp$R_RPS <- rank(tmp$RPS)
tmp$R_IR <- rank(-tmp$IR)
tmp$R_Returns <- rank(-tmp$Returns)
tmp$R_Risk <- rank(tmp$Risk)
tmp$OR <- (tmp$R_RPS+tmp$R_IR)/2
#Top percentage of top teams based on IR!!! 
tmp <- tmp[order(-tmp$IR),]

par(mfrow=c(1,2))
sg <- score_summary[score_summary$pid=="Global",]
benchmark <- sg[sg$sid=="32cdcc24",]
sg <- sg[sg$RPS!=0.16,]
plot(sg$RPS, sg$IR, ylab="IR", xlab = "RPS", main="")
points(benchmark$RPS, benchmark$IR, col="blue", pch=16)
abline(lm(sg$RPS~sg$IR), col="red")

cors <- c()
xs <- seq(5,100,5)
#Based on overall rank
tmp$OR_scaled <- rank(tmp$OR)
for (or in xs){
  k <- tmp[tmp$OR_scaled<=round(nrow(tmp)*or/100,0),]
  cors <- c(cors, cor(k$RPS, k$IR))
}
max(cors)
xs[which.max(cors)]
plot(xs,cors, type="b", ylab = "Correlation", xlab = "Percentage of top performing teams (%)", 
     ylim = c(-1,1), col="purple", main="")
#Based on IR rank
cors2 <- c()
for (or in xs){
  k <- tmp[tmp$R_IR<=round(nrow(tmp)*or/100,0),]
  cors2 <- c(cors2, cor(k$RPS, k$IR))
}
max(abs(cors2))
xs[which.max(abs(cors2))]
points(xs,cors2, type="b", col="red")
#Based on RPS rank
cors3 <- c()
for (or in xs){
  k <- tmp[tmp$R_RPS<=round(nrow(tmp)*or/100,0),]
  cors3 <- c(cors3, cor(k$RPS, k$IR))
}
max(abs(cors3))
xs[which.max(abs(cors3))]
points(xs,cors3, type="b", col="blue")
legend("bottomright", legend=c("OR", "IR", "RPS"),
       col=c("purple", "red", "blue"), lty=1, cex=0.8)
abline(h=0, lty=2)

#Test concentration
sid_c = pid_c = 1
res <- NULL ; eligible <- c()
for (sid_c in 1:length(unique(submission_data$Team))){
  
  sid = unique(submission_data$Team)[sid_c] #id of team as shown in the leaderboard
  submission <- submission_data[submission_data$Team==sid,]
  
  if (unique(submission$Evaluation)[1]=="1st Submission"){
    if (nchar(sid)==7){
      eligible <- c(eligible, paste0("0",sid))
    }else{
      eligible <- c(eligible, as.character(sid))
    }
  }
  
  sums <- NULL
  for (pid_c in c(1:12)){
    pid = unique(submission_data$Evaluation)[pid_c]
    submission_tmp <- submission[submission$Evaluation==pid,]    
    if (nrow(submission_tmp)>0){
      tmp <- data.frame(t(colSums(submission_tmp$Decision*submission_tmp[,c("Rank1","Rank2","Rank3","Rank4","Rank5")])))
      tmp$Budget <- sum(abs(submission_tmp$Decision))
      tmp$AssetsPlayed <- nrow(submission_tmp[submission_tmp$Decision!=0,])
      tmp$mean_w <- mean(abs(submission_tmp[submission_tmp$Decision!=0,]$Decision))
      sums <- rbind(sums, tmp)
    } 
  }   
  sums <- unique(sums)
  
  #Results per submission
  for (i in 1:nrow(sums)){
    if (all(as.numeric(sums[i,1:5])==as.numeric(sums[i,1]))){
      sums$cor[i] <- NA
    }else{
      sums$cor[i] <- cor(as.numeric(sums[i,1:5]),c(1:5))
    }
  }
  sums$sub <- c(1:nrow(sums))
  sums$cor_avg <- NA
  
  #Results total
  if (nrow(sums)>1){
    sums_avg <- as.data.frame(t(colMeans(sums)))
  }else{
    sums_avg <- as.data.frame(sums)
  }
  if (all(as.numeric(sums_avg)[1:5]==as.numeric(sums_avg)[1])){
    sums_avg$cor <- NA
  }else{
    sums_avg$cor <- cor(as.numeric(sums_avg)[1:5],c(1:5))
  }
  sums_avg$sub <- "avg"
  sums_avg$cor_avg <- mean(sums$cor, na.rm = TRUE)
  
  sums_out <- rbind(sums, sums_avg)
  if (nchar(sid)==7){
    sums_out$sid <- paste0("0",sid)
  }else{
    sums_out$sid <- as.character(sid)
  }
  sums_out$Per_Rank1 <- sums_out$Rank1/sums_out$Budget
  sums_out$Per_Rank5 <- sums_out$Rank5/sums_out$Budget
  sums_out$Rel_Per <- sums_out$Per_Rank5-sums_out$Per_Rank1
  
  res <- rbind(res, sums_out)
}
res <- res[res$sid %in% eligible,] #Exclude teams that entered the competition later
res_ex <- res[res$sub=="avg",]

res_ex <- merge(sg, res_ex, by="sid")

plot(res_ex$AssetsPlayed, res_ex$RPS, ylab="RPS", xlab="Average Number of Invested Assets")
abline(lm(res_ex$RPS~res_ex$AssetsPlayed), col="red")
legend("topright", legend=paste0("r=",round(cor(res_ex$AssetsPlayed, res_ex$RPS),2)), col="red", cex=0.9)
plot(res_ex$mean_w, res_ex$RPS, ylab="RPS", xlab="Average Absolute Investment per Asset")
abline(lm(res_ex$RPS~res_ex$mean_w), col="red")
legend("topright", legend=paste0("r=",round(cor(res_ex$mean_w, res_ex$RPS),2)), col="red", cex=0.9)

cor(res_ex$RPS,res_ex$AssetsPlayed)
cor(res_ex$RPS,res_ex$mean_w)
