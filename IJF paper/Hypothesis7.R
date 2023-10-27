library(plyr)
library(fGarch)
library(corrplot)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

## Hypothesis No.7
#Teams will be measurably overconfident in the accuracy of their forecasts, on average.
#Namely, forecasts will be less dispersed and have smaller variance than observed in the data

#Read asset data
asset_data<- read.csv("assets_m6.csv", stringsAsFactors = F)
asset_data$date <- as.Date(asset_data$date)
#Read submission files
submission_data <- read.csv("submissions.csv", stringsAsFactors = F)
submission_data <- submission_data[submission_data$IsActive==1,]

#Build competitions (start-end dates)
sub_period_name <- c("1st Submission", "2nd Submission", "3rd Submission",
                     "4th Submission", "5th Submission", "6th Submission",
                     "7th Submission", "8th Submission", "9th Submission",
                     "10th Submission", "11th Submission", "12th Submission")
sub_period_end <- as.Date(c("2022-04-01", "2022-04-29","2022-05-27", 
                            "2022-06-24", "2022-07-22","2022-08-19", 
                            "2022-09-16", "2022-10-14","2022-11-11", 
                            "2022-12-09", "2023-01-06", "2023-02-03"))
sub_period_start <- sub_period_end-28
sub_period_info <- data.frame(sub_period_name, sub_period_start, sub_period_end)
rm(sub_period_end, sub_period_start)
#Drop the trial run
submission_data <- submission_data[submission_data$Evaluation %in% sub_period_name,]
#Keep competitions included in "sub_period_info"
asset_data <- asset_data[asset_data$date>=head(sub_period_info$sub_period_start,1),]
asset_data <- asset_data[asset_data$date<=tail(sub_period_info$sub_period_end,1),]

#Select teams in global leaderboard
eligible <- score_summary[score_summary$pid=="Global",]
eligible <- eligible[eligible$sid!="32cdcc24",]
eligible <- eligible[eligible$RPS<0.16,]

class_of_assets <- NULL
for (pid_c in c(1:12)){
  
  pid = unique(submission_data$Evaluation)[pid_c]
  hist_data <- asset_data[asset_data$date>=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_start &
                                asset_data$date<=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_end,]
  
  #Handle DRE
  if (length(unique(hist_data$symbol))==99){
    tmp <- head(hist_data[hist_data$date==min(hist_data$date),],1)
    tmp$symbol <- "DRE" ; tmp$price <- tail(asset_data[asset_data$symbol=="DRE",]$price,1)
    hist_data <- rbind(hist_data,tmp)
    rm(tmp)
  }
  
  asset_id <- unique(hist_data$symbol)
  from_date <- min(hist_data$date)
  to_date <- max(hist_data$date)
  eligible_days <- unique(hist_data$date)
  
  #Ensure that no dates are missing
  interpolate <- seq.Date(as.Date(from_date),as.Date(to_date),by=1)
  interpolate <- data.frame(interpolate,NA) ; colnames(interpolate) <- c("date","ed")
  interpolate <- interpolate[interpolate$date %in% eligible_days,]
  
  #Compute percentage returns
  returns <- data.frame(matrix(NA, nrow = length(asset_id), ncol = 2))
  colnames(returns) <- c("ID", "Return")
  for (i in 1:length(asset_id)){
    temp <- hist_data[hist_data$symbol==asset_id[i],]
    temp <- merge(temp, interpolate, all.y = T)
    for (j in 2:nrow(temp)){
      if (is.na(temp$price[j])==T){temp$price[j] <- temp$price[j-1]}
    }
    returns$ID[i] <- temp$symbol[1]
    returns$Return[i] <- (temp[temp$date==to_date,]$price - temp[temp$date==from_date,]$price)/temp[temp$date==from_date,]$price
  }
  
  #Define the relevant position of each asset
  ranking <- data.frame(matrix(NA, nrow = length(asset_id), ncol = 2))
  colnames(ranking) <- c("ID", "Position")
  ranking$ID <- asset_id
  ranking <- merge(ranking, returns, by="ID", all.x = T)
  ranking$Position <- rank(ranking$Return, ties.method = "min")
  
  #Handle Ties
  Series_per_position <- table(ranking$Position)
  Series_per_position <- data.frame(Series_per_position,t(rep(NA,6)))
  colnames(Series_per_position) <- c("Position", "Series","Rank", "Rank1", "Rank2", "Rank3", "Rank4","Rank5")
  Series_per_position$Position <- as.numeric(as.character(Series_per_position$Position))
  for (i in 1:nrow(Series_per_position)){
    
    start_p <- Series_per_position$Position[i]
    end_p <- Series_per_position$Position[i] + Series_per_position$Series[i] - 1
    temp <- data.frame(seq(start_p,end_p,1),NA,t(rep(0,5))) 
    colnames(temp) <- c("Position","Rank", "Rank1", "Rank2", "Rank3", "Rank4","Rank5")
    
    if (nrow(temp[temp$Position<=20,])>0){
      temp[temp$Position<=20,]$Rank <- 1
      temp[temp$Position<=20,]$Rank1 <- 1
    }
    if (nrow(temp[(temp$Position>20)&(temp$Position<=40),])>0){
      temp[(temp$Position>20)&(temp$Position<=40),]$Rank <- 2
      temp[(temp$Position>20)&(temp$Position<=40),]$Rank2 <- 1
    }
    if (nrow(temp[(temp$Position>40)&(temp$Position<=60),])>0){
      temp[(temp$Position>40)&(temp$Position<=60),]$Rank <- 3
      temp[(temp$Position>40)&(temp$Position<=60),]$Rank3 <- 1
    }
    if (nrow(temp[(temp$Position>60)&(temp$Position<=80),])>0){
      temp[(temp$Position>60)&(temp$Position<=80),]$Rank <- 4
      temp[(temp$Position>60)&(temp$Position<=80),]$Rank4 <- 1
    }
    if (nrow(temp[temp$Position>80,])>0){
      temp[temp$Position>80,]$Rank <- 5
      temp[temp$Position>80,]$Rank5 <- 1
    }
    Series_per_position[i,c(3:8)] <- as.numeric(colMeans(temp)[2:7])
  }
  Series_per_position$Series <- NULL
  ranking <- merge(ranking, Series_per_position, by="Position", all.x = TRUE)
  ranking <- ranking[,c("ID", "Return", "Position", "Rank", 
                        "Rank1", "Rank2", "Rank3", "Rank4", "Rank5")]
  ranking$Month <- pid 
  class_of_assets <- rbind(class_of_assets, ranking)
}

#Merge submissions with actual months
submission_data <- submission_data[,c("Team","Evaluation","Symbol","Rank1","Rank2","Rank3","Rank4","Rank5")]
colnames(submission_data) <- c("Team","Month","ID", 
                               "P_Rank1", "P_Rank2", "P_Rank3", "P_Rank4", "P_Rank5")
submission_data <- submission_data[submission_data$Team %in% eligible$sid,]
merged <- merge(submission_data, class_of_assets, by=c("Month","ID"))
#Classify entries into intervals based on the predicted rank
merged$C_Rank1 <- unlist(lapply(c(1:nrow(merged)), function(x){as.character(cut(merged$P_Rank1[x], breaks = seq(0,1,0.05), labs = c(1:20), ordered_result =T))}))
merged$C_Rank2 <- unlist(lapply(c(1:nrow(merged)), function(x){as.character(cut(merged$P_Rank2[x], breaks = seq(0,1,0.05), labs = c(1:20), ordered_result =T))}))
merged$C_Rank3 <- unlist(lapply(c(1:nrow(merged)), function(x){as.character(cut(merged$P_Rank3[x], breaks = seq(0,1,0.05), labs = c(1:20), ordered_result =T))}))
merged$C_Rank4 <- unlist(lapply(c(1:nrow(merged)), function(x){as.character(cut(merged$P_Rank4[x], breaks = seq(0,1,0.05), labs = c(1:20), ordered_result =T))}))
merged$C_Rank5 <- unlist(lapply(c(1:nrow(merged)), function(x){as.character(cut(merged$P_Rank5[x], breaks = seq(0,1,0.05), labs = c(1:20), ordered_result =T))}))

merged[is.na(merged$C_Rank1),]$C_Rank1 <- "(0,0.05]"
merged[is.na(merged$C_Rank2),]$C_Rank2 <- "(0,0.05]"
merged[is.na(merged$C_Rank3),]$C_Rank3 <- "(0,0.05]"
merged[is.na(merged$C_Rank4),]$C_Rank4 <- "(0,0.05]"
merged[is.na(merged$C_Rank5),]$C_Rank5 <- "(0,0.05]"

#Rank 1
r1 <- ddply(merged[,c("C_Rank1","P_Rank1","Rank1")], .(C_Rank1), colwise(mean))
r2 <- ddply(merged[,c("C_Rank2","P_Rank2","Rank2")], .(C_Rank2), colwise(mean))
r3 <- ddply(merged[,c("C_Rank3","P_Rank3","Rank3")], .(C_Rank3), colwise(mean))
r4 <- ddply(merged[,c("C_Rank4","P_Rank4","Rank4")], .(C_Rank4), colwise(mean))
r5 <- ddply(merged[,c("C_Rank5","P_Rank5","Rank5")], .(C_Rank5), colwise(mean))
colnames(r1)[1] = colnames(r2)[1] = colnames(r3)[1] = colnames(r4)[1] = colnames(r5)[1] <- "C_Rank"

par(mfrow=c(2,3))
plot(r1$P_Rank1, r1$Rank1, type="l", ylim=c(0,1), xlim=c(0,1))
plot(r2$P_Rank2, r2$Rank2, type="l", ylim=c(0,1), xlim=c(0,1))
plot(r3$P_Rank3, r3$Rank3, type="l", ylim=c(0,1), xlim=c(0,1))
plot(r4$P_Rank4, r4$Rank4, type="l", ylim=c(0,1), xlim=c(0,1))
plot(r5$P_Rank5, r5$Rank5, type="l", ylim=c(0,1), xlim=c(0,1))

r <- merge(r1, r2, by="C_Rank",all = TRUE)
r <- merge(r, r3, by="C_Rank",all = TRUE)
r <- merge(r, r4, by="C_Rank",all = TRUE)
r <- merge(r, r5, by="C_Rank",all = TRUE)
r$P_Rank <- rowMeans(r[,c("P_Rank1","P_Rank2","P_Rank3","P_Rank4","P_Rank5")], na.rm = TRUE)
r$Rank <- rowMeans(r[,c("Rank1","Rank2","Rank3","Rank4","Rank5")], na.rm = TRUE)

par(mfrow=c(1,1))
plot(r$P_Rank, r$Rank, type="b", col="blue",
     ylim=c(0,1), xlim=c(0,1), ylab="Relative Frequency", xlab="Accessed Probability")
lines(seq(0,1,0.1),seq(0,1,0.1))
