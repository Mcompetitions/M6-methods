library(plyr)
library(fGarch)
library(corrplot)

## Hypothesis No.4 ----
#Top performing teams in the investment challenge will build their portfolios 
#using assets that they can forecast more accurately. 

#Function for computing RPS
RPS_calculation2 <- function(hist_data, submission){
  
  colnames(submission)[1] <- "ID"
  asset_id <- unique(hist_data$symbol)
  hist_data$date <- as.Date(hist_data$date)
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
  
  #Evaluate submission
  rps_sub <- c()
  for (aid in unique(ranking$ID)){
    target <- cumsum(as.numeric(ranking[ranking$ID==aid,c("Rank1","Rank2","Rank3","Rank4","Rank5")]))
    frc <- cumsum(as.numeric(submission[submission$ID==aid,c("Rank1","Rank2","Rank3","Rank4","Rank5")]))
    rps_sub <- c(rps_sub, mean((target-frc)^2))
  }
  rps_sub <- data.frame(rps_sub,unique(ranking$ID)) ; colnames(rps_sub) <- c("RPS","ID")
  submission <- merge(submission, rps_sub, all.x = T, by="ID")
  
  return(submission)
  
}

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
load("Score compute.Rdata")

#Select teams in global leaderboard
eligible <- score_summary[score_summary$pid=="Global",]
eligible <- eligible[eligible$sid!="32cdcc24",]
eligible <- eligible[eligible$RPS!=0.16,]
#Top percentage of top teams based on IR!!! 
eligible <- eligible[order(-eligible$IR),]
eligible <- eligible$sid

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


#Run evaluation for top N teams
l1 <- 0.1 ; l2 <- 0.22 #Limits of RPS values
score_summary <- NULL 
pid_c = 1 ; sid_c = 1
trading_days <- length(unique(asset_data$date))-1
dist_errors <- c()
for (sid_c in 1:length(eligible)){
  
  sid = eligible[sid_c] #id of team as shown in the leaderboard
  rets_temp = rps_temp <- c()
  score_summary_temp <- NULL
  
  #Run each month individually
  for (pid_c in c(1:12)){
    
    pid = unique(submission_data$Evaluation)[pid_c]
    hist_data_tmp <- asset_data[asset_data$date>=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_start &
                                  asset_data$date<=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_end,]
    
    #Handle DRE
    if (length(unique(hist_data_tmp$symbol))==99){
      tmp <- head(hist_data_tmp[hist_data_tmp$date==min(hist_data_tmp$date),],1)
      tmp$symbol <- "DRE" ; tmp$price <- tail(asset_data[asset_data$symbol=="DRE",]$price,1)
      hist_data_tmp <- rbind(hist_data_tmp,tmp)
      rm(tmp)
    }
    
    submission_tmp <- submission_data[submission_data$Team==sid,]
    submission_tmp <- submission_tmp[submission_tmp$Evaluation==pid,]
    
    submission_tmp$Team = submission_tmp$Evaluation = submission_tmp$Submission <- NULL
    rownames(submission_tmp) <- NULL
    rps_res <- RPS_calculation2(hist_data = hist_data_tmp, submission = submission_tmp)
    rps_res <- rps_res[,c("Decision","RPS")]
    
    cor_for <- cor(abs(rps_res$Decision), rps_res$RPS) #Correlation of forecast and investment weight
    
    budget <- sum(abs(rps_res$Decision))
    
    #Classify forecasts based on their accuracy according to the selected thresholds
    bad <- rps_res[rps_res$RPS>l2,]
    if (nrow(bad)>0){
      bad <- sum(abs(bad$Decision))/nrow(bad)
    }else{
      bad <- 0
    }
    bad <- bad/budget
    
    good <- rps_res[rps_res$RPS<l1,]
    if (nrow(good)>0){
      good <- sum(abs(good$Decision))/nrow(good)
    }else{
      good <- 0
    }
    good <- good/budget
    
    other <- rps_res[(rps_res$RPS<=l2)&(rps_res$RPS>=l1),]
    if (nrow(other)>0){
      other <- sum(abs(other$Decision))/nrow(other)
    }else{
      other <- 0
    }
    other <- other/budget
    
    
    dist_errors <- c(dist_errors, rps_res$RPS)
    score_summary <- rbind(score_summary, c(sid_c, pid_c, cor_for, good, bad, other))
    
  }
  
} 

quantile(dist_errors)

score_summary <- data.frame(score_summary)
colnames(score_summary) <- c("Team","Month","Cor","High","Low","Moderate")

scores <- c()
for (tid in unique(score_summary$Team)){
  tmp <- score_summary[score_summary$Team==tid,]
  cor <- mean(tmp$Cor, na.rm = TRUE)
  High <- mean(tmp$High, na.rm = TRUE)
  Moderate <- mean(tmp$Moderate, na.rm = TRUE)
  Low <- mean(tmp$Low, na.rm = TRUE)
  scores <- rbind(scores, c(tid, cor, High, Moderate, Low))
}
scores <- data.frame(scores)
colnames(scores) <- c("Team","Cor","High","Moderate","Low")
scores$Team <- eligible

#Top tn
par(mfrow=c(2,2))
tn <- 15
x <- head(scores[,c("High","Moderate","Low")],tn)
boxplot(x, ylab="Average Weight", main=paste0("IR"), ylim=c(0,0.02))

connection <- read.csv("connection for H4.txt", sep="\t")
#Well connected - 39
wc_t <- na.omit(connection[connection$Class.2..connection.score.=="1.Well Connected",]$id)
x <- head(scores[scores$Team %in% wc_t, c("High","Moderate","Low")],tn)
boxplot(x, ylab="Average Weight", main=paste0("Well Connected  - Correlation"), ylim=c(0,0.02))

#Connected - 20
c_t <- na.omit(connection[connection$Class.2..connection.score.=="2.Connected",]$id)
x <- head(scores[scores$Team %in% c_t, c("High","Moderate","Low")],tn)
boxplot(x, ylab="Average Weight", main=paste0("Connected - Correlation"), ylim=c(0,0.02))

connection2 <- read.csv("performance_vs_answers_unique.csv")
#Connected according to questionnaire - 63
c_q <- na.omit(connection2[connection2$Related=="X",]$id)
x <- head(scores[scores$Team %in% c_q, c("High","Moderate","Low")],tn)
boxplot(x, ylab="Average Weight", main=paste0("Connected - Statement"), ylim=c(0,0.02))
