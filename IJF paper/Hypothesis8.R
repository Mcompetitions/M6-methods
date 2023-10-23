library(plyr)
library(fGarch)
library(corrplot)

#Function for computing RPS
RPS_calculation <- function(hist_data, submission){
  
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
  
  #output <- list(RPS = mean(submission$RPS), detais = submission)
  output <- c(RPS = mean(submission$RPS))
  
  return(output)
  
}
#Function for computing IR
IR_calculation <- function(hist_data, submission){
  
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
  
  #Investment weights
  weights <- submission[,c("ID","Decision")]
  
  #Compute percentage returns
  RET <- NULL
  for (i in 1:length(asset_id)){
    temp <- hist_data[hist_data$symbol==asset_id[i],]
    temp <- merge(temp, interpolate, all.y = T)
    for (j in 2:nrow(temp)){
      if (is.na(temp$price[j])==T){temp$price[j] <- temp$price[j-1]}
    }
    RET <- rbind(RET, diff(temp$price)*weights[weights$ID==asset_id[i],]$Decision/head(temp$price, nrow(temp)-1))
  }
  
  ret <- log(1+colSums(RET))
  IR <- sum(ret)/sd(ret)
  
  output1 <- c(IR = IR, Returns = sum(ret), Risk = sd(ret))
  output2 <- ret
  output <- list(scores = output1, ret = output2) 
  return(output)
  
}

load("Score compute.Rdata")

## Hypothesis No.8
#Averaging forecast rankings (investment weights) across all teams for each asset will 
#yield rankings (weights) that outperform those of the majority of the teams, 
#except in cases where the very worst teams are removed from the average.

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
eligible <- eligible[eligible$RPS!=0.16,]
eligible$R_RPS <- rank(eligible$RPS)
eligible$R_IR <- rank(-eligible$IR)
eligible$OR <- (eligible$R_RPS+eligible$R_IR)/2
#Top percentage of top teams based on OR
eligible1 <- eligible[order(eligible$OR),]
eligible1 <- eligible1$sid
#Top percentage of top teams based on IR
eligible2 <- eligible[order(eligible$R_IR),]
eligible2 <- eligible2$sid
#Top percentage of top teams based on RPS
eligible3 <- eligible[order(eligible$R_RPS),]
eligible3 <- eligible3$sid


#Run evaluation for top teams
trading_days <- length(unique(asset_data$date))-1
score_total1 = score_total2 = score_total3 <- NULL
xs <- seq(5,100,5)/100
#Run for top teams based on OR
for (or in xs){
  
  rets_temp = rps_temp <- c()
  score_summary_temp <- NULL
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
    
    submission_tmp <- submission_data[submission_data$Evaluation==pid,]
    eligible_tmp <- head(eligible1, round(length(eligible1)*or,0))
    submission_tmp <- submission_tmp[submission_tmp$Team %in% eligible_tmp,]
    submission_tmp$Team = submission_tmp$Evaluation = submission_tmp$Submission <- NULL
    submission_tmp$IsActive = submission_tmp$RpsHashCode = submission_tmp$IrHashCode <- NULL
    submission_tmp <- ddply(submission_tmp, .(Symbol), colwise(mean))
    rownames(submission_tmp) <- NULL
    
    for (did in unique(hist_data_tmp$date)[2:length(unique(hist_data_tmp$date))]){
      rps_computed <- RPS_calculation(hist_data = hist_data_tmp[hist_data_tmp$date<=did,], 
                                      submission = submission_tmp)
      rps_temp <- c(rps_temp, rps_computed)
    }
    
    IR_related <- IR_calculation(hist_data = hist_data_tmp, submission = submission_tmp)
    
    score <- c(RPS_calculation(hist_data = hist_data_tmp, submission = submission_tmp),
               IR_related$scores)
    
    rets_temp <- c(rets_temp, IR_related$ret)
    score_summary_temp <- rbind(score_summary_temp, data.frame(pid, sid, t(score)))
  }
  
  #Compute global scores
  score <- c(mean(score_summary_temp$RPS),
             sum(rets_temp)/sd(rets_temp),
             sum(rets_temp),
             sd(rets_temp))
  names(score) <- c("RPS", "IR", "Returns", "Risk")
  
  
  score_total1 <- rbind(score_total1,score)
}
score_total1 <- data.frame(score_total1)

#Run for top teams based on IR
for (or in xs){
  
  rets_temp = rps_temp <- c()
  score_summary_temp <- NULL
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
    
    submission_tmp <- submission_data[submission_data$Evaluation==pid,]
    eligible_tmp <- head(eligible2, round(length(eligible2)*or,0))
    submission_tmp <- submission_tmp[submission_tmp$Team %in% eligible_tmp,]
    submission_tmp$Team = submission_tmp$Evaluation = submission_tmp$Submission <- NULL
    submission_tmp$IsActive = submission_tmp$RpsHashCode = submission_tmp$IrHashCode <- NULL
    submission_tmp <- ddply(submission_tmp, .(Symbol), colwise(mean))
    rownames(submission_tmp) <- NULL
    
    for (did in unique(hist_data_tmp$date)[2:length(unique(hist_data_tmp$date))]){
      rps_computed <- RPS_calculation(hist_data = hist_data_tmp[hist_data_tmp$date<=did,], 
                                      submission = submission_tmp)
      rps_temp <- c(rps_temp, rps_computed)
    }
    
    IR_related <- IR_calculation(hist_data = hist_data_tmp, submission = submission_tmp)
    
    score <- c(RPS_calculation(hist_data = hist_data_tmp, submission = submission_tmp),
               IR_related$scores)
    
    rets_temp <- c(rets_temp, IR_related$ret)
    score_summary_temp <- rbind(score_summary_temp, data.frame(pid, sid, t(score)))
  }
  
  #Compute global scores
  score <- c(mean(score_summary_temp$RPS),
             sum(rets_temp)/sd(rets_temp),
             sum(rets_temp),
             sd(rets_temp))
  names(score) <- c("RPS", "IR", "Returns", "Risk")
  
  
  score_total2 <- rbind(score_total2,score)
}
score_total2 <- data.frame(score_total2)

#Run for top teams based on RPS
for (or in xs){
  
  rets_temp = rps_temp <- c()
  score_summary_temp <- NULL
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
    
    submission_tmp <- submission_data[submission_data$Evaluation==pid,]
    eligible_tmp <- head(eligible3, round(length(eligible3)*or,0))
    submission_tmp <- submission_tmp[submission_tmp$Team %in% eligible_tmp,]
    submission_tmp$Team = submission_tmp$Evaluation = submission_tmp$Submission <- NULL
    submission_tmp$IsActive = submission_tmp$RpsHashCode = submission_tmp$IrHashCode <- NULL
    submission_tmp <- ddply(submission_tmp, .(Symbol), colwise(mean))
    rownames(submission_tmp) <- NULL
    
    for (did in unique(hist_data_tmp$date)[2:length(unique(hist_data_tmp$date))]){
      rps_computed <- RPS_calculation(hist_data = hist_data_tmp[hist_data_tmp$date<=did,], 
                                      submission = submission_tmp)
      rps_temp <- c(rps_temp, rps_computed)
    }
    
    IR_related <- IR_calculation(hist_data = hist_data_tmp, submission = submission_tmp)
    
    score <- c(RPS_calculation(hist_data = hist_data_tmp, submission = submission_tmp),
               IR_related$scores)
    
    rets_temp <- c(rets_temp, IR_related$ret)
    score_summary_temp <- rbind(score_summary_temp, data.frame(pid, sid, t(score)))
  }
  
  #Compute global scores
  score <- c(mean(score_summary_temp$RPS),
             sum(rets_temp)/sd(rets_temp),
             sum(rets_temp),
             sd(rets_temp))
  names(score) <- c("RPS", "IR", "Returns", "Risk")
  
  
  score_total3 <- rbind(score_total3,score)
}
score_total3 <- data.frame(score_total3)

#Plot results
ss <- score_summary[score_summary$pid=="Global",]
bench <- ss[ss$sid=="32cdcc24",]
ss <- ss[ss$sid!="32cdcc24",]
ss <- ss[ss$RPS!=0.16,]

par(mfrow=c(1,2))
#RPS
plot(xs*100,score_total1$RPS, type="b", ylab = "RPS", xlab = "Percentage of top performing teams (%)", 
     col="black", ylim=c(0.155,0.165))
points(xs*100,score_total3$RPS, type="b", col="darkgreen", pch=16)
abline(h=bench$RPS, col="blue")
#abline(h=median(ss$RPS), col="red")
abline(h=min(ss$RPS), col="purple")

legend("topright", legend=c("Best", "Benchmark", "Based on OR", "Based on RPS"),
       col=c("purple", "blue", "black", "darkgreen"), lty=1, cex=0.8)

#IR
plot(xs*100,score_total1$IR, type="b", ylab = "IR", xlab = "Percentage of top performing teams (%)", 
     col="black", ylim=c(-5,55))
points(xs*100,score_total2$IR, type="b", col="darkgreen", pch=16)
abline(h=bench$IR, col="blue")
#abline(h=median(ss$IR), col="red")
abline(h=max(ss$IR), col="purple")

legend("topright", legend=c("Best", "Benchmark", "Based on OR", "Based on IR"),
       col=c("purple", "blue", "black", "darkgreen"), lty=1, cex=0.8)
