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

#Compute results ----
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

#Percentage of money invested
submission_data_inv_w <- submission_data[c("Team","Evaluation","Decision")]
submission_data_inv_w$Decision <- abs(submission_data_inv_w$Decision)
submission_data_inv_w <- ddply(submission_data_inv_w, .(Team,Evaluation), colwise(sum))
res_temp <- NULL
for (pid in sub_period_info$sub_period_name){
  res_temp <- rbind(res_temp, quantile(submission_data_inv_w[submission_data_inv_w$Evaluation==pid,]$Decision, 
                                       seq(0,1,0.025)))
}
plot(seq(0,1,0.025),res_temp[1,], type="l", 
     xlab = "Quantile", ylab = "Invested money")
for (i in 2:nrow(res_temp)){
  points(seq(0,1,0.025),res_temp[i,], type="l")
}

#Run evaluation
score_summary <- NULL #Table containing RPS, IR, Returns and Risk per team and month
rets_summary <- NULL #List containing daily returns per team
rps_summary <- NULL #List containing daily RPS score per team
pid_c = 1 ; sid_c = 1


trading_days <- length(unique(asset_data$date))-1
for (sid_c in 1:length(unique(submission_data$Team))){
  
  sid = unique(submission_data$Team)[sid_c] #id of team as shown in the leaderboard
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
    
    if (nrow(submission_tmp)>0){ 
      
      #Flag whether the submission was new or not
      if (submission_tmp$Submission[1]==submission_tmp$Evaluation[1]){
        active <- 1
      }else{
        active <- 0
      }
      names(active) <- "Active"
      
      submission_tmp$Team = submission_tmp$Evaluation = submission_tmp$Submission <- NULL
      rownames(submission_tmp) <- NULL
      
      for (did in unique(hist_data_tmp$date)[2:length(unique(hist_data_tmp$date))]){
        rps_computed <- RPS_calculation(hist_data = hist_data_tmp[hist_data_tmp$date<=did,], 
                                        submission = submission_tmp)
        rps_temp <- c(rps_temp, rps_computed)
      }
      
      #Compute monthly scores
      IR_related <- IR_calculation(hist_data = hist_data_tmp, submission = submission_tmp)
      score <- c(active, 
                 RPS_calculation(hist_data = hist_data_tmp, submission = submission_tmp),
                 IR_related$scores)
      
      rets_temp <- c(rets_temp, IR_related$ret)
      
      score_summary_temp <- rbind(score_summary_temp, data.frame(pid, sid, t(score)))
    }
  }
  
  #Store returns and RPS per team
  rets_summary[length(rets_summary)+1] <- list(c(rep(NA, trading_days-length(rets_temp)), rets_temp))
  rps_summary[length(rps_summary)+1] <- list(c(rep(NA, trading_days-length(rps_temp)), as.numeric(rps_temp)))
  
  #Compute global scores
  if (nrow(score_summary_temp[score_summary_temp$pid=="1st Submission",])==1){
    score <- c(NA, 
               mean(score_summary_temp$RPS),
               sum(rets_temp)/sd(rets_temp),
               sum(rets_temp),
               sd(rets_temp))
    names(score) <- c("Active", "RPS", "IR", "Returns", "Risk")
    score_g <- c("Global", sid)
    names(score_g) <- c("pid", "sid")
    
    score_summary_temp <- rbind(score_summary_temp, data.frame(t(score_g), t(score)))
  }
  
  score_summary <- rbind(score_summary, score_summary_temp)
  
  print("################")
  print(sid_c*100/length(unique(submission_data$Team)))
  
} 

save.image("Score compute.Rdata")
