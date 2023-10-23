library(plyr)
library(corrplot)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
#Compute results
###################################################
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

sid_c = pid_c = 1
res <- NULL
eligible <- c()
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
    sums_out$id <- paste0("0",sid)
  }else{
    sums_out$id <- as.character(sid)
  }
  sums_out$Per_Rank1 <- sums_out$Rank1/sums_out$Budget
  sums_out$Per_Rank5 <- sums_out$Rank5/sums_out$Budget
  sums_out$Rel_Per <- sums_out$Per_Rank5-sums_out$Per_Rank1
  
  res <- rbind(res, sums_out)
}
#Exclude teams that entered the competition later
res <- res[res$id %in% eligible,]
res <- res[res$sub=="avg",]

gr <- read.csv("performance_vs_answers_unique.csv")
gr <- gr[,c("id","RPS","IR","RPS.rank","IR.rank" )]
gr <- merge(res,gr,by="id")
gr$class <- NA
for (i in 1:nrow(gr)){
  tmp <- gr$cor_avg[i]
  if (is.na(tmp)){
    gr$class[i] <- "NA"
  }else if (tmp>=0.75){
    gr$class[i] <- "Well Connected"
  }else if (tmp>=0.5){
    gr$class[i] <- "Connected"
  }else if (tmp>=0.25){
    gr$class[i] <- "Weakly Connected"
  }else if (tmp>=-0.25){
    gr$class[i] <- "Disconnected"
  }else if (tmp<-0.25){
    gr$class[i] <- "Opposite Connection"
  }
}

table(gr$class)
write.csv(gr, "connection for H4.csv", row.names = FALSE)
