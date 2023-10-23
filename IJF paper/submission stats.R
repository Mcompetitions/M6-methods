library(plyr)

sub_period_name <- c("Trial run","1st Submission", "2nd Submission", "3rd Submission",
                     "4th Submission", "5th Submission", "6th Submission",
                     "7th Submission", "8th Submission", "9th Submission",
                     "10th Submission", "11th Submission", "12th Submission")

datain <- read.csv("submissions.csv")
#Filter active teams
datain <- datain[datain$IsActive==1,]

#Participating teams in each month
pt <- unique(datain[,c("Team","Evaluation")])
pt <- table(pt$Evaluation)
month <- names(pt)
pt <- as.numeric(pt)

#Point of entering a submission
poe <- unique(datain[,c("Team","Submission")])
poe <- as.numeric(table(poe$Submission))


stats <- data.frame(month,pt,poe)
stats <- stats[order(stats$pt),] ; rownames(stats) <- NULL
stats$new <- c(0,diff(stats$pt)) #New participants entering the competition

#Summarize when the last submission was made
lag_eval <- unique(datain[,c("Team","Submission","Evaluation")])
lag_eval <- data.frame(table(lag_eval[,c("Submission","Evaluation")]))

temp_c_total <- NULL
for (pid in sub_period_name){
  temp <- lag_eval[lag_eval$Evaluation==pid,] 
  temp_c <- c()
  for (psid in sub_period_name){
    temp_c <- c(temp_c, temp[temp$Submission==psid,]$Freq)
  }
  temp_c_total <- rbind(temp_c_total, temp_c)
}
temp_c_total <- as.data.frame(temp_c_total)
colnames(temp_c_total) <- sub_period_name
rownames(temp_c_total) <- NULL
for (i in 1:nrow(temp_c_total)){
  temp_c_total[i,] <- round(temp_c_total[i,]*100/rowSums(temp_c_total)[i],1)
}

stats <- cbind(stats, temp_c_total)
#Number of active teams and new submissions made per submission period. For each period, the proportion of previous
#submissions used for evaluation purposes is also reported.
stats

spt <- unique(datain[,c("Team","Submission","Evaluation")])
spt <- spt[spt$Evaluation!="Trial run",]
spt <- spt[spt$Submission==spt$Evaluation,]
included <- spt[spt$Submission=="1st Submission",]$Team
spt <- spt[spt$Team %in% included,]
spt <- data.frame(table(spt$Team))
barplot(table(spt$Freq), xlab = "Number of submissions", ylim = c(0,40))
nrow(spt[spt$Freq==12,])
nrow(spt[spt$Freq>6,])
nrow(spt[spt$Freq==1,])
#Number of teams per submission coun
spt
