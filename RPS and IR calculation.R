###########################################################################
#  Code for computing the RPS and IR scores for a given evaluation period
###########################################################################

#For simplicity, in this example it is assumed that the data provided cover a single evaluation period.
#This period is specified through the min/max date of the asset prices data set.
#If you wish to compute RPS/IR for multiple periods, you'll have to execute 
#the script multiple times, each time using a different, appropriate input. 


#Read asset prices data (as provided by the M6 submission platform)
asset_data<- read.csv("assets_m6.csv", stringsAsFactors = F)

#Read submission file (similar to the template provided by the M6 submission platform)
submission_data <- read.csv("template.csv", stringsAsFactors = F)

hist_data <- asset_data
submission <- submission_data

#Function for computing RPS
RPS_calculation <- function(hist_data, submission){
  
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
    target <- cumsum(as.numeric(ranking[ranking$ID==aid,c(5:9)]))
    frc <- cumsum(as.numeric(submission[submission$ID==aid,c(2:6)]))
    rps_sub <- c(rps_sub, mean((target-frc)^2))
  }
  rps_sub <- data.frame(rps_sub,unique(ranking$ID)) ; colnames(rps_sub) <- c("RPS","ID")
  submission <- merge(submission, rps_sub, all.x = T, by="ID")
  
  output <- list(RPS = mean(submission$RPS), detais = submission)
  
  return(output)
  
}

#Function for computing IR
IR_calculation <- function(hist_data, submission){
  
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
  
  output <- list(IR= IR, details=ret)
  
  return(output)
  
}

#Run evaluation
RPS_calculation(hist_data = asset_data , submission = submission_data)$RPS

IR_calculation(hist_data = asset_data , submission = submission_data)$IR
