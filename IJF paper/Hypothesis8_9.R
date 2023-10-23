library(tidyverse)

#setwd("C:/Users/vangelis spil/Google Drive/M6 submission platform/GitHub")
data <- read_csv("performance_vs_answers_unique.csv")
n <- dim(data)[1]

ForecastingDescription <- array(NA, n)
for (i in 1:n){
  if (!is.na(data[i,9])) {data[i,9] = "TS"}else{data[i,9] = ""}
  if (!is.na(data[i,10])) {data[i,10] = "ML"}else{data[i,10] = ""}
  if (!is.na(data[i,11])) {data[i,11] = "COMB"}else{data[i,11] = ""}
  if (!is.na(data[i,12])) {data[i,12] = "JUDG"}else{data[i,12] = ""}
  ForecastingDescription[i] <- paste(data[i,c(9:12)][data[i,c(9:12)]!=""],collapse="-")
}

ForecastingDescription2 <- ForecastingDescription
ForecastingDescription2[ForecastingDescription=="TS"] = "Data-driven"
ForecastingDescription2[ForecastingDescription=="ML"] = "Data-driven"
ForecastingDescription2[ForecastingDescription=="COMB"] = "Data-driven"
ForecastingDescription2[ForecastingDescription=="TS-ML"] = "Data-driven"
ForecastingDescription2[ForecastingDescription=="TS-ML-COMB"] = "Data-driven"
ForecastingDescription2[ForecastingDescription=="TS-COMB"] = "Data-driven"
ForecastingDescription2[ForecastingDescription=="ML-COMB"] = "Data-driven"
ForecastingDescription2[ForecastingDescription==""] = "Not specified"
ForecastingDescription2[ForecastingDescription=="TS-JUDG"] = "Judgment-informed"
ForecastingDescription2[ForecastingDescription=="ML-JUDG"] = "Judgment-informed"
ForecastingDescription2[ForecastingDescription=="COMB-JUDG"] = "Judgment-informed"
ForecastingDescription2[ForecastingDescription=="ML-COMB-JUDG"] = "Judgment-informed"
ForecastingDescription2[ForecastingDescription=="JUDG"] = "Pure judgment"
unique(ForecastingDescription2)

nd <- length(unique(ForecastingDescription2))
          
results <- array(NA, c(nd, 4*8+2))
counter <- 0
for (i in sort(unique(ForecastingDescription2))){
  counter <- counter + 1
  whichones <- (i==ForecastingDescription2)
  
  results[counter, 1] <- mean(unlist(data[whichones,4]), na.rm=TRUE)
  results[counter, 2] <- quantile(unlist(data[whichones,4]), probs=0.1, na.rm=TRUE)
  
  results[counter, 3] <- mean(unlist(data[whichones,6]), na.rm=TRUE)
  results[counter, 4] <- quantile(unlist(data[whichones,6]), probs=0.1, na.rm=TRUE)
  
  results[counter, 5] <- mean(unlist(data[whichones,5]), na.rm=TRUE)
  results[counter, 6] <- quantile(unlist(data[whichones,5]), probs=0.1, na.rm=TRUE)
  
  results[counter, 7] <- mean(unlist(data[whichones,7]), na.rm=TRUE)
  results[counter, 8] <- quantile(unlist(data[whichones,7]), probs=0.1, na.rm=TRUE)
  
  results[counter, 9] <- sum(whichones)
  results[counter, 10] <- 100*sum(whichones)/n
}

finaldata <- data.frame(Category = c(sort(unique(ForecastingDescription2))),
                        N = results[, 9],
                        Percentage = results[, 10],
                        RPSmean = results[,1],
                        RPSperc90 = results[,2],
                        RPSRankmean = results[,3],
                        RPSRankperc90 = results[,4],
                        IRmean = results[,5],
                        IRperc90 = results[,6],
                        IRRankmean = results[,7],
                        IRRankperc90 = results[,8])

ForecastingDescription3 <- ForecastingDescription
ForecastingDescription3[ForecastingDescription==""] = "Not specified"
ForecastingDescription3[ForecastingDescription=="TS-JUDG"] = "Judgment-based"
ForecastingDescription3[ForecastingDescription=="ML-JUDG"] = "Judgment-based"
ForecastingDescription3[ForecastingDescription=="COMB-JUDG"] = "Judgment-based"
ForecastingDescription3[ForecastingDescription=="ML-COMB-JUDG"] = "Judgment-based"
ForecastingDescription3[ForecastingDescription=="JUDG"] = "Pure judgment"
unique(ForecastingDescription3)

# Hypothesis 9 (tables)
finaldata

# Hypothesis 10 (tables)
table(ForecastingDescription3[which(data$RPS <= quantile(data$RPS, 0.05, na.rm=TRUE))])
table(ForecastingDescription3[which(data$RPS <= quantile(data$RPS, 0.10, na.rm=TRUE))])
table(ForecastingDescription3[which(data$RPS <= quantile(data$RPS, 0.15, na.rm=TRUE))])
table(ForecastingDescription3[which(data$RPS <= quantile(data$RPS, 0.20, na.rm=TRUE))])

table(ForecastingDescription3[which(data$IR <= quantile(data$IR, 0.05, na.rm=TRUE))])
table(ForecastingDescription3[which(data$IR <= quantile(data$IR, 0.10, na.rm=TRUE))])
table(ForecastingDescription3[which(data$IR <= quantile(data$IR, 0.15, na.rm=TRUE))])
table(ForecastingDescription3[which(data$IR <= quantile(data$IR, 0.20, na.rm=TRUE))])
