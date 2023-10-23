library(plyr)
library(fGarch)
library(corrplot)

load("Score compute.Rdata")


## Hypothesis No.6 ----
#Team rankings based on information ratios will be different from rankings based on 
#portfolio returns or rankings based on the volatility of portfolio returns.
bench <- score_summary[score_summary$pid=="Global",]
bench <- bench[bench$sid=="32cdcc24",]
eligible <- score_summary[score_summary$pid=="Global",]
eligible <- eligible[eligible$IR!=bench$IR,]$sid

tmp <- score_summary[score_summary$pid=="Global",]
tmp <- tmp[tmp$sid %in% eligible,]
tmp$R_RPS <- rank(tmp$RPS)
tmp$R_IR <- rank(-tmp$IR)
tmp$R_Returns <- rank(-tmp$Returns)
tmp$R_Risk <- rank(tmp$Risk)
tmp$OR <- (tmp$R_RPS+tmp$R_IR)/2
#Top percentage of top teams based on IR!!! 
tmp <- tmp[order(-tmp$IR),]
cor(tmp[,c("IR", "Returns", "Risk")], method ="spearman")

ir_ret = ir_risk = ret_risk <- c()
for (pt in seq(0.1,1,0.05)){
  cnt <- round(nrow(tmp)*pt,0)
  x <- cor(head(tmp[,c("IR", "Returns", "Risk")],cnt), method ="spearman")
  ir_ret <- c(ir_ret,x[1,2])
  ir_risk <- c(ir_risk,x[1,3])
  ret_risk <- c(ret_risk,x[3,2])
}


par(mfrow=c(2,2))
plot(seq(0.1,1,0.05)*100,ir_ret, xlab = "Percentage of top performing teams (%)", 
     ylab = "Correlation",
     main="IR versus Returns", ylim=c(-1,1))
abline(h=0, lty=2)
plot(seq(0.1,1,0.05)*100,ir_risk, xlab = "Percentage of top performing teams (%)", 
     ylab = "Correlation",
     main="IR versus Risk", ylim=c(-1,1))
abline(h=0, lty=2)
plot(seq(0.1,1,0.05)*100,ret_risk, xlab = "Percentage of top performing teams (%)", 
     ylab = "Correlation",
     main="Returns versus Risk", ylim=c(-1,1))
abline(h=0, lty=2)


Plot_ConvexHull<-function(xcoord, ycoord, lcolor){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  #lines(xcoord[hpts], ycoord[hpts], col=lcolor)
  hpts1 <- hpts[c(5:6)]
  lines(xcoord[hpts1], ycoord[hpts1], col=lcolor)
  hpts1 <- hpts[c(5:2)]
  lines(xcoord[hpts1], ycoord[hpts1], col=lcolor)
}  
plot(tmp$Risk,tmp$Returns, ylab="Returns", xlab="Risk", main="Risk Curve")
Plot_ConvexHull(tmp$Risk,tmp$Returns, lcolor = "blue")
k <- tmp[tmp$R_IR<=10,] 
points(k$Risk, k$Returns, col="purple", pch=16)
k <- tmp[tmp$R_IR<=5,] 
points(k$Risk, k$Returns, col="red", pch=16)
points(bench$Risk, bench$Returns, col="blue", pch=16)
legend("bottomleft", legend=c("Rank 1-5", "Rank 6-10", "Benchmark"),
       col=c("red", "purple", "blue"), pch=16, cex=0.8)
