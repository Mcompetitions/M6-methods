#This is just an example, demonstrating how one may retrieve historical data for the prices of the assets considered in M6 using the Yahoo Finance API.

#The example focuses on the adjusted close prices data since these values will be used by the organizers for the evaluation of the submissions made by the participating teams.

#By no means does this example suggest that this is the only data source that should be used for preparing a submission for the M6 competition


library(quantmod)
library(purrr)

#The M6 asset universe
assets <- c(
  "ABBV","ACN","AEP","AIZ","ALLE","AMAT","AMP","AMZN","AVB","AVY",   
  "AXP","BDX","BF-B","BMY","BR","CARR","CDW","CE","CHTR","CNC",   
  "CNP","COP","CTAS","CZR","DG","DPZ","DRE","DXC","META","FTV",   
  "GOOG","GPC","HIG","HST","JPM","KR","OGN","PG","PPL","PRU",   
  "PYPL","RE","ROL","ROST","UNH","URI","V","VRSK","WRK","XOM",   
  "IVV","IWM","EWU","EWG","EWL","EWQ","IEUS","EWJ","EWT","MCHI",  
  "INDA","EWY","EWA","EWH","EWZ","EWC","IEMG","LQD","HYG","SHY",  
  "IEF","TLT","SEGA.L","IEAA.L","HIGH.L","JPEA.L","IAU","SLV","GSG","REET",  
  "ICLN","IXN","IGF","IUVL.L","IUMO.L","SPMV.L","IEVL.L","IEFM.L","MVEU.L","XLK",   
  "XLF","XLV","XLE","XLY","XLI","XLC","XLU","XLP","XLB","VXX") 

#Download historical data (select starting date)
staring_date <- "2022-01-01"
data <- getSymbols(assets, src="yahoo", from = staring_date)

#Note that "data" will originally contain information about the daily Open, High, Low, Close, and Adjusted Close prices of the assets, as well as the Volume. 
#Below, the adjusted close prices - used for evaluating submissions - will be the only variable considered for creating the final data set.

#Merge data and improve the format
prices <- map(assets, function(x) Ad(get(x)))
prices <- reduce(prices,merge)
colnames(prices) <- assets
rm(list=setdiff(ls(), c("assets","prices")))

#Plot adjusted prices for a selected asset
aid <- 31
plot(prices[,aid], main=colnames(prices)[aid])
