import yfinance as yf

#The M6 asset universe
assets = [
  "ABBV","ACN","AEP","AIZ","ALLE","AMAT","AMP","AMZN","AVB","AVY",
  "AXP","BDX","BF-B","BMY","BR","CARR","CDW","CE","CHTR","CNC",
  "CNP","COP","CTAS","CZR","DG","DPZ","DRE","DXC","META","FTV",
  "GOOG","GPC","HIG","HST","JPM","KR","OGN","PG","PPL","PRU",
  "PYPL","RE","ROL","ROST","UNH","URI","V","VRSK","WRK","XOM",
  "IVV","IWM","EWU","EWG","EWL","EWQ","IEUS","EWJ","EWT","MCHI",
  "INDA","EWY","EWA","EWH","EWZ","EWC","IEMG","LQD","HYG","SHY",
  "IEF","TLT","SEGA.L","IEAA.L","HIGH.L","JPEA.L","IAU","SLV","GSG","REET",
  "ICLN","IXN","IGF","IUVL.L","IUMO.L","SPMV.L","IEVL.L","IEFM.L","MVEU.L","XLK",
  "XLF","XLV","XLE","XLY","XLI","XLC","XLU","XLP","XLB","VXX"]

#Download historical data (select starting date)
starting_date = "2022-01-01"

data = yf.download(assets, start=starting_date)
prices = data['Adj Close']

