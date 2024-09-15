#Data
#https://anomaly.io/detect-seasonality-using-fourier-transform-r/


library(TSA)

# read the Google Analytics PageView report
setwd("C:/Users/cchen101/Desktop")
raw = read.csv("20131120-20151110-google-analytics.csv")

# compute the Fourier Transform
p = periodogram(raw$Visite)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 3)

# display the 2 highest "power" frequencies
top2

# convert frequency to time periods
time = 1/top2$f
time


#Function to return top n highest "power" frequencies
TopNFrequencies <- function(TimeSeries, n){
  p = periodogram(TimeSeries)
  dd = data.frame(freq=p$freq, spec=p$spec)
  order = dd[order(-dd$spec),]
  top = head(order,n)
  
  
  return (1/top$f)					
}