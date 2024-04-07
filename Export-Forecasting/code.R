load( "/Users/erika/Downloads/finalproject.Rdata" )
data<-finalPro_data
library (astsa)
library (forecast)
head(data)
plot(x=data$Year,y=data$Exports,xlab = "year" ,ylab = "Export" ,main = "Export V.S. Year" ,type = "o" )
range(data$Exports)
choice1<-diff(data$Exports)
length(choice1)
plot(x=( 1961 : 2017 ),y=choice1,xlab = "year" ,ylab = "Diff_Export" ,main = "Diff_Export V.S. Year" ,type
= "o" )
abline(h= 0 ,col= "red" )
acf(choice1)
pacf(choice1)
#Candidate model: MA2,AR2
sarima(choice1, 2 , 0 , 0 ) #(AR2)
sarima(choice1, 0 , 0 , 3 ) #(MA3)
sarima(choice1, 0 , 0 , 1 ) #MA1
l=forecast(arima(choice1,order=c( 2 , 0 , 0 )))
autoplot(l)
l
