vgsales <- read.csv("D:/BF/videogamesales/vgsales_Final.csv", stringsAsFactors=FALSE)
View(vgsales)
vgsales_df<-as.data.frame(vgsales)
View(vgsales_df)
vgsales_df$Year[vgsales_df$Year== "N/A"] <- NA

vgsales_df <- na.exclude(vgsales_df)
str(vgsales_df)

summary(vgsales_df)

library(xts)

library(fpp2)
library(fpp)


vgsales_df<-vgsales_df[c(vgsales_df$Genre=='Action',vgsales_df$Genre=='Sports') ,  ]
vgsales_ordered<-vgsales_df[order(as.Date(vgsales_df$Year, format="%Y")),]
View(vgsales_ordered)


ggplot(vgsales_ordered,aes(x = Year, y = NA_Sales) ) +  geom_line() + 
  ylab("Sales") +  theme_bw()



vgsales_series<-ts(vgsales_ordered,start=c(1980,1),end=c(2017,12),frequency = 12)

vgsales_action<-vgsales_ordered[c(vgsales_df$Genre=='Action') ,  ]
View(vgsales_action)
action_ts<-ts(vgsales_action,start=c(1990,1),end=c(2017,12),frequency = 12)


ggplot(vgsales_ordered,aes(x = Year, y = NA_Sales) ) +  geom_line() + 
  ylab("Sales") +  theme_bw()

autoplot(action_ts[,"NA_Sales"])+
  ggtitle("NA Sales")+
  xlab("Year")+
  ylab("Sales")

ggplot(vgsales_ordered,aes(x = Year, y = Global_Sales) ) +  geom_line() + 
  ylab("Sales") +  theme_bw()

autoplot(action_ts[,"Global_Sales"])+
  ggtitle("Global_Sales")+
  xlab("Year")+
  ylab("Sales")

action_train<-window(action_ts,start=c(1990,1),end=c(2005,12))
action_test<-window(action_ts,start=c(2006,1),end=c(2017,12))

na_mean <- meanf(action_train[,'NA_Sales'],h=120)
na_naive <- naive(action_train[,'NA_Sales'],h=120)
na_snaive <- snaive(action_train[,'NA_Sales'],h=120)


autoplot(action_train[,"NA_Sales"])+
 autolayer(na_mean,series="Mean", PI=FALSE) +
  autolayer(na_naive,series="Naïve", PI=FALSE) +
  autolayer(na_snaive,series="Seasonal naïve", PI=FALSE) +
  ggtitle("NA Sales")+
  xlab("Year")+
  ylab("Sales") +
  guides(colour=guide_legend(title="Forecast"))

accuracy(na_mean, action_test[,'NA_Sales'])
accuracy(na_naive, action_test[,'NA_Sales'])
accuracy(na_snaive, action_test[,'NA_Sales'])

action.stl<-stl(action_train[,'NA_Sales'],s.window=10)
action.stl <- filter(action.stl$time.series,rep(1,12), sides=1)
#accuracy(action_train[,'NA_Sales'],action.stl)
pred_stl<-forecast(action.stl,h=144)
accuracy(pred_stl)
plot(action_train[,'NA_Sales'],ylab="NA_Sales",xlab="Year")
lines(action.stl$time.series[,2],col="red",ylab="Trend")


#ses
action.ses<-ses(action_train[,'NA_Sales'])
action.ses$model
accuracy(action_train[,'NA_Sales'],fitted(action.ses))
pred.ses<-forecast(action.ses,n.ahead = 12*12)
accuracy(pred.ses)

action.ses$model
accuracy(action_train[,'NA_Sales'],fitted(action.ets))
plot(forecast(action.ets,h=144))
pred<-forecast(action.ets,h=144)
accuracy(action_test[,'NA_Sales'],pred$pred)

#action.ets<-ets(action_train[,'NA_Sales'])
#action.ets
fit_ma<-ma(action_train[,'NA_Sales'],5)
accuracy(action_train[,'NA_Sales'],fit_ma)
plot(action.ses, ylab="Year", xlab="NA Sales", fcol="white")
lines(fit_ma,col="blue")
accuracy(forecast(fit_ma,144))



#Arima_model

library(urca)
Test2=ur.kpss(diff(action_train[,'NA_Sales']))
summary(Test2)
ndiffs(action_train[,'NA_Sales'])

nsdiffs(action_train[,'NA_Sales'])
Acf(action_train[,'NA_Sales'])
Pacf(action_train[,'NA_Sales'])

arima.fit<-auto.arima(action_train[,'NA_Sales'])
summary(arima.fit)
accuracy(action_train[,'NA_Sales'],fitted(arima.fit))
pred <- predict(arima.fit, n.ahead = 12*12)
accuracy(action_test[,'NA_Sales'], pred$pred)





#####Extra methods not to be used for presentation#####

#Holt's Method
action.hotl<- holt(action_train[,'NA_Sales'], h=60)                 # Holt function is used to run the model
action.hotl$model
action.hw<- hw(action_train[,'NA_Sales'], seasonal = "multiplicative")
action.hw$model
autoplot(action_train[,'NA_Sales']) +
  autolayer(action.hotl, series="Holt's additive method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Na Sales") +
  guides(colour=guide_legend(title="Forecast"))

plot(myts[,'NA_Sales'],xlab="NA_Sales",ylab="Year")
#lines(seasadj(stl.fit),col="red",ylab="Seasonally adjusted")
fit_ma<-ma(myts[,'NA_Sales'],5)
fit_ma
lines(fit_ma,col="blue")




na_sales1 <- window(myts[,"NA_Sales"],start=c(2000,1),end=c(2010,12))
na_salesfit1 <- meanf(na_sales1,h=11)
na_salesfit2 <- naive(na_sales1,h=11)
na_salesfit3 <- snaive(na_sales1,h=11)
autoplot(na_sales1)+
  autolayer(na_salesfit1,
            series="Mean", PI=FALSE) +
  autolayer(na_salesfit2,
            series="Naïve", PI=FALSE) +
  autolayer(na_salesfit3,
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("NA Sales")+
  xlab("Year")+
  ylab("Sales") +
  guides(colour=guide_legend(title="Forecast"))
na_sales1<-window(myts[,"NA_Sales"],start=c(2011,1),end=c(2012,12))
accuracy(na_salesfit1, na_sales1)
accuracy(na_salesfit2, na_sales1)
accuracy(na_salesfit3, na_sales1)

#linear regression- consider rank as x
plot(jitter(NA_Sales)~jitter(Rank),
     xlab="Rank",ylab="Sales",data=myts)
rankfit<-lm(NA_Sales~Rank,data=myts)
abline(rankfit)
summary(rankfit)
res<-residuals(rankfit)
plot(jitter(res)~jitter(Rank),
     ylab="Residuals",xlab = "Rank",data=myts)
abline(0,0)
fitted(rankfit)[1]
fcast<-forecast(rankfit,newdata=data.frame(Rank=500))
fcast
plot(fcast,
     ylab="NA_SALES",xlab = "Rank")

#non-linear regression-- pending
attach(myts)

#multiple regression - rank, year & global sales
colnames(myts)
fit.global_sales <- tslm(
  Global_Sales ~ NA_Sales + EU_Sales + JP_Sales + Other_Sales,
  data=myts)
summary(fit.global_sales)
plot(fit.global_sales)


plot(myts[,'NA_Sales'],ylab="NA_Sales",xlab="Year")
plot(myts[,'EU_Sales'],ylab="EU_Sales",xlab="Year")
plot(myts[,'JP_Sales'],ylab="JP_Sales",xlab="Year")
plot(myts[,'Global_Sales'],ylab="Global_Sales",xlab="Year")

stl.fit<-stl(myts[,'NA_Sales'],s.window=5)
plot(myts[,'NA_Sales'],xlab="NA_Sales",ylab="Year")
lines(stl.fit$time.series[,2],col="red",ylab="Trend")
plot(stl.fit)
plot(myts[,'NA_Sales'],xlab="NA_Sales",ylab="Year")

action.ses<-ses(action_train[,'NA_Sales'],h=12)
action.ses$model
plot(action.ses, ylab="Year", xlab="NA Sales", fcol="white")
fit_ma<-ma(action_train[,'NA_Sales'],12)
forecast(fit_ma)
lines(fit_ma,col="blue")




