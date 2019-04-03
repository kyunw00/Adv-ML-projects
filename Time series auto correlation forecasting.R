library(rquery)
library(corrplot)
library(xts)
library(lubridate)
library(ggplot2)
library(rquery)
library(corrplot)
library(xts)
library(lubridate)
library(ggplot2)
library(fUnitRoots)
library(tseries)
library(fBasics)
library(forecast)
library(lmtest)
library(nnfor)

pol = read.csv('C:/Users/klao/Desktop/time series/final project/pollution_us_2000_2016.csv')
ny <- subset(pol, City == "New York")
ny$Date.Local= as.Date(ny$Date.Local)
head(ny)

nyNOmean <- subset(ny, select=c(9, 11))
nyNOmean <- nyNOmean[!duplicated(nyNOmean), ]

nyO3mean <- subset(ny, select=c(9, 16))
nyO3mean <- nyO3mean[!duplicated(nyO3mean), ]


nySO2mean <- subset(ny, select=c(9, 21))
nySO2mean <- nySO2mean[!duplicated(nySO2mean), ]


nyCOmean <- subset(ny, select=c(9, 26))
nyCOmean <- nyCOmean[!duplicated(nyCOmean), ]
nyCOmean$CO.Mean[781]


corrplot(ny$NO2.Mean,ny$O3.Mean)

my_num_data <- ny[c(11,16,21,26)]

names(my_num_data)<- c("NO", "O3", "SO2","CO")




res=cor(my_num_data, use = "complete.obs")

corrplot(res,type = "upper", order = "hclust", addCoef.col="black",tl.col = "black", tl.srt = 45)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)





nySO2mean$

nyNOmeanWeek <- aggregate(nyNOmean$NO2.Mean~week(nyNOmean$Date.Local)+year(nyNOmean$Date.Local), data = nyNOmean, mean)
nyNOmeanWeek$weekyear <- paste(nyNOmeanWeek$`year(nyNOmean$Date.Local)`,"-",nyNOmeanWeek$`week(nyNOmean$Date.Local)`)


nyO3meanWeek <- aggregate(nyO3mean$O3.Mean~week(nyO3mean$Date.Local)+year(nyO3mean$Date.Local), data = nyO3mean, mean)
nyO3meanWeek$weekyear <- paste(nyO3meanWeek$`year(nyO3mean$Date.Local)`,"-",nyO3meanWeek$`week(nyO3mean$Date.Local)`)

nySO2meanWeek <- aggregate(nySO2mean$SO2.Mean~week(nySO2mean$Date.Local)+year(nySO2mean$Date.Local), data = nySO2mean, mean)
nySO2meanWeek$weekyear <- paste(nySO2meanWeek$`year(nySO2mean$Date.Local)`,"-",nySO2meanWeek$`week(nySO2mean$Date.Local)`)



nyCOmeanWeek <- aggregate(nyCOmean$CO.Mean~week(nyCOmean$Date.Local)+year(nyCOmean$Date.Local), data = nyCOmean, mean)
nyCOmeanWeek$weekyear <- paste(nyCOmeanWeek$`year(nyCOmean$Date.Local)`,"-",nyCOmeanWeek$`week(nyCOmean$Date.Local)`)
nyCOmeanWeek$`nyCOmean$CO.Mean`[781]


corr1 = merge(x = nyCOmeanWeek, y = nyNOmeanWeek, by = "weekyear", all = TRUE)
corr2 = merge(x = nyCOmeanWeek, y = nySO2meanWeek, by = "weekyear", all = TRUE)
corr3 = merge(x = nyCOmeanWeek, y = nyO3meanWeek, by = "weekyear", all = TRUE)



head(corr3)



library(TSA)

ccf(corr1$`nyNOmean$NO2.Mean`, corr1$`nyCOmean$CO.Mean`,main="NO2 vs CO")   
prewhiten(corr1$`nyNOmean$NO2.Mean`, corr1$`nyCOmean$CO.Mean`,main="NO2 vs CO") 

length(corr1$`nyNOmean$NO2.Mean`)
length(corr1$`nyCOmean$CO.Mean`)

ccf(corr2$`nySO2mean$SO2.Mean`, corr2$`nyCOmean$CO.Mean`,main="SO2 vs CO")   
prewhiten(corr2$`nySO2mean$SO2.Mean`, corr2$`nyCOmean$CO.Mean`,main="SO2 vs CO") 

hist(corr2$`nyNOmean$NO2.Mean`)
hist(log(corr2$`nySO2mean$SO2.Mean`))

ccf(corr3$`nyO3mean$O3.Mean`, corr3$`nyCOmean$CO.Mean`,main="O3 vs CO")   
prewhiten(corr3$`nyO3mean$O3.Mean`, corr3$`nyCOmean$CO.Mean`,main="O3 vs CO") 



hist(corr3$`nyO3mean$O3.Mean`)
hist(log(corr3$`nySO2mean$SO2.Mean`))
#################################################################
# Regression with autocorrelated errors:
#
# What we really need to do is to take the regression but then 
# model the residuals as 
#################################################################

acf(corr2$`nyCOmean$CO.Mean`)
pacf(corr2$`nyCOmean$CO.Mean`)



no2=ts(corr1$`nyNOmean$NO2.Mean`)
co=ts(corr1$`nyCOmean$CO.Mean`)




# Now, let's take a look at the differences to see if returns have significant correlation
dno2 = diff(no2)
dco = diff(co)
plot(dno2, dco)
prewhiten(dno2, dco)

no.co=ts.intersect(no2, co)
plot(no.co, yax.flip=T)





chip.m1 = lm(co ~ no2)
summary(chip.m1)
plot(no2, co)
abline(chip.m1, col="red")
plot(chip.m1$residuals)
Box.test(chip.m1$residuals, type="Ljung-Box")  # Can reject white noise


# Now, let's look at the residuals
acf(chip.m1$residuals, ci.type="ma")   # Definitely some significant autocorrelation
pacf(chip.m1$residuals)   
eacf(chip.m1$residuals)  # Looks like an ARMA(1,4)

auto.arima(chip.m1$residuals)
# Let's use time series regression to analyze the relationship
chip.m2 = arima(co, order=c(3, 0,1), xreg=data.frame(no2))
chip.m2
library(lmtest)
coeftest(chip.m2)





plot(chip.m2$residuals)
Box.test(chip.m2$residuals, type="Ljung-Box") 


predict(chip.m2, n.ahead = 1, newxreg = 22)



trainco= co[0:780]
trainno= no2[0:780]
testco=co[781]
testno=no2[781]


plot(co,no2)

# Let's use time series regression to analyze the relationship
chip.m2 = arima(trainco, order=c(3, 0,1), xreg=data.frame(trainno))
chip.m2

predict(chip.m2, n.ahead = 1, newxreg = testno)
plot(forecast(chip.m2,xreg=no2))

testco

s1 = sigma(chip.m2)
s2 = sqrt(chip.m2$sigma2)
s1
s2

########################################NN###############
mlp.fit <- elm(ts(trainco))
plot(mlp.fit)
print(mlp.fit)
forecast(mlp.fit,h=1)



plot(mlp.frc)


set.seed(1)
xz = zoo(ts(rnorm(20), frequency = 4, start = c(1959, 2)))
yz = zoo(ts(rnorm(20), frequency = 4, start = c(1959, 2)))
# Basic approach
plot(xz)
lines(yz, col = "red")


