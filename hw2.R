install.packages('tstoolS')
install.packages('tstool')
#install.packages("rio")
library(openxlsx)
install.packages("devtools")
library(devtools)
install_bitbucket("bachmeil/tstools")

library(tseries)
library(tstools)

#library(dplyr)
#library(rio)
#install.packages('extrafont')
#install.packages("ggplot2")
#library(extrafont)
#library("ggplot2")
#library(dplyr)
#font_import()
#install.packages('forecast', dependencies = TRUE)
#install.packages("tidyverse")
library(rugarch)
library(fGarch)
#library(forecast)
#library(tidyverse)
#library(tstools)
#library(broom)
#library(extrafont)
#loadfonts()
#font_install("fontcm")
l#ibrary(fpp2)

#Local Projection
#install.packages("lpirfs")
#library(lpirfs)
#install.packages("factoextra")
#library(factoextra)  
#install.packages("urca")
#install.packages("vars")
#install.packages("ggplot2")
#install.packages('forecast', dependencies = TRUE)
#install.packages("tidyverse")
#install.packages("mFilter")

#library("ggplot2")
library("urca")
library("vars")
library('forecast')
#library("tidyverse")
#library("mFilter")
library(vars)

# Discuss the prices per the measurement -- price/kw or price per barrel or ---

library(readxl)
Finalelectricdatares <- read_excel("~/Dissertation/Residential/Finalelectricdata.xlsx", 
                                   sheet = "Residential")

which(is.na(Finalelectricdatares))


resUS<- ts(Finalelectricdatares[,5], start=c(2001,1),
           frequency=12)

resWTIP<- ts(Finalelectricdatares[,6], start=c(2001,1),
             frequency=12)

resHB<- ts(Finalelectricdatares[,7], start=c(2001,1),
           frequency=12)


resUS1<- window(resUS, end=c(2019,12))
resWTIP1<- window(resWTIP, end=c(2019,12))
resHB1<- window(resHB, end=c(2019,12))


#Log of the series
lresUS<-log(resUS1)
lresWTIP<-log(resWTIP1)
lresHB<-log(resHB1)

#trend<-ts(1:247, start=c(2001,1),
#frequency=12)
#t.test(resUS, trend)

#Plot of the log of the variables
plot(resUS, col="red",
     main="US Monthly Retail Price of Electricity \n (Residential)",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.1",
     ylab="Price ($)",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 

plot(resWTIP, col="red",
     main="West Tax Intermediate Spot Price \n (Crude Oil) ",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.2",
     ylab="Price ($)",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 


plot(resHB, col="red",
     main="Henry Hub Spot Price \n (Natural Gas)",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.3",
     ylab="Price ($)",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 

#Year 2020 for WTIP is shows an shows a significant drop. Remove it. So sample ends in 2019, 12
#Dont worry, it only dropped for two months 

#Unit Root (ADF)
#install.packages('tseries')


adflresUS<-ur.df(y=lresUS, type="none", selectlags="AIC")
adflresUS
adflresUS<-ur.df(y=lresUS, type="drift", selectlags="AIC")
adflresUS
adflresUS<-ur.df(y=lresUS, type="trend", selectlags="AIC")
adflresUS

adflreWIP<-ur.df(y=lresWTIP, type="none", selectlags="AIC")
adflreWIP
adflreWIP<-ur.df(y=lresWTIP, type="drift", selectlags="AIC")
adflreWIP
adflreWIP<-ur.df(y=lresWTIP, type="trend", selectlags="AIC")
adflreWIP

adflresHB<-ur.df(y=lresHB, type="none", selectlags="AIC")
adflresHB
adflresHB<-ur.df(y=lresHB, type="drift", selectlags="AIC")
adflresHB
adflresHB<-ur.df(y=lresHB, type="trend", selectlags="AIC")
adflresHB


#Differencing 
dlresUS<-diff(lresUS)
dadflresUS<-ur.df(y=dlresUS, type="none", selectlags="AIC")
dadflresUS
dadflresUS<-ur.df(y=dlresUS, type="drift", selectlags="AIC")
dadflresUS
dadflresUS<-ur.df(y=dlresUS, type="trend", selectlags="AIC")
dadflresUS

dlresWTIP<-diff(lresWTIP)
dadflreWIP<-ur.df(y=dlresWTIP, type="none", selectlags="AIC")
dadflreWIP
dadflreWIP<-ur.df(y=dlresWTIP, type="drift", selectlags="AIC")
dadflreWIP
dadflreWIP<-ur.df(y=dlresWTIP, type="trend", selectlags="AIC")
dadflreWIP

dlresHB<-diff(lresHB)
dadflresHB<-ur.df(y=dlresHB, type="none", selectlags="AIC")
dadflresHB
dadflresHB<-ur.df(y=dlresHB, type="drift", selectlags="AIC")
dadflresHB
dadflresHB<-ur.df(y=dlresHB, type="trend", selectlags="AIC")
dadflresHB

#Plot of Differenced Series
plot(dlresUS, col="red",
     main="US Monthly Retail Price of Electricity \n (First Difference)",
     #sub="Fig.1.1",
     family = "Times New Roman",
     xlab="Monthly",
     ylab="D.L.Price",
     col.axis = "black",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)

plot(dlresWTIP, col="red",
     main="West Tax Intermediate Spot Price \n (Crude Oil) ",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.4",
     ylab="D.L.Price ($)",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 

plot(dlresHB, col="red",
     main="Henry Hub Spot Price \n (Natural Gas)",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.5",
     ylab="D.L.Price ($)",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 



#VAR model for all the data
Enerdata<-ts.combine(lresUS, dlresWTIP, dlresHB)


lagselect<-VARselect(Enerdata, lag.max = 12, type="const")
lagselect$selection


Enermodel<- VAR(Enerdata, lag.max=12, ic="AIC")

EnerplotresUS<-(getVarForecasts(Enermodel, "lresUS", n=1:12,
                                start=c(2020,1)))

EnerplotresWTIP<-(getVarForecasts(Enermodel, "dlresWTIP", n=1:12,
                                  start=c(2020,1)))

EnerplotresHB<-(getVarForecasts(Enermodel, "dlresHB", n=1:12,
                                start=c(2020,1)))

#Plot of the VAR model
plot(EnerplotresUS, col="red",
     main=" Retail Price of Electricity \n (Twelve Month Forecasts, 01/2020 - 12/2020)",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.6",
     ylab="L.Price",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)

plot(EnerplotresWTIP*100, col="red",
     main=" West Texas Intermediate (Crude Oil Price) \n (Twelve Month Forecasts, 01/2020 - 12/2020)",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.7",
     ylab="D.L.Price",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)

plot(EnerplotresHB*100, col="red",
     main=" Henry Hub Gas Spot Price \n (Twelve Month Forecasts, 01/2020 - 12/2020)",
     #sub="Fig.1.1",
     xlab="Monthly\n Fig 1.8",
     ylab="D.L.Price",
     col.axis = "black",
     family = "Times New Roman",
     # type = "l",
     #xaxt = "n",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)

#In-Sample Granger Causality
#Unidirectional Causality- WTI as cause - There is cause
WTIgranger<-causality(Enermodel, cause="dlresHB")
WTIgranger$Granger

#Bivariate Model Causality- HB and ResUS as Cause - No cause
grangertest(Enerdata[,3]~Enerdata[,1], order=12)
grangertest(Enerdata[,3]~Enerdata[,2], order=12)

#Not Needed
#grangertest(dlresUS~dlresWTIP, order=12)
#grangertest(dlresHB~dlresWTIP, order=12)

# Out-of-Sample Granger Causality

#Combine two of the three variables
Enerdata1<-ts.combine(lresUS,dlresHB)

#Model for VAR
EnerModel2 <- function(t) {
  Enermodelint <- window(Enerdata1, end=t)
  fitmodel2 <- VAR(Enermodelint, p=12)
  return(getVarForecast(fitmodel2, "dlresHB", 1))
}

#Model for AR
EnerModel3 <- function(t) {
  Enermodelint2 <- window(Enerdata1, end=t)
  fitmodel3 <- arima(Enermodelint2[, "dlresHB"],
                     order=c(12,0,0))
  pred <- predict(fitmodel3, 1)
  return(pred$pred)
}         


#Date setting
dates <- make.dates(c(2016,12), c(2019,12),
                    12)


#Forecast Function
Make.Fcst.EnerModel <- function(f, endDates, 
                                firstForecast) {
  return(ts(unlist(lapply(endDates, f)),
            start=firstForecast, 
            frequency=frequency(Enerdata)))
}

# Forecast Values and Actual Data
F.var <- Make.Fcst.EnerModel(EnerModel2, dates, c(2016,1))         
F.ar <- Make.Fcst.EnerModel(EnerModel3, dates, c(2016,1))   
Actual <- window(dlresUS, start=c(2016,1),
                 end=c(2019,12))

#Forecast Error for VAR and AR
E.var <- Actual - F.var
E.ar <- Actual - F.ar  


# Loss differential series
#errorcom<-ts.combine(E.var,E.ar)
#E <- e.var^2 - e.ar^2
#summary(lm(d ~ 1))  # I guess, regressing d on constant 
#plot(errorcom)

#Enc-New Statistics - We reject the null- WTIP granger cause resUS Electric Price, 
#Even Enc-New ends table ends at 10 lags, the statistics is high enough for rejection.
Num <- sum(E.ar^2 - E.ar*E.var)
Den <- sum(E.var^2)  
Enc.New <- 48*Num/Den  
Enc.New

#Engel-Granger Cointegration Test
Enerdata2<-ts.combine(lresWTIP,lresUS, lresHB)

lresUS

Enermodel4 <- lm(lresHB ~ lresUS + lresWTIP, data = Enerdata2)
ErrorTerm<-Enermodel4$residuals

#Test of Cointegration
DF<-adf.test(ErrorTerm)                                  #2
DF

#Plot of the Deviation - 

Actual1 <- window(lresHB, start=c(2001,1),
                  end=c(2019,12))

Residu<-Actual1 - Enermodel4$fitted.values

plot(Residu, col="red",
     main="Deviation of Henry Hub Gas \n Price from the Fitted Values",
     #sub="Fig.1.1",
     xlab=" Monthly\n Fig 1.9",
     ylab="Residuals",
     col.axis = "black",
     family = "Times New Roman",
     # type = "l",
     #xaxt = "n",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)






















































