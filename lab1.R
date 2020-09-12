days <- c('Mon','Tue','Wed','Thur','Fri')
temp <- c(28,30,32,31,29.3)
snowed <- c('T','T','F','F','T')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

EPI_data <-read.csv(file.choose(), header = TRUE)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI=EPI_data$EPI
EPI
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
lines(density(EPI,na.rm=TRUE,bw=SJ))
rug(EPI)
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(EPI);qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)
DALY
summary(DALY)
fivenum(DALY,na.rm=TRUE)
stem(DALY)
hist(DALY)
hist(DALY,seq(0.,92.,1.0),prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)
plot(ecdf(DALY),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(DALY);qqline(DALY)
x<-seq(0.,92.,1.0)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")

AIR_H
summary(AIR_H)
fivenum(AIR_H,na.rm=TRUE)
stem(AIR_H)
hist(AIR_H)
hist(AIR_H,seq(0.,100.,1.0),prob=TRUE)
lines(density(AIR_H,na.rm=TRUE,bw=1.))
rug(AIR_H)
plot(ecdf(AIR_H),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(AIR_H);qqline(AIR_H)
x<-seq(0.,100.,1.0)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
boxplot(EPI,DALY)
boxplot(DALY,WATER_H)
boxplot(AIR_H,DALY)
qqplot(EPI,DALY)

help("distributions")

EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1.))
rug(Eland)
plot(ecdf(Eland),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(Eland);qqline(Eland)
x<-seq(0.,100.,1.0)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")

EPI_Desert<-EPI[!Desert]
EDesert <- EPILand[!is.na(Desert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)

EPI_Desert<-EPI[!Desert]
EDesert <- EPI_Deser[!is.na(Desert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)

EPI_Water<-EPI[!No_surface_water]
EWater <- EPI_Water[!is.na(Desert)]
hist(EWater)
hist(EWater, seq(30., 95., 1.0), prob=TRUE)

EPI_South_Asia <-EPI[EPI_regions]
ESouth_Asia <- EPI_South_Asia[!is.na(EPI_regions)]
hist(ESouth_Asia)
hist(ESouth_Asia, seq(30., 95., 1.0), prob=TRUE)

GPW3_data <-read.csv(file.choose(), header = TRUE)
View(GPW3_data)
attach(GPW3_data)
fix(GPW3_data)

CountryCodeLong
summary(CountryCodeLong)
fivenum(CountryCodeLong,na.rm=TRUE)
stem(CountryCodeLong)
hist(CountryCodeLong)
hist(CountryCodeLong,seq(0.,10000.,10.0),prob=TRUE)
lines(density(CountryCodeLong,na.rm=TRUE,bw=1.))
lines(density(CountryCodeLong,na.rm=TRUE,bw=SJ))
rug(CountryCodeLong)
plot(ecdf(CountryCodeLong),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(CountryCodeLong);qqline(CountryCodeLong)
x<-seq(0.,10000.,10.0)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

water_treatment <-read.csv(file.choose(), header = TRUE)
View(water_treatment)
attach(water_treatment)
fix(water_treatment)

SSV.E
summary(SSV.E)
fivenum(SSV.E,na.rm=TRUE)
stem(SSV.E)
hist(SSV.E)
hist(SSV.E,seq(0.,80.,10.0),prob=TRUE)
lines(density(SSV.E,na.rm=TRUE,bw=1.))
lines(density(SSV.E,na.rm=TRUE,bw=SJ))
rug(SSV.E)
plot(ecdf(SSV.E),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(SSV.E);qqline(SSV.E)
x<-seq(0.,10000.,10.0)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)