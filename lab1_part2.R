plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
help("qqnorm")
par(pty="s")
qqnorm(EPI)
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)  
par(pty="s")
help("qqnorm") 
help("qqplot") 
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) 
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

plot(ecdf(EPI_data$DALY),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE)  
par(pty="s")
help("qqnorm") 
help("qqplot") 
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY) 
y <- seq(30,95,1)
y
y2 <-seq(30,95,2)
y2
y2 <-seq(30,96,2)
y2
qqplot(qt(ppoints(250),df=5),y, xlab = "Q-Q plot")
qqline(y)


plot(ecdf(EPI_data$AIR_H),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$AIR_H),do.points=TRUE,verticals = TRUE)  
par(pty="s")
help("qqnorm") 
help("qqplot") 
qqnorm(EPI_data$AIR_H)
qqline(EPI_data$AIR_H) 
z <- seq(30,95,1)
z
z2 <-seq(30,95,2)
z2
z2 <-seq(30,96,2)
z2
qqplot(qt(ppoints(250),df=5),z, xlab = "Q-Q plot")
qqline(z)

AIR_H=EPI_data$AIR_H
qqplot(EPI,AIR_H)
boxplot(EPI,AIR_H)

qqplot(EPI,EPI_data$ECOSYSTEM)
boxplot(EPI,EPI_data$ECOSYSTEM)

qqplot(EPI,EPI_data$ENVHEALTH)
boxplot(EPI,EPI_data$ENVHEALTH)

qqplot(EPI_data$DALY,EPI_data$WATER_H)
boxplot(EPI_data$DALY,EPI_data$WATER_H)

qqplot(EPI_data$WATER_E,EPI_data$AIR_E)
boxplot(EPI_data$WATER_E,EPI_data$AIR_E)

qqplot(EPI_data$WATER_E,EPI_data$BIODIVERSITY)
boxplot(EPI_data$WATER_E,EPI_data$BIODIVERSITY)

multivariate <- read.csv("F:/rpi/data_analys/9.18/multivariate.csv")
attach(multivariate)
names(multivariate)
multivariate

mm<- lm(Homeowners ~ Immigrant)
mm

summary(mm)$coef

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
predict(mm,(newImmigrantdata))                #Another way is to pass the value to mm


abline(mm)
abline(mm,col=3,lwd=3) 
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+ geom_point()
plot(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
library(ggplot2)

qplot(pressure$temperature,pressure$pressure/2,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()


barplot(BOD$demand,names.arg=BOD&Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl))) +geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
gplot(mpg, data =mtcars, binwidth-4)
ggplot(mtcars, aes(x-mpg)) + geom_histogram(binwidth = 4) 
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)


plot(ToothGrowth$supp, ToothGrowth$len) 
boxplot(len ~ supp, data = ToothGrowth) 
boxplot(len ~ supp + dose, data = ToothGrowth)

library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom ="boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot") 
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) +geom_boxplot()
