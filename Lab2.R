EPI_data <- read.csv("F:/rpi/data_analys/EPI_data.csv")
attach(EPI_data)
EPI

summary(EPI)
boxplot(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)

summary(DALY)
boxplot(DALY)
fivenum(DALY,na.rm=TRUE)
stem(DALY)

hist(EPI)
hist(DALY)

boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)

boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)

DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))

NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
pENV
cENV<- predict(lmENVH,NEW,interval="confidence")
cENV

boxplot(AIR_E,DALY,AIR_H,WATER_H)
lmAIRE<-lm(AIR_E~DALY+AIR_H+WATER_H)
lmAIRE
summary(lmAIRE)
cAIRH<-coef(lmAIRE)

pAIR<- predict(lmAIRE,NEW,interval="prediction")
pAIR
cAIR<- predict(lmAIRE,NEW,interval ="confidence" )
cAIR


boxplot(CLIMATE,DALY,AIR_H,WATER_H)
lmCLIMATE<-lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIMATE
summary(lmCLIMATE)
cCLIMATEH<-coef(lmCLIMATE)

pCLIMATE<- predict(lmCLIMATE,NEW,interval="prediction")
pCLIMATE
cCLIMATE<- predict(lmCLIMATE,NEW,interval ="confidence" )
cCLIMATE

multipleRegression <- read.csv("F:/rpi/data_analys/9.29/dataset_multipleRegression.csv")
head(multipleRegression)
attach(multipleRegression)
uh_r<-lm(ROLL~UNEM+HGRAD)
uh_r
summary(uh_r)

plot(ROLL~UNEM+HGRAD)
abline(uh_r)
new_uh <- data.frame(UNEM=0.07,HGRAD=90000)
predict(uh_r,(new_uh))

uhi_r<-lm(ROLL~UNEM+HGRAD+INC)
uhi_r
summary(uhi_r)
new_uhi <- data.frame(UNEM=0.07,HGRAD=90000,INC=25000)
predict(uhi_r,(new_uhi))

abalone<- read.csv("F:/rpi/data_analys/9.29/abalone.csv")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
summary(abalone)
str(abalone)
summary(abalone$rings)
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)

aba <- abalone
aba$sex <- NULL


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)

library(class)
help("knn") 
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)


library(ggplot2) 
head(iris) 
str(iris) 

summary(iris)

sapply(iris[,-5], var)
summary(iris)
# plot Sepal.Length Vs Sepal.Width using ggplot 
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
# plot Petal.Length Vs Sepal.Width using ggplot 
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()

set.seed(300)
k.max <- 15

wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 1000)$tot.withinss})
wss 
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
newcluster <- kmeans(iris[,3:4],3,nstart = 20)
table(iris[,5],newcluster$cluster)

EPI_data <- read.csv("F:/rpi/data_analys/EPI_data.csv")
attach(EPI_data)
install.packages('dplyr')
library(dplyr)
sample_n(EPI_data, 5)
sample_frac(EPI_data,0.1)
new_decs_EPI <- arrange(EPI_data,desc(EPI))
new_decs_EPI                        
new_decs_DALY <- arrange(EPI_data,desc(DALY))
new_decs_DALY 
head(mutate(EPI_data, double_EPI = EPI*2))
head(mutate(EPI_data, double_DALY = DALY*2))
summarise(EPI_data, avg_EPI = mean(EPI, na.rm = TRUE))
summarise(EPI_data, avg_DALY = mean(DALY, na.rm = TRUE))
