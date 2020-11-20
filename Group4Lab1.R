wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")

head(wine_data)
nrow(wine_data)
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data) # Now you can see the header names.
help("heatmap") 
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 
help(factor)
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes
help(prcomp)
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

library(gcookbook)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD
BOD1 <- BOD # make a copy of the dataset
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
ggplot(BOD, aes(x=Time, y= demand)) +geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) +geom_line() + expand_limits(y=0)
# Adding points to a line graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
library(gcookbook) # For the data set
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
# same with log-y axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10()  
