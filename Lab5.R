install.packages("titanic")


library(rpart)
library(rpart.plot)
titanic_train <- titanic::titanic_train
attach(titanic_train)
dectionTreeModel <- rpart(Survived~Pclass+Sex+Age, titanic_train)
dectionTreeModel
rpart.plot(dectionTreeModel)

library(party)

tree_titanic<-ctree(Survived~Pclass+Age, data=titanic_train)#Characters are not supported in CTree
plot(tree_titanic)

summary(tree_titanic)

cforest(Survived~Pclass+Age, data=titanic_train, controls=cforest_control(mtry=2, mincriterion=0))

library(tree)
tr_titanic <- tree(Survived~Pclass+Age, data=titanic_train)
tr_titanic$frame
plot(tr_titanic)
text(tr_titanic)