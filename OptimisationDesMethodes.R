library(randomForest)
library(unbalanced)
library(caret)
library(xgboost)
library(e1071)



####Base de données####
bdd <- readRDS(file = 'base.rds')

bdd$Class <- as.factor(bdd$Class)
form <- Class~.

idx <- sample(1:nrow(bdd), as.integer(0.75*nrow(bdd)))
train <- bdd[idx, ]
test <- bdd[-idx, ]

X <- train[,-ncol(train)]
Y <- train$Class
newData <- ubBalance(X, Y, type="ubUnder", positive=1, perc=8, method="percPos")
train_ub <- as.data.frame(cbind(newData$X, newData$Y))
colnames(train_ub)[colnames(train_ub)=="newData$Y"] <- "Class"



#Optimisation du Random Forest
TrainData <- train_ub[,-31] 
TrainClasses <- train_ub[,31] 
rf.fcttrain <- train(TrainData, TrainClasses, method = "rf", trControl = trainControl(method = "cv"))
mtry_opt <- as.integer(rf.fcttrain$bestTune)
taux_erreur_ntree <- vector()
ntr <- c(1,seq(10,500,by=10))
for(j in ntr){
  rf.datant <- randomForest(form, train_ub, mtry=mtry_opt, ntree=j)
  rf.datant.pred <- predict(rf.datant,newdata=test)
  txerreur <- mean(rf.datant.pred!=test$Class)
  taux_erreur_ntree <- rbind(taux_erreur_ntree,txerreur)
}
ntree_opt <- ntr[which.min(taux_erreur_ntree)]

#Optimisation du Gradient Boosting
xgb_model = train(TrainData, TrainClasses, trControl = trainControl(method = "cv"), method = "xgbTree")
shrinkage_opt=xgb_model$bestTune[[3]]
max_prof_opt=xgb_model$bestTune[[2]]

#Optimisation du SVM
train.X=train_ub[,-31]
train.Y=train_ub[,31]
SVM.tune <- tune(svm,train.X,train.Y,kernel="linear", ranges=list(cost=2^(-3:3), gamma=2^(-2:2)),tunecontrol = tune.control(cross=10))
cost_opt=SVM.tune$best.parameters[[1]]