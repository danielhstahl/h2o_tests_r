library(h2o)
library(titanic)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('./data_utils.R')
localh2o=h2o.init()
transformed_train=convertFactors(titanic_train, c("Survived", "Embarked", "Sex"))
train=as.h2o(transformed_train)
train_test_split=h2o.splitFrame(train, ratios=0.8)
model=h2o.deeplearning(
  y="Survived",
  x=c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"),
  training_frame=train_test_split[[1]],
  validation_frame=train_test_split[[2]],
  activation="Rectifier",
  hidden=c(50, 50, 50),
  epochs=100,
  balance_classes=T ## slightly unbalanced
)
h2o.saveModel(object=model, path=getwd(), force=T)
## once saved, the model can be loaded in h2o flow

modelxg=h2o.xgboost(
  y="Survived",
  x=c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"),
  training_frame=train_test_split[[1]],
  validation_frame=train_test_split[[2]]
)
h2o.saveModel(object=modelxg, path=getwd(), force=T)