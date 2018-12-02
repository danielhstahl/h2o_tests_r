require(caret)
require(Matrix)
require(dplyr)
require(e1071)
dropColumns=function(data, columnsToDrop){
  data[ , !(names(data) %in% columnsToDrop)]
}

convertFactors=function(data, columnsToConvert){
  for(column in columnsToConvert){
    data[[column]]=as.factor(data[[column]])
  }
  return(data)
}
convertTarget=function(data, target, valueTree){
  data[[target]]=if_else(data[[target]]==valueTree, 1, 0)
  return(data)
}

cleanData=function(data){
  data %>%
    dropColumns(columnsToDrop) %>%
    convertFactors(columnsToConvertToFactor) 
}

updateTarget=function(data, target){
  data %>% convertTarget("Gender", POSITIVE_FACTOR)
}

convertToMatrix=function(data, target, xlev=NULL){
  list(
    features=sparse.model.matrix(
      as.formula(paste0(target, "~ .")), 
      data = data, 
      xlev=xlev
    )[, -1], 
    target=data[[target]]
  )
}

getFactorLevels=function(data){
  lapply(data[, sapply(data, is.factor), drop=F], function(field){
    levels(field)
  })
}

getTrainTest=function(features, target, p=.6){
  trainIndex <- c(createDataPartition(
    target, p = p, 
    list = FALSE, 
    times = 1
  ))
  train=list(features=features[trainIndex, ], target=target[trainIndex])
  test=list(features=features[-trainIndex, ], target=target[-trainIndex])
  list(train=train, test=test)
}

convertNumToString=function(nums){
  if_else(nums==1, POSITIVE_FACTOR, NEGATIVE_FACTOR)
}
convertProbabilitiesToBinary=function(probabilities, threshold){
  as.numeric(probabilities>threshold)
}
confusionMatrixRaw=function(predict, actual){
  confusionMatrix(
    as.factor(convertNumToString(predict)), 
    as.factor(convertNumToString(actual)),
    positive=POSITIVE_FACTOR
  )
}

plotImportance=function(model, featureNames){
  importance <- xgb.importance(feature_names = featureNames, model = model)
  xgb.plot.importance(importance_matrix = importance)
}

generateConfusionMatrixByThreshold=function(model, features, target){
  predictions=model %>% predict(test$features)
  function(threshold){
    confusionMatrixRaw(
      predictions %>% convertProbabilitiesToBinary(threshold), 
      test$target
    )
  }
}
