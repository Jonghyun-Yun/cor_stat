my_tr <- trainControl(
  method = "cv",
  number = nfold,
  classProbs = TRUE,
  savePredictions = "all",
  ## summaryFunction = twoClassSummary, # AUC
  ## ,summaryFunction = prSummary # PR-AUC
  ## ,summaryFunction = fSummary # F1
  summaryFunction = mnLogLoss,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE,
  indexOut = data_folds
)

## imputation needs for ranger
ixtrain <- xtrain
ixtrain[is.na(ixtrain)] <- -99

## above50k needs to be "positive"
## caret considers 1st class as "positive" class
fytrain <- factor(ytrain)
levels(fytrain) <- c("no_cor", "cor")

ranger_grid <- expand.grid(
  mtry = c(20),
  splitrule = "gini",
  min.node.size = c(10)
)

set.seed(999999)
ranger_tune <- train(
  x = ixtrain, y = fytrain,
  method = "ranger",
  trControl = my_tr,
  tuneGrid = ranger_grid,
  weights = weights,
  preProc = NULL,
  importance = "impurity",
  num.trees = 500
)

temp <- ranger_tune$pred$co
ranger_id <- ranger_tune$pred$rowIndex
ranger_prob <- temp[order(ranger_id)]
ranger_final <- ranger_tune$finalModel
ranger_imp <- varImp(ranger_tune)$importance
ranger_imp %>% arrange(-ranger_imp) %>% slice(1:15)
