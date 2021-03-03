## set caret training parameters

stk_grid =  NULL  # default tuning parameters

                                        # model specific training parameter
stk_tr  = trainControl(
  method = 'cv',
  number = 4L,
  ## classProbs = TRUE,
  savePredictions = "all",
  ## summaryFunction = mnLogLoss,
  ## summaryFunction = twoClassSummary, # AUC
  ## summaryFunction = prSummary, # PR-AUC
  ## summaryFunction = fSummary, # F1
  ## search = "random",
  verboseIter = T,
  allowParallel = T,
  indexOut = data_folds
)

nnet_grid = expand.grid(size = 3,
                        decay = 0.05)

sup_x = data.frame(cat = cat_yhat,xgb = xgb$pred,ranger = ranger_final$predictions)
sup_y = ytrain
