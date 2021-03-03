# set caret training parameters

stk_grid =  NULL  # default tuning parameters

# model specific training parameter
stk_tr  = trainControl(
  method = 'cv',
  number = 4L,
  classProbs = TRUE,
  savePredictions = "all",
  summaryFunction = mnLogLoss,
  ## summaryFunction = twoClassSummary, # AUC
  ## summaryFunction = prSummary, # PR-AUC
  ## summaryFunction = fSummary, # F1
  ## search = "random",
  verboseIter = T,
  allowParallel = T,
  indexOut = data_folds
)

nnet_grid = expand.grid(size = 3,
                        decay = 0.1)

sup_x = data.frame(cat = cat_prob,xgb = xgb$prob,ranger = ranger_prob)
sup_y = fytrain

# train the model
 set.seed(1)
 super_tune = train(x=sup_x, y=sup_y,
              weights = weights,
              method = "nnet",
              trControl = stk_tr,
              tuneGrid = nnet_grid,
              verbose = T)
## super_tune
super_final = super_tune$finalModel
super_imp = varImp(super_final)
saveRDS(super_final, "super_final.rds")

## train the model
set.seed(1)
super_tune = train(x=sup_x, y=sup_y,
                   weights = weights,
                   method = "nnet",
                   trControl = stk_tr,
                   tuneGrid = nnet_grid,
                   verbose = T)
## super_tune
super_final = super_tune$finalModel
super_imp = varImp(super_final)
saveRDS(super_final, "sor_super_final.rds")
