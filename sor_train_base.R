xtrain = model.matrix(~ 0 +., data = dx)
## y = log(df$sor)
ytrain <- y <- sory
weights = rep(1,length(y)) / length(y)

set.seed(1)
nfold = 4
## shu = sample(1:length(ytrain))
## xtrain = xtrain[shu,]
## ytrain = ytrain[shu]
data_folds = caret::createFolds(ytrain, k=nfold)

cat1fold = function(outfold, xtrain, ytrain, cat_features, fit_params, weights) {

n = length(ytrain);
oo = outfold;
ii = (1:n)[-oo];

inx = xtrain[ii,]
outx = xtrain[oo,]
iny = ytrain[ii]
outy = ytrain[oo]
inw = weights[ii]
outw = weights[oo]

inpool = catboost.load_pool(data = inx,
                           label = iny,
                           weight = inw,
                           cat_features = cat_features - 1)

outpool =  catboost.load_pool(data = outx,
                           label = outy,
                           weight = outw,
                           cat_features = cat_features - 1)

fit_params$loss_function = c('RMSE')

catmod = catboost.train(inpool, outpool)
yhat = catboost.predict(catmod, outpool)
loss = sqrt(mean((outy - yhat)^2))
return(list(yhat = yhat, loss = loss, catmod = catmod, names = names(catmod)))
}

cat_features = which(stringr::str_detect(colnames(xtrain), "ncat_"))

out = list()

fit_params <- list(iterations = 500,
                   border_count = 254,
                   depth = 10,
                   early_stopping_rounds = 20,
                   od_type = "Iter",
                   eval_metric = "RMSE", # overfit detection metric
                   use_best_model = T, # no tree saves after the best iteration
                   learning_rate = 0.1,
                   l2_leaf_reg = 3,
                   rsm = 1)

best_iter <- cv_loss <- numeric(nfold)
yhat = list()

set.seed(1)
for (k in 1:nfold){
out[[k]] = cat1fold(data_folds[[k]], xtrain, ytrain, cat_features, fit_params, weights)
cv_loss[k] = out[[k]]$loss
yhat[[k]] = out[[k]]$yhat
best_iter[k] = out[[k]]$catmod$tree_count
}

temp = unlist(yhat)
cat_id = unlist(data_folds)
cat_yhat = temp[order(cat_id)]
cat_best_iter = floor(median(best_iter))

trainpool = catboost.load_pool(data = xtrain,
                           label = ytrain,
                           weight = weights,
                           cat_features = cat_features - 1)

fit_params$loss_function = c('RMSE')
fit_params$iterations = cat_best_iter

cat_final = catboost.train(trainpool, params = fit_params)
cat_imp = cat_final$feature_importances

catboost.save_model(cat_final, "sor_cat_final.rds")

cat_train_pred = catboost.predict(cat_final, trainpool)

library(xgboost)

smxtrain = sparse.model.matrix(~.+0,as.data.frame(xtrain))
## smxtest = sparse.model.matrix(~.+0,outx)

dtrain = xgb.DMatrix(data = smxtrain, label = ytrain)
## dtest = xgb.DMatrix(data = smxtest, label = outy)

#default parameters
xgb_params <- list(objective = "reg:squarederror",
                   ## booster = "gbtree",
                   booster = "dart",
                   max_depth = c(10),
                   min_child_weight = c(1),
                   gamma = c(1),
                   colsample_bytree = c(0.9),
                   subsample = c(0.8),
                   eta = c(0.1)
                   # alpha = c(0),
                   # lambda = c(0)
                   )

set.seed(1)
xgbcv <- xgb.cv(params = xgb_params,
                weight = weights,
                data = dtrain,
                nrounds = 300,
                prediction = T,
                folds = data_folds,
                print_every_n = 10,
                early_stopping_rounds = 20,
                metrics = c("rmse")
                )
xgb = list(pred = xgbcv$pred,
           loss =  sqrt(mean((ytrain - xgbcv$pred)^2)),
           best_iter = xgbcv$best_ntreelimit)

##model training
xgb_final = xgb.train (params = xgb_params, data = dtrain, nrounds = xgb$best_iter)
xgb_imp = xgb.importance(model = xgb_final) # use Gain for importance
# xgb.plot.importance(importance_matrix = xgb_imp[1:40])
xgb.save(xgb_final, 'sor_xgb.model')

my_tr = trainControl(
method = 'cv',
number = nfold,
## classProbs = TRUE,
savePredictions = "all",
## ,summaryFunction = twoClassSummary # AUC
## ,summaryFunction = prSummary # PR-AUC
## ,summaryFunction = fSummary # F1
## summaryFunction = mnLogLoss,
search = "random",
verboseIter = TRUE,
allowParallel = TRUE,
indexOut = data_folds
)

## imputation needs for ranger
ixtrain = xtrain
ixtrain[is.na(ixtrain)] = -99

## above50k needs to be "positive"
## caret considers 1st class as "positive" class
## fytrain = factor(-(ytrain - 1))
## levels(fytrain) = c("no_co", "co")

ranger_grid <- expand.grid(
  mtry = c(20),
  ## splitrule = "gini",
  min.node.size = c(10)
)

set.seed(1)
ranger_tune <- train(x = ixtrain, y = ytrain,
                     method = "ranger",
                     trControl = my_tr,
                     ## tuneGrid = ranger_grid,
                     weights = weights,
                     preProc = NULL,
                     importance = 'impurity',
                     num.trees = 500
                     )

ranger_id = ranger_tune$pred$rowIndex
ranger_final = ranger_tune$finalModel
ranger_imp = varImp(ranger_tune)$importance

saveRDS(ranger_final, "sor_ranger_final.rds")
