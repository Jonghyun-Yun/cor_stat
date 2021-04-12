cat1fold = function(outfold, xtrain, ytrain, cat_features, fit_params, weights) {

n = length(ytrain);
oo = outfold;
ii = (1:n)[-oo];

inx = xtrain[ii,]
outx = xtrain[oo,]
iny = ytrain[ii]
outy = ytrain[oo    ]
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

fit_params$loss_function = c('Logloss')

catmod = catboost.train(inpool, outpool, params = fit_params)
prob = catboost.predict(catmod, outpool, prediction_type = 'Probability')
logloss = wlogloss(outy, prob, outw)
return(list(prob = prob, logloss = logloss, catmod = catmod))
}

trainpool = catboost.load_pool(data = xtrain,
                           label = ytrain,
                           weight = weights,
                           cat_features = cat_features - 1)

fit_params$loss_function = c('Logloss')
fit_params$iterations = cat_best_iter

cat_final = catboost.train(trainpool, params = fit_params)
cat_imp = cat_final$feature_importances

catboost.save_model(cat_final, "cor_cat_final.rds")

cat_train_pred = catboost.predict(cat_final, trainpool, prediction_type = 'Probability')

library(xgboost)

smxtrain = sparse.model.matrix(~.+0,as.data.frame(xtrain))
## smxtest = sparse.model.matrix(~.+0,outx)

dtrain = xgb.DMatrix(data = smxtrain, label = ytrain)
## dtest = xgb.DMatrix(data = smxtest, label = outy)

#default parameters
xgb_params <- list(objective = "binary:logistic",
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
                early_stopping_rounds = 10,
                metrics = c("logloss")
                )
xgb = list(prob = xgbcv$pred,
           logloss =  wlogloss(y = ytrain, p = xgbcv$pred, w=weights),
           best_iter = xgbcv$best_ntreelimit)

##model training
xgb_final = xgb.train (params = xgb_params, data = dtrain, nrounds = xgb$best_iter)
xgb_imp = xgb.importance(model = xgb_final) # use Gain for importance
# xgb.plot.importance(importance_matrix = xgb_imp[1:40])
xgb.save(xgb_final, 'cor_xgb_model')
