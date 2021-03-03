cat_features = which(stringr::str_detect(colnames(xtrain), "ncat_"))

out = list()

fit_params <- list(iterations = 500,
                   border_count = 254,
                   depth = 10,
                   early_stopping_rounds = 20,
                   od_type = "Iter",
                   eval_metric = "Logloss", # overfit detection metric
                   use_best_model = T, # no tree saves after the best iteration
                   learning_rate = 0.1,
                   l2_leaf_reg = 3,
                   rsm = 1)

cv_logloss = numeric(nfold)
lprob = list()
best_iter = numeric(nfold)

set.seed(1)
for (k in 1:nfold){
out[[k]] = cat1fold(data_folds[[k]], xtrain, ytrain, cat_features, fit_params, weights)
cv_logloss[k] = out[[k]]$logloss
lprob[[k]] = out[[k]]$prob
best_iter[k] = out[[k]]$catmod$tree_count
}

temp = unlist(lprob)
cat_id = unlist(data_folds)
cat_prob = temp[order(cat_id)]
cat_best_iter = floor(median(best_iter))
