if (TEST_SET){
cat_final = catboost.load_model("cat_final.rds")

cat_features_test = which(stringr::str_detect(colnames(xtest), "n"))

testpool = catboost.load_pool(data = xtest,
                           cat_features = cat_features_test - 1)

cat_test_prob = catboost.predict(cat_final, testpool, prediction_type = 'Probability')
}

if (TEST_SET){
xgb_final = xgb.load('xgb_model')
smxtest = sparse.model.matrix(~.+0,xtest)
dtest = xgb.DMatrix(data = smxtest)
xgb_test_prob = predict(xgb_final, dtest)
}

if (TEST_SET){
ranger_final = readRDS("ranger_final.rds")
ixtest = xtest
ixtest[is.na(ixtest)] = -99
ranger_test_prob = predict(ranger_final, ixtest)$predictions[,1]
}

if (TEST_SET){
super_final = readRDS("super_final.rds")
sup_x_test = data.frame(cat = cat_test_prob,xgb = xgb_test_prob,ranger = ranger_test_prob)
super_test_prob = 1-predict(super_final, sup_x_test)

}
label_test = 1*(super_test_prob > 0.4122)

if (TEST_SET){
cat_final = catboost.load_model("cat_final.rds")

cat_features_test = which(stringr::str_detect(colnames(xtest), "n"))

testpool = catboost.load_pool(data = xtest,
                           cat_features = cat_features_test - 1)

cat_test_prob = catboost.predict(cat_final, testpool, prediction_type = 'Probability')
}

if (TEST_SET){
xgb_final = xgb.load('xgb_model')
smxtest = sparse.model.matrix(~.+0,xtest)
dtest = xgb.DMatrix(data = smxtest)
xgb_test_prob = predict(xgb_final, dtest)
}

if (TEST_SET){
ranger_final = readRDS("ranger_final.rds")
ixtest = xtest
ixtest[is.na(ixtest)] = -99
ranger_test_prob = predict(ranger_final, ixtest)$predictions[,1]
}

if (TEST_SET){
super_final = readRDS("super_final.rds")
sup_x_test = data.frame(cat = cat_test_prob,xgb = xgb_test_prob,ranger = ranger_test_prob)
super_test_prob = 1-predict(super_final, sup_x_test)

}
label_test = 1*(super_test_prob > 0.4122)
