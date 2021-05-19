if (TEST_SET){
ranger_final = readRDS("ranger_final.rds")
ixtest = xtest
ixtest[is.na(ixtest)] = -99
ranger_test_prob = predict(ranger_final, ixtest)$predictions[,1]
}
