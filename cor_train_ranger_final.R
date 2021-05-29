set.seed(9999)
mydd = data.frame(y=fytrain, ixtrain)

## to calulate importance p-value based on purmutation
ranger_ff <- ranger(y~.,data = mydd,
                       num.trees = 500, mtry = 25,
  ## importance = "impurity_corrected",
  importance = "permutation",
  ## importance = "hold-out",
  ## importance = "impurity",
  write.forest = TRUE,
  probability = TRUE,
  min.node.size = 5,
  class.weights = NULL, splitrule = "gini", classification = TRUE,
  seed = 99
)

ranger_ho <- holdoutRF(y~.,data = mydd,
                       num.trees = 500, mtry = 25,
  ## importance = "impurity_corrected",
  ## importance = "permutation",
  ## importance = "hold-out",
  ## importance = "impurity",
  write.forest = TRUE,
  probability = TRUE,
  min.node.size = 5,
  class.weights = NULL, splitrule = "gini", classification = TRUE,
  seed = 99
)

ranger_imp <- ranger_ff$variable.importance
rtemp <- data.frame(ranger_imp = ranger_imp)
row.names(rtemp) <- names(ranger_imp)
rrtemp <- rtemp %>%
  arrange(-ranger_imp) %>%
  slice(1:15)
rrtemp[,1] = rrtemp[,1] / rrtemp[1,1] * 100
rrtemp %>% knitr::kable()

set.seed(9999)
im_pp = importance_pvalues(ranger_ff, method = "altmann", formula = y ~ ., data = mydd,
                                  num.permutations = 200)
pimp = sort(im_pp[,2])[1:15]
pimp %>% knitr::kable()
pimp_vv = names(pimp)[pimp < 0.1]

saveRDS(ranger_ff, "cor_ranger_rural_ff.rds")
