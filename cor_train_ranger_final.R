ranger_ff <- ranger(
  x = ixtrain, y = fytrain,
  num.trees = 500, mtry = 20, importance = "impurity",
  write.forest = TRUE,
  probability = TRUE,
  min.node.size = 10,
  class.weights = NULL, splitrule = "gini", classification = TRUE
)
