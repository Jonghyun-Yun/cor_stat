xtrain = model.matrix(~ 0 + ., data = dx)
ytrain = 1 * (y > 0)
weights = rep(1,length(y)) / length(y)

set.seed(1)
nfold = 4
shu = sample(1:length(ytrain))
xtrain = xtrain[shu,]
ytrain = ytrain[shu]
data_folds = caret::createFolds(ytrain, k=nfold)
