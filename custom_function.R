wlogloss = function(y, p, w = rep(1,length(y)) / length(y)){
# return weighted log loss
# y: actual y
# p: prediction prob
# w: case weight
eps = 1e-15
p = pmax(pmin(p, 1 - eps), eps)
w = w / sum(w)
out = - sum(w*(y*log(p)+(1-y)*log(1-p)))
return(out)
}

order_level = function(x, label){
tab = table(x,label)
cp = tab / apply(tab,1,sum)
ll = row.names(tab[order(cp[,2]),])
return(ll)}

to_factor = function(x, label = ytrain) {
out = factor(x, levels = order_level(x,label))
return(out)}

find_continent = function(x) {
if (x %in% c("Panama", "Guatemala", "Honduras", "Dominican-Republic", "El-Salvador", "Columbia", "Nicaragua", "Trinadad&Tobago", "Puerto-Rico", "Haiti", "Peru", "Ecuador", "Jamaica", "Cuba")){ out = "South.America"
} else if (x %in% c("Japan", "Iran", "Laos", "India", "Outlying-U S (Guam USVI etc)", "Vietnam", "Taiwan", "China", "Cambodia", "Thailand", "South Korea", "Philippines", "Hong Kong")) {out = "Asia"
} else if (x %in% c("Portugal", "Italy", "Yugoslavia", "Greece", "Poland", "Germany", "England", "Hungary", "Scotland", "France", "Ireland", "Holand-Netherlands")) {out = "Europe"
} else if (x %in% c("United-States", "Canada", "Mexico")) {out = "North.America"
} else out = NA
return(out)
}

plot_y = function(x1, x2, point_size = 0.2, x1lab = NULL, x2lab = NULL, label = ytrain){
dd = data.frame(x1,x2,label)
    pp =
      ggplot(data = dd,aes(x = x1,y = x2,label = label, color = as.factor(label))) +
      geom_point(size = point_size,position = "jitter")  +  theme_bw() +
      theme(
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank()
        panel.background = element_blank()) + labs(y= x2lab, x = x1lab)
return(pp)}

plot_prob = function(x1, x2, point_size = 0.2, x1lab = NULL, x2lab = NULL, prob){
dd = data.frame(x1,x2,prob)
    pp =
      ggplot(data = dd,aes(x = x1,y = x2, color = prob)) +
      geom_point(size = point_size, position = "jitter")  +  theme_bw() +
      theme(
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank()
        panel.background = element_blank()) + labs(y= x2lab, x = x1lab)
return(pp)}

mylogit = function(x) {
eps = 10^(-100)
log(x + eps) / log(1 - x - eps)}

caret_wlogloss = function (data, pos = "cor", neg = "no_cor", cut = 0.5)
{
ff = data$Resample
obs = 1*(data$obs == pos)
prob = data[[pos]]
pred = 1 * (prob > cut)
if (is.null(data$weights)) {
  ww = rep(1,length(pred))
  } else ww = data$weights

return(wlogloss(y=obs, p=prob, w=ww))}

caret_wf1 = function(data, cut = 0.5) {
ff = data$Resample
obs = 1*(data$obs == "above50k")
prob = data$above50k
pred = 1 * (prob > cut)
if (is.null(data$weights)) {
  ww = rep(1,length(pred))
  } else ww = data$weights

ww = ww / sum(ww)
TP = sum(1* ww * (pred == 1 & obs == 1))
FP = sum(1* ww * (pred == 1 & obs == 0))
TN = sum(1* ww * (pred == 0 & obs == 0))
FN = sum(1* ww * (pred == 0 & obs == 1))

precision = TP / (TP + FP)
recall = TP / (TP + FN)
f1 = 2 * precision * recall / (precision + recall)
return(list(precision = precision, recall = recall, f1 = f1))
}

ssbar = function(varn, cc){
out = list()
tf = wtd.table(cl, varn, weights = sweight) / gw
wc = which(apply(tf,2,sum) < cc)
Other = tf[,wc]
Other = apply(Other,1,sum)
if (length(wc) > 0) {
tf = tf[,-wc]
out$table = cbind(tf,Other)
} else out$table = tf

tf = as.data.frame(tf)
of = data.frame(Var1 = 1:K, Var2 = rep("Other",K), Freq = Other)
tf = rbind(tf, of)

colnames(tf) = c("Cluster", "Category", "Conditional.Frequency")
pp = ggplot(tf, aes(Cluster, Conditional.Frequency, fill=Category)) +
  geom_bar(position="dodge",stat="identity")
out$plot = pp
return(out)}

## row.names(vimp) should be variable names
## vimp should be p by 1 (matrix, array, or data.frame)

require(ggplot2)
plot_vimp = function(vimp, cutoff = 1) {

   tryCatch({
      
      if(dim(vimp)[1] < dim(vimp)[2]) vimp <- t(vimp)
      
    }, error = function(e)
      {
      stop('vimp should be a p by 1 matrix, array, or data.frame.')
      }

    )
    
  ## Get the relative importance
  mval = max(vimp[,1])
  vimp[,2] = ( vimp[,1]/mval ) * 100

  cid = vimp[,2] > cutoff
  rnimp = vimp[,2][cid]
  imp.var = row.names(vimp)[cid]

  imp.dat = data.frame(imp.var, rnimp)

  ## barplot of relative importance
  gg = ggplot(data=imp.dat, aes(x=reorder(imp.var,-rnimp), y=rnimp, fill=TRUE)) +
    ylab("Relative variable importance") +
    xlab("Important features") +
    geom_bar(stat="identity") + coord_flip() +
    scale_fill_discrete(guide=FALSE) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=10),
          ##panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          ##panel.border = element_blank()
          panel.background = element_blank()
          )
  return(gg)
}
