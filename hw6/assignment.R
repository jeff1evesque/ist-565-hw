##
## assignment.R
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'hw6/visualization'), showWarnings = FALSE)

## load custom package
devtools::install_local(paste(cwd, sep='', '/packages/loadPackage'))
library('loadPackage')

## load contrib packages
load_package(c('rpart', 'rpart.plot', 'naivebayes'))

## import dataset
df.train = read.csv('data/digit--train.csv')
df.test = read.csv('data/digit--test.csv')

##
## decision tree
##
tree.start <- Sys.time()
fit.tree = rpart(
  label ~ .,
  data = df.train,
  method = 'class'
)
tree.end <- Sys.time()

tree.class.start <- Sys.time()
fit.tree.class = predict(fit.tree, df.test, type = 'class')
tree.class.end <- Sys.time()

tree.prob.start <- Sys.time()
fit.tree.prob = predict(fit.tree, df.test, type = 'prob')
tree.prob.end <- Sys.time()

## visualize default tree
png('hw6/visualization/default_tree.png', width=10, height=5, units='in', res=1400)
rpart.plot(fit.tree)
dev.off()

## decision tree summary
sink('hw6/visualization/tree_analysis.txt')
cat('===========================================================\n')
cat(' Note: the "root node error" is the error rate for a single\n')
cat(' node tree, if the tree was pruned to node 1. It is useful\n')
cat(' when comparing different decision tree models. measures of\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
printcp(fit.tree)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.tree.table = table(predict(fit.tree, type='class'), df.train$label)
1-sum(diag(fit.tree.table))/sum(fit.tree.table)
cat('\n\n')
cat('===========================================================\n')
cat(' cross validation performance \n')
cat('===========================================================\n')
xpred.rpart(fit.tree, xval=3)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
print(fit.tree.prob, n=length(fit.tree.prob))
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
print(fit.tree.class, n=length(fit.tree.class))
cat('\n\n')
cat('===========================================================\n')
cat(' performance \n')
paste('fitting tree: ', tree.end - tree.start)
paste('predicting probability: ', tree.prob.end - tree.prob.start)
paste('predicting class: ', tree.class.end - tree.class.start)
cat('===========================================================\n')
sink()

##
## naive bayes
##
nb.start <- Sys.time()
fit.nb = naive_bayes(
    as.factor(label) ~ .,
    data=df.train,
    laplace = 1
)
nb.end <- Sys.time()

nb.class.start <- Sys.time()
fit.nb.class = predict(fit.nb, df.test, type='class')
nb.class.end <- Sys.time()

nb.prob.start <- Sys.time()
fit.nb.prob = predict(fit.nb, df.test, type='prob')
nb.prob.end <- Sys.time()

## naive bayes summary
sink('hw6/visualization/nb_analysis.txt')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.nb.table = table(predict(fit.nb, type='class'), df.train$label)
1-sum(diag(fit.nb.table))/sum(fit.nb.table)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
print(fit.tree.prob, n=length(fit.tree.prob))
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
print(fit.nb.class, n=length(fit.nb.class))
cat('===========================================================\n')
cat(' performance \n')
paste('fitting tree: ', tree.end - tree.start)
paste('predicting probability: ', tree.prob.end - tree.prob.start)
paste('predicting class: ', tree.class.end - tree.class.start)
sink()
