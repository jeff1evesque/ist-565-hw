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
load_package(c('rpart', 'rpart.plot'))

## import dataset
df.train = read.csv('data/digit--train.csv')
df.test = read.csv('data/digit--test.csv')

##
## decision tree
##
fit.tree = rpart(
  label ~ .,
  data = df.train,
  method = 'class'
)

## visualize default tree
png('hw6/visualization/default_tree.png', width=10, height=5, units='in', res=1400)
rpart.plot(fit.tree)
dev.off()

## decision tree summary
sink('hw6/visualization/default_tree_analysis.txt')
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
fit.tree.pred = table(predict(fit.tree, type='class'), df.train$label)
1-sum(diag(fit.tree.pred))/sum(fit.tree.pred)
cat('\n\n')
cat('===========================================================\n')
cat(' cross validation performance \n')
cat('===========================================================\n')
xpred.rpart(fit.tree, xval=3)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
predict(fit.tree, df.test, type = 'prob')
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
predict(fit.tree, df.test, type = 'class')
sink()
