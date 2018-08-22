##
## assignment.R
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'hw5/visualization'), showWarnings = FALSE)

## load custom package
devtools::install_local(paste(cwd, sep='', '/packages/loadPackage'))
library('loadPackage')

## load contrib packages
load_package(c('caret', 'rpart', 'rpart.plot'))

## import dataset
df = read.csv('data/fedPapers/fedPapers85.csv')

## preprocess data: filename columns
df = df[, -c(2)]

## training keys
fulltrain = which(df$author != 'dispt')
disputed = which(df$author == 'dispt')

## train + test set
train = fulltrain[1:floor(length(fulltrain)*2/3)]
test = fulltrain[floor(length(fulltrain)*2/3): length(fulltrain)]

##
## default tree
##
fit.default = rpart(
    author ~ .,
    data = train,
    method = 'class'
)

## visualize default tree
png('hw5/visualization/default_tree.png')
rpart.plot(fit.default)
dev.off()

## default tree summary
sink('hw5/visualization/default_tree_analysis.txt')
cat('===========================================================\n')
cat(' Note: the "root node error" is the error rate for a single\n')
cat(' node tree, if the tree was pruned to node 1. It is useful\n')
cat(' when comparing different decision tree models. measures of\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
printcp(fit.default)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.default.train = table(predict(fit.default, type='class'), train$author)
fit.default.train
cat('\nrestribution error:\n')
1-sum(diag(fit.default.train))/sum(fit.default.train)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on test sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.default.test = table(predict(fit.default, type='class'), test$author)
fit.default.test
cat('\nrestribution error:\n')
1-sum(diag(fit.default.test))/sum(fit.default.test)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
predict(fit.default, disputed, type = 'prob')
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
predict(fit.default, disputed, type = 'class')
sink()

##
## tuned tree
##
## @maxdepth, john jay has 5 articles, indicated by the default tree
##     on the second level. Using one level elminates the 'john jay'
##     leaf-node branch.
##
fit.tuned = rpart(
  author ~ .,
  data = train,
  method = 'class',
  control = list(maxdepth = 1)
)

## visualize default tree
png('hw5/visualization/tuned_tree.png')
rpart.plot(fit.tuned)
dev.off()

## tuned tree summary
sink('hw5/visualization/tuned_tree_analysis.txt')
cat('===========================================================\n')
cat(' Note: the "root node error" is the error rate for a single\n')
cat(' node tree, if the tree was pruned to node 1. It is useful\n')
cat(' when comparing different decision tree models. measures of\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
printcp(fit.tuned)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.tuned.train = table(predict(fit.tuned, type='class'), train$author)
fit.tuned.train
cat('\nrestribution error:\n')
1-sum(diag(fit.tuned.train))/sum(fit.tuned.train)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on test sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.tuned.test = table(predict(fit.tuned, type='class'), test$author)
fit.tuned.test
cat('\nrestribution error:\n')
1-sum(diag(fit.tuned.test))/sum(fit.tuned.test)
cat('\n\n')
cat('===========================================================\n')
cat(' cross validation performance \n')
cat('===========================================================\n')
xpred.rpart(fit.tuned, xval=10)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
predict(fit.tuned, disputed, type = 'prob')
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
predict(fit.tuned, disputed, type = 'class')
sink()