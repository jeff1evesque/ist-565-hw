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

## preprocess data: remove author + filename columns
#df[, 1] = as.numeric(df[, 1])
df = df[, -c(2)]

## train size: 2/3 train + 1/3 test
train_size = floor((2/3) * nrow(df))

## random seed: allows reproducible random numbers
set.seed(123)

## training keys
train_keys = sample(seq_len(nrow(df)), size = train_size)

## train + test set
train = df[train_keys, ]
test = df[-train_keys, ]

##
## default tree
##
fit.default = rpart(
    author ~ .,
    data = train
)

## visualize default tree
png('hw5/visualization/default_tree.png')
rpart.plot(fit.default)
dev.off()

## expectation maximization summary
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
fit.default.pred = table(predict(fit.default, type='class'), train$author)
1-sum(diag(fit.default.pred))/sum(fit.default.pred)
cat('\n\n')
cat('===========================================================\n')
cat(' cross validation performance \n')
cat('===========================================================\n')
xpred.rpart(fit.default, xval=10)
sink()
