##
## assignment.R
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'hw7/visualization'), showWarnings = FALSE)

## load custom package
devtools::install_local(paste(cwd, sep='', '/packages/loadPackage'))
library('loadPackage')

## load contrib packages
load_package(c('e1071', 'class'))

## import dataset
df.train.full = read.csv('data/digit--train.csv')
df.test.full = read.csv('data/digit--test.csv')

## smaller dataset: reduce computation
df.train = random_sample(df.train.full, (nrow(df.train.full) * 0.1))
df.test = random_sample(df.test.full, (nrow(df.test.full) * 0.1))

## remove redundant pixels
delete = c(
  'pixel0',
  'pixel1',
  'pixel2',
  'pixel3',
  'pixel4',
  'pixel5',
  'pixel6',
  'pixel7',
  'pixel8',
  'pixel9',
  'pixel10',
  'pixel11',
  'pixel780',
  'pixel781',
  'pixel782',
  'pixel783'
)
df.train = df.train[, !(names(df.train) %in% delete)]
df.test = df.test[, !(names(df.test) %in% delete)]

## max print
max_print = getOption('max.print')
options(max.print = nrow(df.train))

##
## knn: using a guess for number of neighbors (i.e. k)
##
knn.start = Sys.time()
fit.knn = knn(
  train=df.train[-1],
  test=df.test,
  cl=df.train$label,
  k=1,
  prob=F
)
knn.end = Sys.time()

## confusion matrix
knn.error = 'NA'

##
## knn report
##
sink('hw7/visualization/knn_analysis.txt')
cat('===========================================================\n')
cat('knn model: \n')
cat('===========================================================\n')
fit.knn
cat('\n\n')
cat('===========================================================\n')
cat('confusion matrix:\n')
cat('===========================================================\n')
cat('N/A')
cat('\n\n')
cat('===========================================================\n')
cat('resubstitution error:\n')
cat('===========================================================\n')
cat('N/A')
sink()

##
## multiclass svm: using 3 cross validation
##
svm.start = Sys.time()
fit.svm = svm(
  label~.,
  data=df.train,
  kernel='radial',
  scale=FALSE,
  cost=1,
  cross=3
)
svm.end = Sys.time()

## generate prediction
svm.pred.start = Sys.time()
fit.svm.class = predict(fit.svm, df.train[-1], type='class')
svm.pred.end = Sys.time()

## confusion matrix
svm.table = table(fit.svm.class, df.train$label)
svm.error = 1-sum(diag(svm.table)) / sum(svm.table)

##
## svm report
##
sink('hw7/visualization/svm_analysis.txt')
cat('===========================================================\n')
cat('svm model: \n')
cat('===========================================================\n')
fit.svm
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
fit.svm.class
cat('\n\n')
cat('===========================================================\n')
cat('confusion matrix:\n')
cat('===========================================================\n')
svm.table
cat('\n\n')
cat('===========================================================\n')
cat('resubstitution error:\n')
cat('===========================================================\n')
svm.error
sink()

##
## model performance
##
sink('hw7/visualization/model_performance.txt')
cat('===========================================================\n')
cat(' performance (minutes) \n')
cat('===========================================================\n')
paste('fitting svm: ', svm.end - svm.start)
paste('predicting svm: ', svm.pred.end - svm.pred.start)
paste('knn model + prediction: ', knn.end - knn.start)
sink()

## reset max.print
options(max.print = max_print)
