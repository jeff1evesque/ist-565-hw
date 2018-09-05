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
png('hw6/visualization/default_tree.png')
rpart.plot(fit.tree)
dev.off()
