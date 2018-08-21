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
load_package(c('caret'))

## import dataset
df = read.csv('data/fedPapers/fedPapers85.csv')

## preprocess data: remove author + filename columns
df = df[, -c(1, 2)]

## train size: 2/3 train + 1/3 test
train_size = floor((2/3) * nrow(df))

## random seed: allows reproducible random numbers
set.seed(123)

## training keys
train_keys = sample(seq_len(nrow(df)), size = train_size)

## train + test set
train = df[train_keys, ]
test = df[-train_keys, ]
