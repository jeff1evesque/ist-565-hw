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
load_package(c(''))

## import dataset
df.train = read.csv('data/digit--train.csv')
df.test = read.csv('data/digit--test.csv')

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

## reset max.print
options(max.print = max_print)
