##
## assignment.R
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'hw8/visualization'), showWarnings = FALSE)

## load custom package
devtools::install_local(paste(cwd, sep='', '/packages/loadPackage'))
library('loadPackage')

## load contrib packages
load_package(c('stringi'))

## import dataset
df.colnames = read.table('data/deception_data_converted_final.csv', nrow=1, stringsAsFactors=FALSE, sep=',')
df.full = read.table('data/deception_data_converted_final.csv', skip=1,header=FALSE, sep='\n')

##
## separate columns: match first two instances of commas, and split
##     into three different columns.
##
out = stri_split_fixed(str = df.full[, c(1)], pattern = ',', n = 3)
df.split = as.data.frame(do.call(rbind, out))

## assign column name
colnames(df.split) = df.colnames
