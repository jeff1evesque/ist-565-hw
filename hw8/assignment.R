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
load_package(c('stringi', 'naivebayes', 'text2vec'))

## import dataset
filepath = 'data/deception_data_converted_final.csv'
df.colnames = read.table(filepath, nrow=1, stringsAsFactors=FALSE, sep=',')
df.full = read.table(
  filepath,
  skip=1,
  header=FALSE,
  sep='\n',
  quote = '',
  comment.char = ''
)

##
## separate columns: match first two instances of commas, and split
##     into three different columns.
##
out = stri_split_fixed(str = df.full[, c(1)], pattern = ',', n = 3)
df.split = as.data.frame(do.call(rbind, out))

## assign column name
colnames(df.split) = df.colnames

## tokenize
tokens = word_tokenizer(tolower(df.split$review[1:nrow(df.split)]))

## document term matrix
dtm = create_dtm(itoken(tokens), hash_vectorizer())

## term frequency-inverse document frequency
model_tfidf = TfIdf$new()
dtm_tfidf = model_tfidf$fit_transform(dtm)

##
## create train + test
##
## Note: seed defined to ensure reproducible sample
##
set.seed(123)
sample_size = floor(2/3 * nrow(df.split))
train = sample(seq_len(nrow(df.split)), size = sample_size)
df.train = df.split[train, ]
df.test = df.split[-train, ]
