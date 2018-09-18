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

## max print
max_print = getOption('max.print')
options(max.print = nrow(df.full) * 1500)

##
## separate columns: match first two instances of commas, and split
##     into three different columns.
##
out = stri_split_fixed(str = df.full[, c(1)], pattern = ',', n = 3)
df.split = as.data.frame(do.call(rbind, out))

## assign column name
colnames(df.split) = df.colnames
df.split$review = as.character(df.split$review)

## create vocabulary
it_train = itoken(
    df.split$review,
    preprocessor = tolower,
    tokenizer = word_tokenizer
)
vocab = create_vocabulary(it_train)

## document term matrix
vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm = create_dtm(it_train, vectorizer)

## term frequency-inverse document frequency
model_tfidf = TfIdf$new()
dtm_tfidf = model_tfidf$fit_transform(dtm)

## dtm summary
sink('hw8/visualization/dtm.txt')
as.data.frame(as.matrix(dtm))
sink()

## dtm_tfidf summary
sink('hw8/visualization/dtm_tfidf.txt')
as.data.frame(as.matrix(dtm))
sink()=

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

##
## multinomial naive bayes
##
nb.fit.start = Sys.time()
fit.nb = naive_bayes(
  as.factor(lie) ~ .,
  data=df.train,
  laplace = 1
)
nb.fit.end = Sys.time()

## reset max.print
options(max.print = max_print)
