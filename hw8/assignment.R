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
load_package(c('stringi', 'naivebayes', 'text2vec', 'FSelector'))

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
df.split$review = as.character(df.split$review)

## max print
max_print = getOption('max.print')
options(max.print = nrow(df.full) * 1500)

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
df.merged = as.data.frame(as.matrix(dtm_tfidf))
df.merged
sink()

##
## reduce feature set: keep words if colsums > 0.3. This reduces the
##     sparse matrix from 1462 features, down to 118 (columns).
##
df.merged = df.merged[, colSums(df.merged) > 0.3]

## merge datasets
df.merged$lie = df.split$lie
df.merged$sentiment = df.split$sentiment
df.merged$review = df.split$review
df.merged = subset(df.merged, select = -c(review))

##
## create train + test
##
## Note: seed defined to ensure reproducible sample
##
set.seed(123)
sample_size = floor(2/3 * nrow(df.merged))
train = sample(seq_len(nrow(df.merged)), size = sample_size)
df.train = df.merged[train, ]
df.test = df.merged[-train, ]

##
## naive bayes: lie detection
##
nb.fit.lie.start = Sys.time()
fit.nb.lie = naive_bayes(
  as.factor(lie) ~ .,
  data=subset(df.train, select=-c(sentiment)),
  laplace = 1
)
nb.fit.lie.end = Sys.time()

nb.class.lie.start = Sys.time()
fit.nb.lie.class = predict(fit.nb.lie, subset(df.test, select = -c(sentiment)), type='class')
nb.class.lie.end = Sys.time()

nb.prob.lie.start = Sys.time()
fit.nb.lie.prob = predict(fit.nb.lie, subset(df.test, select = -c(sentiment)), type='prob')
nb.prob.lie.end = Sys.time()

##
## report: naive bayes, lie detection
##
sink('hw8/visualization/nb_lie.txt')
cat('===========================================================\n')
cat('naive bayes (lie detection):\n')
cat('===========================================================\n')
fit.nb.lie
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
paste('class', fit.nb.lie.class)
paste('probability', fit.nb.lie.prob)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.nb.table = table(fit.nb.lie.class, df.test$lie)
paste('class error:', 1-sum(diag(fit.nb.table))/sum(fit.nb.table))
fit.nb.table
sink()

##
## naive bayes: sentiment
##
nb.fit.sentiment.start = Sys.time()
fit.nb.sentiment = naive_bayes(
  as.factor(sentiment) ~ .,
  data=subset(df.train, select=-c(lie)),
  laplace = 1
)
nb.fit.sentiment.end = Sys.time()

nb.class.sentiment.start = Sys.time()
fit.nb.sentiment.class = predict(fit.nb.sentiment, subset(df.test, select = -c(lie)), type='class')
nb.class.sentiment.end = Sys.time()

nb.prob.sentiment.start = Sys.time()
fit.nb.sentiment.prob = predict(fit.nb.sentiment, subset(df.test, select = -c(lie)), type='prob')
nb.prob.sentiment.end = Sys.time()

##
## report: naive bayes, sentiment
##
sink('hw8/visualization/nb_sentiment.txt')
cat('===========================================================\n')
cat('naive bayes (sentiment):\n')
cat('===========================================================\n')
fit.nb.sentiment
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
paste('class', fit.nb.sentiment.class)
paste('probability', fit.nb.sentiment.prob)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.nb.table = table(fit.nb.sentiment.class, df.test$sentiment)
paste('class error:', 1-sum(diag(fit.nb.table))/sum(fit.nb.table))
fit.nb.table
sink()

##
## svm: lie detection
##
svm.fit.lie.start = Sys.time()
svm.lie.model = svm(
    as.factor(lie) ~ .,
    data=subset(df.train, select=-c(sentiment)),
    probability = TRUE
)
svm.fit.lie.end = Sys.time()

svm.prob.lie.start = Sys.time()
svm.lie.pred = predict(
    svm.lie.model,
    subset(df.test, select = -c(sentiment)),
    decision.values = TRUE,
    probability = TRUE
)
svm.prob.lie.end = Sys.time()

##
## report: svm, lie detection
##
sink('hw8/visualization/svm_lie.txt')
cat('===========================================================\n')
cat('svm (lie):\n')
cat('===========================================================\n')
svm.lie.model
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
paste('probability', svm.lie.pred)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.svm.table = table(svm.lie.pred, df.test$lie)
paste('class error:', 1-sum(diag(fit.svm.table))/sum(fit.svm.table))
fit.svm.table
sink()

##
## svm: sentiment
##
svm.fit.sentiment.start = Sys.time()
svm.sentiment.model = svm(
  as.factor(sentiment) ~ .,
  data=subset(df.train, select=-c(lie)),
  probability = TRUE
)
svm.fit.sentiment.end = Sys.time()

svm.sentiment.start = Sys.time()
svm.sentiment.pred = predict(
  svm.sentiment.model,
  subset(df.test, select = -c(lie)),
  decision.values = TRUE,
  probability = TRUE
)
svm.sentiment.end = Sys.time()

##
## report: svm, sentiment
##
sink('hw8/visualization/svm_sentiment.txt')
cat('===========================================================\n')
cat('svm (sentiment):\n')
cat('===========================================================\n')
svm.sentiment.model
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
paste('probability', svm.sentiment.pred)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.svm.table = table(svm.sentiment.pred, df.test$sentiment)
paste('class error:', 1-sum(diag(fit.svm.table))/sum(fit.svm.table))
fit.svm.table
sink()

##
## gain ratio + chi2
##
gain.sentiment = gain.ratio(
  sentiment ~ .,
  data=subset(df.train, select=-c(lie))
)
gain.sentiment20 = cutoff.k(gain.sentiment, 20)

gain.lie = gain.ratio(
  lie ~ .,
  data=subset(df.train, select=-c(sentiment))
)
gain.lie20 = cutoff.k(gain.lie, 20)

chi2.sentiment = chi.squared(
  sentiment ~ .,
  data=subset(df.train, select=-c(lie))
)
chi2.sentiment20 = cutoff.k(chi2.sentiment, 20)

chi2.lie = chi.squared(
  lie ~ .,
  data=subset(df.train, select=-c(sentiment))
)
chi2.lie20 = cutoff.k(chi2.lie, 20)

##
## report: gainratio + chi2
##
sink('hw8/visualization/gainratio_chi2.txt')
cat('===========================================================\n')
cat('gain ratio:\n')
cat('===========================================================\n')
paste('sentiment: ', gain.sentiment)
paste('lie: ', gain.lie)
paste('sentiment (top 20): ', gain.sentiment20)
paste('lie (top 20): ', gain.lie20)
cat('\n\n')
cat('===========================================================\n')
cat('chi squared: \n')
cat('===========================================================\n')
paste('sentiment: ', chi2.sentiment)
paste('lie: ', chi2.lie)
paste('sentiment (top 20): ', chi2.sentiment20)
paste('lie (top 20): ', chi2.lie20)
sink()

##
## model performance
##
sink('hw8/visualization/model_performance.txt')
cat('===========================================================\n')
cat(' performance (minutes) \n')
cat('===========================================================\n')
paste('fitting naive bayes (lie detection): ', nb.fit.lie.end - nb.fit.lie.start)
paste('fitting naive bayes (sentiment): ', nb.fit.sentiment.end - nb.fit.sentiment.start)
paste('fitting svm (lie detection): ', svm.fit.lie.end - svm.fit.lie.start)
paste('fitting svm (sentiment): ', svm.fit.sentiment.end - svm.fit.sentiment.start)
cat('\n')
paste('predicting naive bayes, class (lie detection): ', nb.class.lie.end - nb.class.lie.start)
paste('predicting naive bayes, prob (lie detection): ', nb.prob.lie.end - nb.prob.lie.start)
cat('\n')
paste('predicting naive bayes, class (sentiment): ', nb.class.sentiment.end - nb.class.sentiment.start)
paste('predicting naive bayes, prob (sentiment): ', nb.prob.sentiment.end - nb.prob.sentiment.start)
cat('\n')
paste('predicting svm, (lie detection): ', svm.prob.lie.end - svm.prob.lie.start)
paste('predicting svm, (sentiment): ', svm.sentiment.end - svm.sentiment.start)
sink()

## reset max.print
options(max.print = max_print)
