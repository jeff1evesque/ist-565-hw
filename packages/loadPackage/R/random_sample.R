##
## random_sample.R, create random dataframe subsample.
##
random_sample = function(df, n) {
    return (df[sample(nrow(df), n),])
}
