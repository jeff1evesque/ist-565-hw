##
## assignment.R
##
##     In this homework you are provided with the Federalist Paper data set. The features
##     are a set of "function words", for example, "upon". The feature value is the percentage
##     of the word occurrence in an essay. For example, for the essay "Hamilton_fed_31.txt",
##     if the function word "upon" appeared 3 times, and the total number of words in this
##     essay is 1000, the feature value is 3/1000=0.3%. Now you are going to try solving this
##     mystery using clustering algorithms k-Means, EM, and HAC. Document your analysis process
##     and draw your conclusion on who wrote the disputed essays. Provide evidence for each
##     method to demonstrate what patterns had been learned to predict the disputed papers, for
##     example, visualize the clustering results and show where the disputed papers are located
##     in relation to Hamilton and Madison's papers. By the way, where are the papers with joint
##     authorship located? For k-Means and EM, analyze the centroids to explain which attributes
##     are most useful for clustering. Hint: the centroid values on these dimensions should be
##     far apart from each other to be able to distinguish the clusters.
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'hw4/visualization'), showWarnings = FALSE)

## load custom package
devtools::install_local(paste(cwd, sep='', '/packages/loadPackage'))
library('loadPackage')

## load contrib packages
load_package(c('stats', 'factoextra'))

## import dataset
df = read.csv('data/fedPapers/fedPapers85.csv')

## convert author to numeric
df[, 1] = as.numeric(df[, 1])

##
## kmeans clustering
##
## @nstart=xx, select best of xx random initial configurations
##
kcluster = kmeans(df[,-c(1:2)], 3, nstart=20)

