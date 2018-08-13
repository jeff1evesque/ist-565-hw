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
library('loadPackage', 'factoextra')

## load contrib packages
load_package(c('stats', 'flexclust', 'mclust'))

## import dataset
df = read.csv('data/fedPapers/fedPapers85.csv')

##
## preprocess data:
##
## - convert author to numeric
## - remove filename
##
df[, 1] = as.numeric(df[, 1])
df = df[, -c(2)]

##
## kmeans clustering: four clusters implemented, since there were three different
##     authors, and two (hamilton + madison) coauthored.
##
## @nstart=xx, select best of xx random initial configurations
##
KMeansCluster = kmeans(df, 4, nstart=20)

## cross tabulation: author and cluster membership
kClusterTable = table(df$author, KMeansCluster$cluster)

## visualize cross tabulation
png('hw4/visualization/mosaic_kmeans.png')
mosaicplot(kClusterTable, xlab='Author', ylab='Cluster')
dev.off()

## kmeans summary
sink('hw4/visualization/kmeans_analysis.txt')
KMeansCluster
cat('\n\n')
cat('===========================================================\n')
cat(' randIndex: measure between author, and cluster partitions.\n')
cat('\n')
cat(' Note: values vary between -1 to 1\n')
cat('===========================================================\n')
randIndex(kClusterTable)
sink()

## visualize kmeans
fviz_cluster(
    KMeansCluster,
    data = df,
    ellipse.type = 'euclid', # Concentration ellipse
    star.plot = TRUE, # Add segments from centroids to items
    repel = TRUE, # Avoid label overplotting (slow)
    ggtheme = theme_minimal()
)

ggsave(
  'hw4/visualization/kmeans_cluster.png',
  width = 16,
  height = 9,
  dpi = 100
)

## expectation maximization (em) clustering
author = factor(df$author, levels = 1:4, labels = c('hamilton', 'hm', 'jay', 'madison'))
X = data.matrix(df)
EMCluster <- Mclust(X)

## cross tabulation: author and cluster membership
EMTable = table(df$author, EMCluster$classification)

## visualize cross tabulation
png('hw4/visualization/mosaic_kmeans.png')
mosaicplot(EMTable, xlab='Author', ylab='Cluster')
dev.off()

## expectation maximization summary
sink('hw4/visualization/em_analysis.txt')
summary(EMCluster)
cat('\n\n')
cat('===========================================================\n')
cat(' randIndex: measure between author, and cluster partitions.\n')
cat('\n')
cat(' Note: values vary between -1 to 1\n')
cat('===========================================================\n')
adjustedRandIndex(author, EMCluster$classification)
sink()

## visualize cross tabulation
png('hw4/visualization/mosaic_em.png')
mosaicplot(EMTable, xlab='Author', ylab='Cluster')
dev.off()

## visualize expectation maximization
png('hw4/visualization/em_cluster.png')
plot(MclustDR(EMCluster, lambda = 1), what = 'scatterplot')
dev.off()
