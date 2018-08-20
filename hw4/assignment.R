##
## assignment.R
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
load_package(c('stats', 'flexclust', 'mclust', 'ggplot2', 'factoextra'))

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
melted_kmeans <- melt(kClusterTable)
ggplot(data = melted_kmeans, aes(x=Var.1, y=Var.2, fill=value)) +
  geom_tile() +
  labs(x = 'Cluster', y = 'Author', title = 'Author vs Cluster')

ggsave(
  'hw4/visualization/kmeans_em.png',
  width = 16,
  height = 9,
  dpi = 100
)

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
melted_em <- melt(EMTable)
ggplot(data = melted_em, aes(x=Var.1, y=Var.2, fill=value)) +
  geom_tile() +
  labs(x = 'Cluster', y = 'Author', title = 'Author vs Cluster')

ggsave(
  'hw4/visualization/crosstab_em.png',
  width = 16,
  height = 9,
  dpi = 100
)

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

## visualize expectation maximization
png('hw4/visualization/em_cluster.png')
plot(MclustDR(EMCluster, lambda = 1), what = 'scatterplot')
dev.off()

## hierarchical clustering
HCluster = hclust(dist(df))
clusterCut = cutree(HCluster, 4)
HClusterTable = table(clusterCut, df$author)

## hierarchical clustering summary
sink('hw4/visualization/hr_analysis.txt')
summary(HClusterTable)
sink()

## visualize hierarchical cluster
png('hw4/visualization/hr_cluster.png')
plot(HCluster)
dev.off()

## visualize cross tabulation
melted_hclust <- melt(HClusterTable)
ggplot(data = melted_hclust, aes(x=clusterCut, y=Var.2, fill=value)) +
    geom_tile() +
    labs(x = 'Cluster', y = 'Author', title = 'Author vs Cluster')

ggsave(
    'hw4/visualization/crosstab_hclust.png',
    width = 16,
    height = 9,
    dpi = 100
)

##
## preprocess 2: remove 'hamilton-madison' articles
##
df2 = df[df$author != 3, ]

##
## kmeans clustering: three clusters for each author.
##
## @nstart=xx, select best of xx random initial configurations
##
KMeansCluster2 = kmeans(df2, 3, nstart=20)

## cross tabulation: author and cluster membership
kClusterTable2 = table(df2$author, KMeansCluster2$cluster)

## visualize cross tabulation
melted_kmeans2 <- melt(kClusterTable2)
ggplot(data = melted_kmeans2, aes(x=Var.1, y=Var.2, fill=value)) +
  geom_tile() +
  labs(x = 'Cluster', y = 'Author', title = 'Author vs Cluster')

ggsave(
  'hw4/visualization/kmeans_em2.png',
  width = 16,
  height = 9,
  dpi = 100
)

## kmeans summary
sink('hw4/visualization/kmeans_analysis2.txt')
KMeansCluster2
cat('\n\n')
cat('===========================================================\n')
cat(' randIndex: measure between author, and cluster partitions.\n')
cat('\n')
cat(' Note: values vary between -1 to 1\n')
cat('===========================================================\n')
randIndex(kClusterTable2)
sink()

## visualize kmeans
fviz_cluster(
  KMeansCluster2,
  data = df2,
  ellipse.type = 'euclid', # Concentration ellipse
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal()
)

ggsave(
  'hw4/visualization/kmeans_cluster2.png',
  width = 16,
  height = 9,
  dpi = 100
)

## expectation maximization (em) clustering
author2 = factor(df2$author, levels = 1:3, labels = c('hamilton', 'jay', 'madison'))
X2 = data.matrix(df2)
EMCluster2 <- Mclust(X2)

## cross tabulation: author and cluster membership
EMTable2 = table(df2$author, EMCluster2$classification)

## visualize cross tabulation
melted_em2 <- melt(EMTable2)
ggplot(data = melted_em2, aes(x=Var.1, y=Var.2, fill=value)) +
  geom_tile() +
  labs(x = 'Cluster', y = 'Author', title = 'Author vs Cluster')

ggsave(
  'hw4/visualization/crosstab_em2.png',
  width = 16,
  height = 9,
  dpi = 100
)

## expectation maximization summary
sink('hw4/visualization/em_analysis2.txt')
summary(EMCluster2)
cat('\n\n')
cat('===========================================================\n')
cat(' randIndex: measure between author, and cluster partitions.\n')
cat('\n')
cat(' Note: values vary between -1 to 1\n')
cat('===========================================================\n')
adjustedRandIndex(author2, EMCluster2$classification)
sink()

## visualize expectation maximization
png('hw4/visualization/em_cluster2.png')
plot(MclustDR(EMCluster2, lambda = 1), what = 'scatterplot')
dev.off()

## hierarchical clustering
HCluster2 = hclust(dist(df2))
clusterCut2 = cutree(HCluster2, 3)
HClusterTable2 = table(clusterCut2, df2$author)

## hierarchical clustering summary
sink('hw4/visualization/hr_analysis2.txt')
summary(HClusterTable2)
sink()

## visualize hierarchical cluster
png('hw4/visualization/hr_cluster2.png')
plot(HCluster2)
dev.off()

## visualize cross tabulation
melted_hclust2 <- melt(HClusterTable2)
ggplot(data = melted_hclust2, aes(x=clusterCut2, y=Var.2, fill=value)) +
  geom_tile() +
  labs(x = 'Cluster', y = 'Author', title = 'Author vs Cluster')

ggsave(
  'hw4/visualization/crosstab_hclust2.png',
  width = 16,
  height = 9,
  dpi = 100
)
