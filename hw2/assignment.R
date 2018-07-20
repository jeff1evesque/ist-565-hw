##
## HW2_levesque_jeffrey.R
##
##     Each of 5 schools (A, B, C, D and E) is implementing the same math
##     course this semester, with 35 lessons. There are 30 sections total.
##     The semester is about 3/4 of the way through.
##
##     For each section, we record the number of students who are:
##
##         . very ahead (more than 5 lessons ahead)
##         . middling (5 lessons ahead to 0 lessons ahead)
##         . behind (1 to 5 lessons behind)
##         . more behind (6 to 10 lessons behind)
##         . very behind (more than 10 lessons behind)
##         . completed (finished with the course)
##
##    What's the story (or stories) in this data? Find it, and tell it
##    visually and, above all, truthfully
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'hw2/visualization'), showWarnings = FALSE)

## import dataset
df = read.csv('data/data-storyteller.csv')

## load custom package
devtools::install_local(paste(cwd, sep='', '/packages/loadPackage'))
library('loadPackage')

## load contrib packages
load_package(c('plyr', 'ggplot2', 'reshape2'))

## remove column if all 0s
df = df[, colSums(df != 0) > 0]

## remove section column
df = df[, -c(which(colnames(df)=='Section'))]

## sum rows on first column
aggregate = ddply(df, 'School', numcolwise(sum))

## cell ratio: each dataframe cell value is divided by the row sum
aggregate.avg = aggregate[,2:length(aggregate)] / rowSums(aggregate[,2:length(aggregate)])
aggregate.avg$School = aggregate$School

## melt dataset to long format
aggregate.m = melt(aggregate.avg, id='School')

## generate stacked bargraphs
stacked_bar <- ggplot(aggregate.m) +
  geom_bar(stat = 'summary', fun.y = 'mean', color='black', aes(x=variable, y=value, fill=School)) +
  labs(x = 'Lesson State', y = 'Aggregate Mean', title = 'Aggregate Mean vs Lesson State', fill = 'School') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_hue(l=30)

## save visualization
ggsave(
  'hw2/visualization/stacked_barcharts.png',
  width = 16,
  height = 9,
  dpi = 100
)

## generate segregated bargraphs
line_graphs <- ggplot(aggregate.m) +
  geom_bar(stat = 'summary', fun.y = 'mean', color='black', aes(x=variable, y=value, fill=School)) +
  facet_wrap(~School) +
  labs(x = 'Lesson State', y = 'Aggregate Mean', title = 'Aggregate Mean vs Lesson State', fill = 'School') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_hue(l=30)

head(within(aggregate.m, Mean<- ave(value, School, variable)))

## save visualization
ggsave(
  'hw2/visualization/segregated_barcharts.png',
  width = 16,
  height = 9,
  dpi = 100
)
