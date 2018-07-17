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
  cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(cwd)
}

## import dataset
df = read.csv('data-storyteller.csv')

## remove column if all 0s
df = df[, colSums(df != 0) > 0]
