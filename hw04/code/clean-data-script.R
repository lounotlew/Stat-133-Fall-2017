# ===================================================================
# Title: HW04: Grades Visualizer
# Subtitle: Stat 133, Fall 2017
# Description: Script file for data preparation of 'rawscores.csv'
# Author: Woo Sik (Lewis) Kim
# Date: 11/26/2017
# ===================================================================

# Importing 'functions.R':
source('functions.R')

# Reading in the 'rawscores.csv' file. 'rawscores' is kept in its original state,
# and 'cleanscores' is the data frame that will be processed:
rawscores <- read.csv('../data/rawdata/rawscores.csv',
                       stringsAsFactors = FALSE)
cleanscores <- read.csv('../data/rawdata/rawscores.csv',
                        stringsAsFactors = FALSE)

# Sinking the str() of the data frame of RAWSCORES to a file 'summary-rawscores.txt':
sink(file = '../output/summary-rawscores.txt')
str(cleanscores)
sink()

# Traversing the columns of RAWSCORES using a for() loop to replace all
# missing values 'NA' with 0's:
for (col in 1:16) {
  coldata <- cleanscores[, col]
  navals <- is.na(coldata)
  for (row in 1:334) {
    if (navals[row]) {
      cleanscores[row, col] = 0
    }
  }
}

# Rescaling QZ1 in RAWSCORES using rescale100():
qz1scores <- cleanscores[ , 11]
rescaledqz1 <- rescale100(qz1scores, 0, 12)
cleanscores[ , 11] <- rescaledqz1

# Rescaling QZ2 in RAWSCORES using rescale100():
qz2scores <- cleanscores[ , 12]
rescaledqz2 <- rescale100(qz2scores, 0, 18)
cleanscores[ , 12] <- rescaledqz2

# Rescaling QZ3 in RAWSCORES using rescale100():
qz3scores <- cleanscores[ , 13]
rescaledqz3 <- rescale100(qz3scores, 0, 20)
cleanscores[ , 13] <- rescaledqz3

# Rescaling QZ4 in RAWSCORES using rescale100():
qz4scores <- cleanscores[ , 14]
rescaledqz4 <- rescale100(qz4scores, 0, 20)
cleanscores[ , 14] <- rescaledqz4

# Adding a new column 'Lab' to RAWSCORES using 'score_lab()' on values
# of 'ATT':
cleanscores['Lab'] <- NA
for (row in 1:334) {
  cleanscores[row, 17] <- score_lab(cleanscores[row, 10])
}

# Adding a new column 'Homework' by dropping the lowest HW grade for 
# each student (row) and computing their average (row, col value):
cleanscores['Homework'] <- NA
for (row in 1:334) {
  studentHWscores <- cleanscores[row, 1:9]
  avgHWScore <- score_homework(studentHWscores, drop=TRUE)
  cleanscores[row, 18] = avgHWScore
}

# Adding a new column 'Quiz' by dropping the lowest quiz grade for 
# each student (row) and computing their average (row, col value):
cleanscores['Quiz'] <- NA
for (row in 1:334) {
  studentQuizScores <- cleanscores[row, 11:14]
  avgQuizScore <- score_homework(studentQuizScores, drop=TRUE)
  cleanscores[row, 19] = avgQuizScore
}

# Adding a new column 'Test1' to RAWSCORES by rescaling EX1 using rescale100():
ex1scores <- cleanscores[ , 15]
rescaledex1 <- rescale100(ex1scores, 0, 80)
cleanscores['Test1'] <- rescaledex1

# Adding a new column 'Test2' to RAWSCORES by rescaling EX2 using rescale100():
ex2scores <- cleanscores[ , 16]
rescaledex2 <- rescale100(ex2scores, 0, 90)
cleanscores['Test2'] <- rescaledex2

# Adding an overall course grade (scale of 0 to 100) 'Overall' by the
# following grading scheme: 10% lab score, 30% HW score (lowest dropped),
# 15% quiz score (lowest dropped), 20% test 1 score, 25% test 2 score.
cleanscores['Overall'] <- NA
for (row in 1:334) {
  cleanscores[row, 22] = 0.1*cleanscores[row, 17] + 0.3*cleanscores[row, 18] +
    0.15*cleanscores[row, 19] + 0.2*cleanscores[row, 20] + 0.25*cleanscores[row, 21]
}

# Adding a variable 'Grade' (letter grades) calculated through the following
# grade buckets: 
cleanscores['Grade'] <- NA
for (row in 1:334) {
  cleanscores[row, 23] = letter_grade(cleanscores[row, 22])
}
cleanscores$Grade = factor(cleanscores$Grade,
                           levels = c('A+', 'A', 'A-',
                                      'B+', 'B', 'B-',
                                      'C+', 'C', 'C-',
                                      'D', 'F'))

# Using a for() loop to export, via sink(), the summary statistics for
# 'Lab', 'Homework', 'Quiz', 'Test1', 'Test2', and 'Overall' into .txt files:
filenames <- c('../output/Lab-stats.txt', '../output/Homework-stats.txt',
               '../output/Quiz-stats.txt', '../output/Test1-stats.txt',
               '../output/Test2-stats.txt', '../output/Overall-stats.txt')
for (i in 1:6) {
  smry <- summary_stats(cleanscores[ , i + 16])
  sink(filenames[i])
  print_stats(smry)
  sink()
}

# sinking (via sink()) the structure, or str(), of the data frame of
# clean scores ('cleanscores') to a file 'summary-cleanscores.txt' inside
# the /output folder:
sink('../output/summary-cleanscores.txt')
str(cleanscores)
sink()

# Exporting the clean data frame of scores 'cleanscores' to a CSV file
# 'cleanscores.csv' inside the folder data/cleandata:
write.csv(cleanscores, file = '../data/cleandata/cleanscores.csv', row.names = FALSE)
