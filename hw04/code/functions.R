# ===================================================================
# Title: HW04: Grades Visualizer
# Subtitle: Stat 133, Fall 2017
# Description: Computational functions for data processing
# Author: Woo Sik (Lewis) Kim
# Date: 11/26/2017
# ===================================================================

# Takes a vector X, and returns the input vector without missing values.
remove_missing <- function(x) {
  missing_vals <- is.na(x)
  trimmed <- x[!missing_vals]
  return (trimmed)
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and finds the minimum value of X.
get_minimum <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
    return (sort(x)[1])
  }
    return (sort(x)[1])
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and finds the maximum value of X.
get_maximum <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
    return (sort(x)[length(x)])
  }
    return (sort(x)[length(x)])
  }

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and finds the range of values (from min to max) of X.
get_range <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  max <- get_maximum(x)
  min <- get_minimum(x)
  return (max - min)
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the 10th percentile of X.
get_percentile10 <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return (quantile(x, 0.1))
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the 90th percentile of X.
get_percentile90 <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return (quantile(x, 0.9))
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the median of X.
get_median <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sort(x)
  len <- length(x)
  if ((len %% 2) == 0) {
    return ((x[len/2] + x[len/2 + 1]) / 2)
  }
  return (x[len/2 + 0.5])
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the mean of X.
get_average <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sum <- 0
  for (i in 1:length(x)) {
    sum <- sum + x[i]
  }
  return (sum / length(x))
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the standard deviation of X.
get_stdev <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  sum <- 0
  mean <- get_average(x)
  for (i in 1:length(x)) {
    sum <- sum + (mean - x[i])^2
  }
  return (sqrt(sum / (length(x) - 1)))
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the 1st quartile of X.
get_quartile1 <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return (quantile(x, 0.25))
}

# Takes a numeric vector X, and an optional logical NA.RM argument,
# and calculates the 3rd quartile of X.
get_quartile3 <- function(x, na.rm=FALSE) {
  if (na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return (quantile(x, 0.75))
}

# Takes a numeric vector X, and counts the number of 'NA' (missing)
# values in X.
count_missing <- function(x) {
  count <- 0
  for (boolean in is.na(x)) {
    if (boolean == TRUE) {
      count <- count + 1
    }
  }
  return (count)
}

# Takes a numeric vector X, and returns a list of summary statistics.
summary_stats <- function(x) {
  mis <- count_missing(x)
  if (checknarm(x)) {
    x <- remove_missing(x)
  }
  min <- get_minimum(x)
  perc10 <- get_percentile10(x)
  q1 <- get_quartile1(x)
  med <- get_median(x)
  mn <- get_average(x)
  q3 <- get_quartile3(x)
  perc90 <- get_percentile90(x)
  max <- get_maximum(x)
  rang <- get_range(x)
  std <- get_stdev(x)
  
  output <- list(minimum = min, percent10 = perc10, quartile1 = q1,
                 median = med, mean = mn, quartile3 = q3,
                 percent90 = perc90, maximum = max, range = rang,
                 stdev = std, missing = mis)
  
  return (output)
}

## Auxillary function. Takes in a numeric vector X and returns TRUE if
## X contains any 'NA' values; else, return FALSE.
checknarm <- function(x) {
  if (is.element(TRUE, is.na(x))) {
    return (TRUE)
  }
  else {
    return (FALSE)
  }
}

# Takes a list STATS of summary statistics, and prints the values
# in a nice format.
print_stats <- function(stats) {
  line1 <- paste("minimum  : ", format(round(stats[[1]], 4), nsmall=4), sep="")
  line2 <- paste("percent10: ", format(round(stats[[2]], 4), nsmall=4), sep="")
  line3 <- paste("quartile1: ", format(round(stats[[3]], 4), nsmall=4), sep="")
  line4 <- paste("median   : ", format(round(stats[[4]], 4), nsmall=4), sep="")
  line5 <- paste("mean     : ", format(round(stats[[5]], 4), nsmall=4), sep="")
  line6 <- paste("quartile3: ", format(round(stats[[6]], 4), nsmall=4), sep="")
  line7 <- paste("percent90: ", format(round(stats[[7]], 4), nsmall=4), sep="")
  line8 <- paste("maximum  : ", format(round(stats[[8]], 4), nsmall=4), sep="")
  line9 <- paste("range    : ", format(round(stats[[9]], 4), nsmall=4), sep="")
  line10 <- paste("stdev    : ", format(round(stats[[10]], 4), nsmall=4), sep="")
  line11 <- paste("missing  : ", format(round(stats[[11]], 4), nsmall=4), sep="")
  
  cat(paste(line1, line2, line3, line4, line5, line6, line7,
            line8, line9, line10, line11, sep="\n"))
}

# Takes four arguments: a numeric vector X, a minimum XMIN, a
# maximum XMIN, and an optional NA.RM, and computes a rescaled vector
# with a potential scale from 0 to 100.
rescale100 <- function(x, xmin, xmax, na.rm=FALSE) {
  if (na.rm==TRUE) {
    x <- remove_missing(x)
  }
  return (100 * ((x - xmin) / (xmax - xmin)))
}

# Takes a numeric vector X of length n, and returns a vector of length n − 1
# by dropping the lowest value (drops only one if >1 lowest values).
# by dropping the lowest value.
drop_lowest <- function(x) {
  x <- remove_missing(x)
  x <- x[-which.min(x)]
  return (x)
}

# Takes a numeric vector X of homework scores (of length n), and an optional
# logical argument DROP, to compute a single homework value. 
# If drop = TRUE, the lowest HW score will be dropped.
score_homework <- function(x, drop=FALSE) {
  x <- remove_missing(x)
  if (drop == TRUE) {
    x <- drop_lowest(x)
  }
  return (get_average(x))
}

# Takes a numeric vector X of quiz scores (of length n), and an optional
# logical argument DROP, to compute a single quiz value. 
# If drop = TRUE, the lowest quiz score will be dropped.
score_quiz <- function(x, drop=FALSE) {
  x <- remove_missing(x)
  if (drop == TRUE) {
    x <- drop_lowest(x)
  }
  return (get_average(x))
}

# Takes a numeric value X of lab attendance, and returns the lab score.
# The attendance value ranges between 0 and 12, and is graded on the 
# following scale (attendance score to lab score): 11-12 to 100,
# 10 to 80, 9 to 60, 8 to 40, 7 to 20, 0-6 to 0.
score_lab <- function(x) {
  if (x > 12 | x < 0) {
    return ("Error: Invalid Score")
  }
  else if (x == 11 | x == 12) {
    return (100)
  }
  else if (x == 10) {
    return (80)
  }
  else if (x == 9) {
    return (60)
  }
  else if (x == 8) {
    return (40)
  }
  else if (x == 7) {
    return (20)
  }
  else if (x == 6 | x < 6 && x > 0) {
    return (0)
  }
}

# Auxillary function that takes in a numeric value X and returns a
# letter grade by the following scale: F if 0 <= x < 50, D if
# 50 <= x < 60, C- if 60 <= x < 70, C if 70 <= x < 77.5, C+ if 
# 77.5 <= x < 79.5, B- if 79.5 <= x < 82, B if 82 <= x < 86,
# B+ if 86 <= x < 88, A- if 88 <= x < 90, A if 90 <= x < 95, and A+ if
# 95 <= x <= 100.
letter_grade <- function(x) {
  if (x >= 0 && x < 50) {
    return ('F')
  }
  else if (x >= 50 && x < 60) {
    return ('D')
  }
  else if (x >= 60 && x < 70) {
    return ('C-')
  }
  else if (x >= 70 && x < 77.5) {
    return ('C')
  }
  else if (x >= 77.5 && x < 79.5) {
    return ('C+')
  }
  else if (x >= 79.5 && x < 82) {
    return ('B-')
  }
  else if (x >= 82 && x < 86) {
    return ('B')
  }
  else if (x >= 86 && x < 88) {
    return ('B+')
  }
  else if (x >= 88 && x < 90) {
    return ('A-')
  }
  else if (x >= 90 && x < 95) {
    return ('A')
  }
  else if (x >= 95 && x <= 100) {
    return ('A+')
  }
}
