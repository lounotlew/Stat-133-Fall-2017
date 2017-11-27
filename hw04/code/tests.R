# ===================================================================
# Title: HW04: Grades Visualizer
# Subtitle: Stat 133, Fall 2017
# Description: Unit tests for 'functions.R'
# Author: Woo Sik (Lewis) Kim
# Date: 11/26/2017
# ===================================================================
library(testthat)
source('functions.R')

# Testing remove_missing():
test_that("Testing 'remove_missing()'", {
  expect_equal(remove_missing(c(1, 2, 3, NA, 4)), c(1, 2, 3, 4))
  expect_equal(remove_missing(c(1, 2, 3, 4, 5)), c(1, 2, 3, 4, 5))
  expect_equal(remove_missing(c(1, 3, NA, NA, 7, NA)), c(1, 3, 7))
  expect_equal(remove_missing(c(NA, NA, NA, 1, NA, NA, NA)), c(1))
})

# Testing get_minimum():
test_that("Testing 'get_minimum()'", {
  expect_equal(get_minimum(c(1, 2, 3, 4, 5)), 1)
  expect_equal(get_minimum(c(1, 1, 2, 3, 4)), 1)
  expect_equal(get_minimum(c(3, 2, 5, NA, 1, NA, NA, 0), na.rm=TRUE), 0)
  expect_equal(get_minimum(c(1, 1, 1, 1, 1)), 1)
})

# Testing get_maximum():
test_that("Testing 'get_maximum()'", {
  expect_equal(get_maximum(c(1, 2, 3, 4, 5)), 5)
  expect_equal(get_maximum(c(1, 1, 2, 3, 4, 4)), 4)
  expect_equal(get_maximum(c(3, 2, 5, 5, NA, 1, NA, NA, 0), na.rm=TRUE), 5)
  expect_equal(get_maximum(c(5, 5, 5, 5, 5)), 5)
})

# Testing get_range():
test_that("Testing 'get_range()'", {
  expect_equal(get_range(c(1, 2, 3, 4, 5)), 4)
  expect_equal(get_range(c(1, 1, 2, 3, 4, 4, 10)), 9)
  expect_equal(get_range(c(3, 2, 5, 5, NA, 1, NA, NA, 0), na.rm=TRUE), 5)
  expect_equal(get_range(c(5, 5, 5, 5, 5)), 0)
})

# Testing get_median():
test_that("Testing 'get_median()'", {
  expect_equal(get_median(c(4, 56, 34, 1, 0, 4, 0, 5, 65)), 0)
  expect_equal(get_median(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE), 3)
  expect_equal(get_median(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE), 4)
  expect_equal(get_median(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)), 20.15)
})

# Testing get_average():
test_that("Testing 'get_average()'", {
  expect_equal(get_average(c(4, 56, 34, 1, 0, 4, 0, 5)), 13)
  expect_equal(get_average(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE), 3)
  expect_equal(get_average(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE), 4)
  expect_equal(get_average(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)), 25.975)
})

# Testing get_stdev():
test_that("Testing 'get_stdev()'", {
  expect_equal(get_stdev(c(4, 56, 34, 1, 0, 4, 0, 5)),
               sd(c(4, 56, 34, 1, 0, 4, 0, 5)))
  expect_equal(get_stdev(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE),
               sd(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE))
  expect_equal(get_stdev(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE),
               sd(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE))
  expect_equal(get_stdev(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)),
               sd(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)))
})

# Testing get_percentile10():
test_that("Testing 'get_percentile10()'", {
  expect_equal(get_percentile10(c(4, 56, 34, 1, 0, 4, 0, 5)),
               quantile(c(4, 56, 34, 1, 0, 4, 0, 5), 0.1))
  expect_equal(get_percentile10(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE),
               quantile(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), 0.1, na.rm=TRUE))
  expect_equal(get_percentile10(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE),
               quantile(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), 0.1, na.rm=TRUE))
  expect_equal(get_percentile10(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)),
               quantile(c(3, 0, 47.5, 6, 34.3, 5, 67, 45), 0.1))
})

# Testing get_percentile90():
test_that("Testing 'get_percentile90()'", {
  expect_equal(get_percentile90(c(4, 56, 34, 1, 0, 4, 0, 5)),
               quantile(c(4, 56, 34, 1, 0, 4, 0, 5), 0.9))
  expect_equal(get_percentile90(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE),
               quantile(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), 0.9, na.rm=TRUE))
  expect_equal(get_percentile90(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE),
               quantile(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), 0.9, na.rm=TRUE))
  expect_equal(get_percentile90(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)),
               quantile(c(3, 0, 47.5, 6, 34.3, 5, 67, 45), 0.9))
})

# Testing get_quartile1():
test_that("Testing 'get_quartile1()'", {
  expect_equal(get_quartile1(c(4, 56, 34, 1, 0, 4, 0, 5)),
               quantile(c(4, 56, 34, 1, 0, 4, 0, 5), 0.25))
  expect_equal(get_quartile1(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE),
               quantile(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), 0.25, na.rm=TRUE))
  expect_equal(get_quartile1(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE),
               quantile(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), 0.25, na.rm=TRUE))
  expect_equal(get_quartile1(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)),
               quantile(c(3, 0, 47.5, 6, 34.3, 5, 67, 45), 0.25))
})

# Testing get_quartile3():
test_that("Testing 'get_quartile3()'", {
  expect_equal(get_quartile3(c(4, 56, 34, 1, 0, 4, 0, 5)),
               quantile(c(4, 56, 34, 1, 0, 4, 0, 5), 0.75))
  expect_equal(get_quartile3(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), na.rm=TRUE),
               quantile(c(NA, NA, 1, 2, 3, 4, 5, NA, NA), 0.75, na.rm=TRUE))
  expect_equal(get_quartile3(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), na.rm=TRUE),
               quantile(c(NA, 4, 4, 4, 4, 4, NA, NA, NA), 0.75, na.rm=TRUE))
  expect_equal(get_quartile3(c(3, 0, 47.5, 6, 34.3, 5, 67, 45)),
               quantile(c(3, 0, 47.5, 6, 34.3, 5, 67, 45), 0.75))
})

# Testing count_missing():
test_that("Testing 'count_missing()'", {
  expect_equal(count_missing(c(1, 4, 5, 6, 4, 1)), 0)
  expect_equal(count_missing(c(NA, 1, 3, 4, 5, 2, NA)), 2)
  expect_equal(count_missing(c(0, 0, 0, 4, 54, NA, NA, NA, 4, NA)), 4)
  expect_equal(count_missing(c(NA, NA, NA, NA, NA)), 5)
})

# Testing summary_stats():

test_that("Testing 'summary_stats()'", {
  test1 <- summary_stats(c(1, 4, 7, NA, 10))
  test2 <- summary_stats(c(1/3, 1/3, 1/3, 1/3, 1/3))
  
  expect_equal(test1[[1]] + test1[[4]] + test1[[5]], 12)
  expect_equal(length(test1), 11)
  expect_equal(test2[[4]], test2[[5]])
  expect_equal(test2[[7]], get_percentile90(c(1/3, 1/3, 1/3, 1/3, 1/3)))
})

# Testing print_stats():
test_that("Testing 'print_stats()'", {
  # Not required by Piazza Post @521
})

# Testing rescale100():
test_that("Testing 'rescale100()'", {
  expect_equal(rescale100(c(10, 15, 0, 20), 0, 20), c(50, 75, 0, 100))
  expect_equal(rescale100(c(23, 18, 17, 87), 0, 90),
               c(100*23/90, 100*18/90, 100*17/90, 100*87/90))
  expect_equal(rescale100(c(NA, NA, NA, NA, 80), 0, 100, na.rm=TRUE), c(80))
  expect_equal(rescale100(c(0, 0, 0, 0, 0), 0, 0), c(NaN, NaN, NaN, NaN, NaN))
})

# Testing drop_lowest():
test_that("Testing 'drop_lowest()'", {
  expect_equal(drop_lowest(c(0, 90, 40, 54, 23)), c(90, 40, 54, 23))
  expect_equal(drop_lowest(c(0, 0, 0, 54)), c(0, 0, 54))
  expect_equal(drop_lowest(c(0, 0, 0, 0, 0)), c(0, 0, 0, 0))
  expect_equal(drop_lowest(c(NA, NA, 34, 12, 54, 12)), c(34, 54, 12))
})

# Testing score_homework():
test_that("Testing 'score_homework()'", {
  expect_equal(score_homework(c(100, 80, 30, 70, 75, 85), drop=FALSE),
               mean(c(100, 80, 30, 70, 75, 85)))
  expect_equal(score_homework(c(100, 80, 30, 70, 75, 85), drop=TRUE),
               mean(c(100, 80, 70, 75, 85)))
  expect_equal(score_homework(c(NA, 80, 30, 70, 75, 12, NA, NA), drop=FALSE),
               mean(c(80, 30, 70, 75, 12)))
  expect_equal(score_homework(c(NA, 80, 30, 70, 75, 12, NA, NA), drop=TRUE),
               mean(c(80, 30, 70, 75)))
})

# Testing score_quiz():
test_that("Testing 'score_quiz()'", {
  expect_equal(score_quiz(c(100, 80, 70, 0), drop=FALSE),
               mean(c(100, 80, 70, 0)))
  expect_equal(score_quiz(c(100, 80, 70, 0), drop=TRUE),
               mean(c(100, 80, 70)))
  expect_equal(score_quiz(c(NA, 12, 34, 45, 33, 12, NA, NA), drop=FALSE),
               mean(c(12, 34, 45, 33, 12)))
  expect_equal(score_quiz(c(NA, 12, 34, 45, 33, 12, NA, NA), drop=TRUE),
               mean(c(12, 34, 45, 33)))
}) 

# Testing score_lab():
test_that("Testing 'score_lab()'", {
  expect_equal(score_lab(100), 'Error: Invalid Score')
  expect_equal(score_lab(12), 100)
  expect_equal(score_lab(11), 100)
  expect_equal(score_lab(10), 80)
  expect_equal(score_lab(9), 60)
  expect_equal(score_lab(8), 40)
  expect_equal(score_lab(4), 0)
  expect_equal(score_lab(-10), 'Error: Invalid Score')
})
