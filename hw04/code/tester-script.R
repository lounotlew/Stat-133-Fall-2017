# ===================================================================
# Title: HW04: Grades Visualizer
# Subtitle: Stat 133, Fall 2017
# Description: Script file for 'tests.R' (Unit tests for 'functions.R')
# Author: Woo Sik (Lewis) Kim
# Date: 11/26/2017
# ===================================================================

# test script
library(testthat)

# source in functions to be tested
source('functions.R')

sink('../output/test-reporter.txt')
test_file('tests.R')
sink()
