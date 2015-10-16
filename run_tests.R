library('RUnit')
library('testthat')
library('rbenchmark')

source('cachematrix.R')

test.suite <- defineTestSuite("Programming Assignment 2",
                              dirs = file.path("tests"),
                              testFileRegexp = '.+\\.test\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)