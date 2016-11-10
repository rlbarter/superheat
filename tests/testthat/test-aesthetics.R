# tests for correct layout table

library(testthat)
context("additional plot aesthetics")

test_that("all scatterplot plot options work", {
  expect_that(superheat(X = iris[,-5],
                       yr = 1:150),
              not(throws_error()))
  expect_that(superheat(X = iris[1:80,-5],
                        yr = 1:80),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5],
                        yr = 1:150,
                        yr.obs.col = rep("red", 150)),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5],
                        yr = 1:150,
                        membership.rows = iris[,5],
                        yr.obs.col = rep("red", 150)),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5],
                        yr = 1:150,
                        membership.rows = iris[,5]),
              not(throws_error()))
  expect_that(superheat(X = iris[1:99,-5],
                        yr = 1:99,
                        membership.rows = iris[1:99,5]),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5],
                        yr = 1:3,
                        membership.rows = iris[,5]),
              not(throws_error()))
  expect_that(superheat(X = iris[1:80,-5],
                        yr = 1:2,
                        membership.rows = iris[1:80,5]),
              not(throws_error()))
  })

test_that("Clustered additional plots and unclustered plot options are correct", {
  expect_that(superheat(X = iris[,-5], 
                        yt = 1:2, 
                        membership.cols = c(1, 2, 2, 2)),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yt = 1:2, 
                        membership.cols = c(1, 2, 2, 2),
                        yt.plot.type = "bar"),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yt = 1:4, 
                        membership.cols = c(1, 2, 2, 2)),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yt = 1:4, 
                        membership.cols = c(1, 2, 2, 2),
                        yt.plot.type = "bar"),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yt = 1:4, 
                        membership.cols = c(1, 2, 2, 2),
                        yt.plot.type = "boxplot"),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yt = 1:2, 
                        membership.cols = c(1, 2, 2, 2),
                        yt.plot.type = "boxplot"),
              not(throws_error()))
  
  
  expect_that(superheat(X = iris[,-5], 
                        yr = 1:150, 
                        membership.rows = c(rep("fl1", 20), 
                                            rep("fl2", 70), 
                                            rep("fl3", 60))),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yr = 1:150, 
                        membership.rows = c(rep("fl1", 20), 
                                            rep("fl2", 70), 
                                            rep("fl3", 60)),
                        yr.plot.type = "bar"),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yr = 1:3, 
                        membership.rows = c(rep("fl1", 20), rep("fl2", 70), rep("fl3", 60)),
                        yr.plot.type = "bar"),
              not(throws_error()))
  expect_that(superheat(X = iris[,-5], 
                        yr = 1:3, 
                        membership.rows = c(rep("fl1", 20), rep("fl2", 70), rep("fl3", 60))),
              not(throws_error()))

})

