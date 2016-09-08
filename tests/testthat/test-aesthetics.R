# tests for correct layout table

library(testthat)
context("additional plot aesthetics")

X <- matrix(rnorm(100), 50, 50)

yr <- rexp(50, 1)
yt <- rt(50, 10)


test_that("all additional plot options work", {
  expect_that(superheat(X = X,
                       yt = yt,
                       yt.plot.type = "line",
                       yr = yr,
                       yr.plot.type = "line",
                       membership.rows = c(rep(1, 25), rep(2, 25)),
                       membership.cols = c(rep(1, 25), rep(2, 25))),
              not(throws_error()))
  expect_that(superheat(X = X,
                        yt = yt,
                        yt.plot.type = "scatterline",
                        yr = yr,
                        yr.plot.type = "scatterline",
                        membership.rows = c(rep(1, 25), rep(2, 25)),
                        membership.cols = c(rep(1, 25), rep(2, 25))),
              not(throws_error()))
  expect_that(superheat(X = X,
                        yt = yt,
                        yt.plot.type = "scattersmooth",
                        yr = yr,
                        yr.plot.type = "scattersmooth",
                        membership.rows = c(rep(1, 25), rep(2, 25)),
                        membership.cols = c(rep(1, 25), rep(2, 25))),
              not(throws_error()))
  expect_that(superheat(X = X,
                         yt = yt,
                         yt.plot.type = "scatter",
                         yr = yr,
                         yr.plot.type = "scatter",
                         membership.rows = c(rep(1, 25), rep(2, 25)),
                         membership.cols = c(rep(1, 25), rep(2, 25))),
               not(throws_error()))
  expect_that(superheat(X = X,
                        yt = yt,
                        yt.plot.type = "bar",
                        yr = yr,
                        yr.plot.type = "bar",
                        membership.rows = c(rep(1, 25), rep(2, 25)),
                        membership.cols = c(rep(1, 25), rep(2, 25))),
              not(throws_error()))
  expect_that(superheat(X = X,
                        yt = yt,
                        yt.plot.type = "boxplot",
                        yr = yr,
                        yr.plot.type = "boxplot",
                        membership.rows = c(rep(1, 25), rep(2, 25)),
                        membership.cols = c(rep(1, 25), rep(2, 25))),
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