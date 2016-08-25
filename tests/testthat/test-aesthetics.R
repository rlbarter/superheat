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
