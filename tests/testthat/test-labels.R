# tests for correct labels table

library(testthat)
context("labels")

X <- matrix(rep(c(1, 1, 2, 1, 4, 5,1 ,7, 5, 2, 1,8), 2), ncol = 3)
cluster.rows <- c(1, 2, 1, 1, 2, 3, 4, 4)
cluster.cols <- c(1, 1, 2)

test_that("no warning on default", {
  superheat(X)
  })




test_that("no warning when clustering rows", {
  superheat(X,
            membership.rows = cluster.rows)
})


test_that("no warning when clustering cols", {
  superheat(X,
            membership.cols = cluster.cols)
})


test_that("no warning when coloring left labels", {
  superheat(X,
            left.label.col = c("red", "blue"))
})


test_that("no warning when coloring left labels", {
  superheat(X,
            left.label.col = c("red", "blue", "red"))
})



test_that("no warning when coloring left labels", {
  superheat(X,
            left.label.col = c("red", "blue", "green", "red", "red", "blue", "purple"))
})


test_that("no warning when coloring bottom labels", {
  superheat(X,
            bottom.label.col = c("red", "blue", "green"))
})


test_that("no warning when coloring bottom labels", {
  superheat(X,
            bottom.label.col = c("red", "blue"))
})



test_that("no warning when clustering and coloring bottom labels", {
  superheat(X,
            membership.cols = cluster.cols,
            bottom.label.col = c("red", "blue"))
})


test_that("no warning when clustering and coloring left labels", {
  superheat(X,
            membership.rows = cluster.rows,
            left.label.col = c("red", "blue", "green", "purple"))
})



test_that("no warning when clustering and coloring left label text", {
  superheat(X,
            membership.rows = cluster.rows,
            left.label.text.col = c("red", "blue", "green"))
})


test_that("no warning when clustering and coloring bottom label text", {
  superheat(X,
            membership.rows = cluster.rows,
            bottom.label.text.col = c("red", "blue"))
})

