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




test_that("colored row labels and legend", {
  superheat(X, 
            left.label = c(rep("a", 6), rep("b", 2)))
})

test_that("colored row labels and legend", {
  superheat(X, 
            left.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3))))
})

test_that("colored row labels and legend with heatmap legend on right", {
  superheat(X, 
            left.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3))),
            legend.position = "right")
})


test_that("colored row labels and legend with right plot", {
  superheat(X, 
            left.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3))),
            legend.position = "right",
            yr = 1:8)
})


test_that("colored row labels and legend with top plot", {
  superheat(X, 
            left.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3)),
                              c(rep("e", 1), rep("f", 2), rep("g", 5))),
            legend.position = "right",
            yt = 1:3)
})



test_that("colored row labels and legend with top and right plot", {
  superheat(X, 
            left.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3)),
                              c(rep("e", 1), rep("f", 2), rep("g", 5))),
            legend.position = "right",
            yt = 1:3,
            yr = 1:8)
})








test_that("colored col labels and legend", {
  superheat(t(X), 
            bottom.label = c(rep("a", 6), rep("b", 2)))
})

test_that("colored col labels and legend", {
  superheat(t(X), 
            bottom.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3))))
})

test_that("colored column labels and legend with heatmap legend on right", {
  superheat(t(X), 
            bottom.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3))),
            legend.position = "right")
})


test_that("colored column labels and legend with right plot", {
  superheat(t(X), 
            bottom.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3))),
            legend.position = "right",
            yr = 1:3)
})


test_that("colored col labels and legend with top plot", {
  superheat(t(X), 
            bottom.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3)),
                              c(rep("e", 1), rep("f", 2), rep("g", 5))),
            legend.position = "right",
            yt = 1:8)
})



test_that("colored col labels and legend with top and right plot", {
  superheat(t(X), 
            bottom.label = list(c(rep("a", 6), rep("b", 2)),
                              c(rep("c", 5), rep("d", 3)),
                              c(rep("e", 1), rep("f", 2), rep("g", 5))),
            legend.position = "right",
            yr = 1:3,
            yt = 1:8)
})



test_that("colored col and row labels", {
  superheat(X, 
            bottom.label = list(c(rep("a", 6), rep("b", 2)),
                                c(rep("c", 5), rep("d", 3)),
                                c(rep("e", 1), rep("f", 2), rep("g", 5))),
            left.label = list(c("a", "b", "c"), c("a", "bb", "bb")))
})
