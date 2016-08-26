library(testthat)
context("layout")


X <- matrix(rep(c(1, 1, 2, 1, 4, 5,1 ,7, 5, 2, 1,8), 2), ncol = 3)
cluster.rows <- c(1, 2, 1, 1, 2, 3, 4, 4)
cluster.cols <- c(1, 1, 2)

test_that("no warning on default", {
  superheat(X)
})



test_that("Removing legend", {
  superheat(X,
            legend = F)
})



test_that("Removing left labels", {
  superheat(X,
            left.label = "none")
})


test_that("Removing bottom labels", {
  superheat(X,
            bottom.label = "none")
})



############## title #############

test_that("Adding title", {
  superheat(X,
            title = "title!")
})



############## column names #############
test_that("Adding column name", {
  superheat(X,
            column.title = "colname")
})


test_that("Adding column name with title", {
  superheat(X,
            column.title = "colname",
            title ="title")
})



test_that("Adding column name with large bottom labels", {
  superheat(X,
            column.title = "colname",
            bottom.label.size = 0.3)
})



test_that("Adding column name with right plot", {
  superheat(X,
            column.title = "colname",
            yr = 1:8)
})


test_that("Adding column name with right plot and no axes", {
  superheat(X,
            column.title = "colname",
            yr = 1:8,
            yr.axis = F)
})


test_that("Adding column name with right plot and no axes and no bottom labels", {
  superheat(X,
            column.title = "colname",
            yr = 1:8,
            yr.axis = F,
            bottom.label = "none")
})

test_that("Adding column name with right plot and no axes and big bottom labels", {
  superheat(X,
            column.title = "colname",
            yr = 1:8,
            yr.axis = F,
            bottom.label.size = 0.4)
})


test_that("Adding column name with right plot and axes and big bottom labels", {
  superheat(X,
            column.title = "colname",
            yr = 1:8,
            bottom.label.size = 0.4)
})


test_that("Adding column name with right plot and axes and no bottom labels", {
  superheat(X,
            column.title = "colname",
            yr = 1:8,
            bottom.label = "none")
})



####



test_that("Adding column name with top plot", {
  superheat(X,
            column.title = "colname",
            yt = 1:3)
})


test_that("Adding column name with top plot and no axes", {
  superheat(X,
            column.title = "colname",
            yt = 1:3,
            yt.axis = F)
})


test_that("Adding column name with top plot and no axes and no bottom labels", {
  superheat(X,
            column.title = "colname",
            yt = 1:3,
            yt.axis = F,
            bottom.label = "none")
})

test_that("Adding column name with top plot and no axes and no left labels", {
  superheat(X,
            column.title = "colname",
            yt = 1:3,
            yt.axis = F,
            left.label = "none")
})



test_that("Adding column name with top plot and axes and no left labels", {
  superheat(X,
            column.title = "colname",
            yt = 1:3,
            left.label = "none")
})



test_that("Adding column name with top plot and no axes and big left labels", {
  superheat(X,
            column.title = "colname",
            yt = 1:3,
            left.label.size = 0.3)
})



test_that("Adding column name with top plot and axes and big bottom labels", {
  superheat(X,
            column.title = "colname",
            yt = 1:3,
            bottom.label.size = 0.3)
})



###


test_that("Adding row name with top plot", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3)
})


test_that("Adding row name with top plot and no axes", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3,
            yt.axis = F)
})


test_that("Adding row name with top plot and no axes and no bottom labels", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3,
            yt.axis = F,
            bottom.label = "none")
})

test_that("Adding row name with top plot and no axes and no left labels", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3,
            yt.axis = F,
            left.label = "none")
})



test_that("Adding row name with top plot and axes and no left labels", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3,
            left.label = "none")
})



test_that("Adding row name with top plot and no axes and big left labels", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3,
            left.label.size = 0.3)

})


test_that("Adding row name with top plot and axes and big bottom labels", {
  superheat(X,
            row.title = "rowname",
            yt = 1:3,
            bottom.label.size = 0.3)
})

