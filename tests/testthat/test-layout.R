# tests for correct layout table

library(testthat)
context("layout")

X <- iris[,-c(1,5)]
yr <- iris[,1]
yt <- 1:3
gg.heat <- generate_heat(X = X)$gg.heat
gg.legend <- generate_heat(X = X)$gg.legend
gg.right <- generate_scatter(y = yr, membership = iris[,5], location = "right")
gg.top <- generate_scatter(y = yt, membership = colnames(iris[,-c(1,5)]), location = "top")
gg.left <- generate_cluster_label(membership = iris[,5], location = "left")
gg.bottom <- generate_var_label(names = colnames(iris), location = "bottom")

g <- generate_layout(gg.heat = gg.heat,
                     gg.top = gg.top,
                     gg.right = gg.right,
                     gg.legend = gg.legend,
                     gg.left = gg.left,
                     gg.bottom = gg.bottom)
gtable::gtable_show_layout(g)

test_that("correct full layout", {
  expect_equal(ncol(g), 4)
  expect_equal(nrow(g), 5)
  })




g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend)
test_that("correct empty layout", {
  expect_equal(ncol(g), 1)
  expect_equal(nrow(g), 2)
})



g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend,
                     gg.top = gg.top,
                     yt.axis = T,
                     yt.axis.name = T)
gtable::gtable_show_layout(g)
test_that("correct top only layout", {
  expect_equal(ncol(g), 3)
  expect_equal(nrow(g), 3)
})


g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend,
                     gg.top = gg.top,
                     yt.axis = F,
                     yt.axis.name = "")
gtable::gtable_show_layout(g)
test_that("correct top with name layout", {
  expect_equal(ncol(g), 1)
  expect_equal(nrow(g), 3)
})



g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend,
                     gg.top = gg.top,
                     yt.axis = T,
                     yt.axis.name = "")
gtable::gtable_show_layout(g)
test_that("correct top with axis layout", {
  expect_equal(ncol(g), 3)
  expect_equal(nrow(g), 3)
})




g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend,
                     gg.right = gg.right,
                     yr.axis = T,
                     yr.axis.name = "y")
gtable::gtable_show_layout(g)
test_that("correct right only layout", {
  expect_equal(ncol(g), 2)
  expect_equal(nrow(g), 4)
})



g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend,
                     gg.right = gg.right,
                     yr.axis = T)
gtable::gtable_show_layout(g)
test_that("correct right with axis layout", {
  expect_equal(ncol(g), 2)
  expect_equal(nrow(g), 4)
})



g <- generate_layout(gg.heat = gg.heat,
                     gg.legend = gg.legend,
                     gg.right = gg.right,
                     yr.axis = F)
gtable::gtable_show_layout(g)
test_that("correct right with axis name layout", {
  expect_equal(ncol(g), 2)
  expect_equal(nrow(g), 2)
})








g <- generate_layout(gg.heat = gg.heat,
                     gg.top = gg.top,
                     gg.right = gg.right,
                     gg.legend = gg.legend,
                     yr.axis = F,
                     yt.axis = F)
gtable::gtable_show_layout(g)
test_that("correct layout without bottom and top axis", {
  expect_equal(ncol(g), 2)
  expect_equal(nrow(g), 3)
})




g <- generate_layout(gg.heat = gg.heat,
                     gg.top = gg.top,
                     gg.right = gg.right,
                     gg.legend = gg.legend,
                     yr.axis.name = NULL,
                     yt.axis.name = NULL)
gtable::gtable_show_layout(g)
test_that("correct layout without bottom and top axis labels", {
  expect_equal(ncol(g), 4)
  expect_equal(nrow(g), 5)
})





X = t(iris[,c("Sepal.Width","Petal.Length","Petal.Width")])
yt = iris[,"Sepal.Length"]
yt.axis.name = "Sepal.Length"
membership.cols = iris[,"Species"]
cluster.rows = FALSE
heat <- generate_heat(X = X)
gg.heat <- heat$gg.heat
gg.legend <- heat$gg.legend
gg.top <- generate_scatter(y = yt, location = "top", membership = membership.cols, axis.name = yt.axis.name)
gg.right <- NULL
gg.bottom <- generate_cluster_label(membership = membership.cols, location = "bottom")
gg.left <- generate_var_label(names = rownames(X), location = "left")


g <- generate_layout(gg.heat = gg.heat,
                     gg.top = gg.top,
                     gg.right = gg.right,
                     gg.bottom = gg.bottom,
                     gg.left = gg.left,
                     gg.legend = gg.legend)
gtable::gtable_show_layout(g)
