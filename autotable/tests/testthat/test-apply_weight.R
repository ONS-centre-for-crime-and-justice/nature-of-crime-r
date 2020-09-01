context("apply weight")

test_that("dimension of dataframe remain the same", {

  expect_equal(length(apply_weight(mtcars, c("cyl"), "gear" )), length(mtcars))
  expect_equal(length(apply_weight(mtcars, c("cyl", "mpg"), "gear" )), length(mtcars))
  expect_equal(nrow(apply_weight(mtcars, c("cyl"), "gear" )), nrow(mtcars))

})

test_that("weight is accurately applied", {

  expect_equal(apply_weight(mtcars, c("cyl"), "gear")["cyl"], mtcars["cyl"]*mtcars["gear"])

  expect_equal(apply_weight(data.frame(x=c(1,2,3), y= c(2,3,4), z = c(3,4,5)), "x", "z"),
               data.frame(x=c(3,8,15), y= c(2,3,4), z = c(3,4,5))
               )

  expect_equal(apply_weight(data.frame(x=c(1,2,3), y= c(2,3,4), z = c(3,4,5)), c("x", "y"), "z"),
               data.frame(x=c(3,8,15), y= c(6,12,20), z = c(3,4,5)))


})

