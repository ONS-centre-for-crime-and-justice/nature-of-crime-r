context("functions remove rows with 0 weigh")

test_that("columns remain the same", {

  expect_equal(length(remove_zeroweighted_rows(mtcars, "vs" )), length(mtcars))

})

test_that("rows are dropped when weight is zero", {

  expect_equal(nrow(remove_zeroweighted_rows(mtcars, "vs" )) , nrow(mtcars)-sum(mtcars$vs   == 0))
  expect_equal(nrow(remove_zeroweighted_rows(mtcars, "am" )) , nrow(mtcars)-sum(mtcars$am   == 0))
  expect_equal(nrow(remove_zeroweighted_rows(mtcars, "carb")), nrow(mtcars)-sum(mtcars$carb == 0))

})



test_that("api", {
  carb = NULL
  expect_error(remove_zeroweighted_rows(mtcars))
  expect_error(remove_zeroweighted_rows(mtcars), carb)

})


