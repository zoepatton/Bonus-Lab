context("ridgereg")

library(MASS)


test_that("ridgereg gives correct coefficient estimates",{
  test_ridge <- ridgereg$new(formula=Petal.Length~Species,data=iris, lambda=2)
  MASS_ridge <- lm.ridge(formula=Petal.Length~Species, data = iris, lambda=2)
  expect_equal(names(test_ridge$coef()[1]), names(MASS_ridge$coef[1]))
  expect_equal(test_ridge$coef()[1], MASS_ridge$coef[1], tolerance=2)
})

test_that("ridgereg gives correct coefficient estimates",{
  test_ridge <- ridgereg$new(formula=Petal.Length~Species,data=iris, lambda=2)
  MASS_ridge <- lm.ridge(formula=Petal.Length~Species, data = iris, lambda=2)
  expect_equal(names(test_ridge$coef()[2]), names(MASS_ridge$coef[2]))
  expect_equal(test_ridge$coef()[2], MASS_ridge$coef[2], tolerance=2)
})

