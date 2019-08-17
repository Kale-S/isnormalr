context('Test the functons form multikolonarity.R')

# VIF
testthat::test_that('VIF returms the right output', {
  set.seed(123)
  X <- data.frame(matrix(rnorm(100), ncol=4))
  v <- isnormalr:::VIF(X)
  testthat::expect_equal(v, c(1.05065590203148, 1.15607503324735,
                                   1.063129114148, 1.07183494555832))
  set.seed(123)
  X <- data.frame(matrix(runif(99), ncol=3))
  v <- isnormalr:::VIF(X)
  testthat::expect_equal(v, c(1.00136992025217, 1.01861120174133,
                                   1.01998947635563))
})
testthat::test_that('VIF has the right length', {
  set.seed(123)
  X <- data.frame(matrix(rnorm(2000), ncol=20))
  testthat::expect_equal(length(VIF(X)), 20)

  set.seed(123)
  X <- data.frame(matrix(rexp(200), ncol=4))
  testthat::expect_equal(length(VIF(X)), 4)

})
testthat::test_that('VIF has the right type', {
  set.seed(123)
  X <- data.frame(matrix(rnorm(200), ncol=4))
  v <- isnormalr:::VIF(X)
  testthat::expect_type(v, 'double')
  testthat::expect_true(is.vector(v) == TRUE)
})
