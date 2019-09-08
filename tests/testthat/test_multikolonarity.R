context('Test the functons form multikolonarity.R')

# VIF
testthat::test_that('VIF returms the right output', {
  set.seed(123)
  X <- matrix(rnorm(100), ncol=4)
  b <- as.vector(c(2, 3, 4, 8))
  e <- rnorm(25)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  v <- isnormalr:::VIF(model.matrix(mod))
  testthat::expect_equal(v, c(X1 = 1.05065590203148,
                              X2 = 1.15607503324735,
                              X3 = 1.063129114148,
                              X4 = 1.07183494555832))

  set.seed(123)
  x1 = rnorm(n = 100, mean = 80, sd = 10)
  x2 = rnorm(n = 100, mean = 70, sd = 5)
  x3 = 2 * x1 + 4 * x2 + rnorm(100, mean = 20, sd = 10)
  y = 3 + x1 + x2 + rnorm(n = 100, mean = 0, sd = 1)
  mod <- lm(y ~ x1 + x2 + x3)
  v <- isnormalr:::VIF(model.matrix(mod))
  testthat::expect_equal(v, c(x1 = 4.27741639804934,
                              x2 = 5.32173922080072,
                              x3 = 8.22165147148913
  ))
})
testthat::test_that('VIF has the right length', {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 5)
  b <- as.vector(c(2, 3, 4, 8, 5))
  e <- rnorm(20)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  v <- isnormalr:::VIF(model.matrix(mod))
  testthat::expect_equal(length(v), 5)

  set.seed(123)
  X <- matrix(rnorm(2000), ncol = 20)
  b <- as.vector(rep(c(1, 7, 9, 5), times = 5))
  e <- rnorm(100)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  v <- isnormalr:::VIF(model.matrix(mod))
  testthat::expect_equal(length(v), 20)

})
testthat::test_that('VIF has the right type', {
  set.seed(123)
  X <- matrix(rnorm(100), ncol=4)
  b <- as.vector(c(2, 3, 4, 8))
  e <- rnorm(25)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  v <- isnormalr:::VIF(model.matrix(mod))
  testthat::expect_type(v, 'double')
  testthat::expect_true(is.vector(v) == TRUE)
})
