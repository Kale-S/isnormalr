context('Test the functons form outlier.R')

# Influence.observation
testthat::test_that('Influence.observation has the right dimanions',{
  set.seed(123)
  n <- 1000
  x <- matrix(runif(n, min=-1), ncol=10)
  b <- as.vector(seq(1, 10))
  e <- as.vector(rnorm(100))
  y <- x %*% b + e
  mod <- lm(y ~., data=data.frame(x))
  e <- mod$residuals
  X <- model.matrix(mod)
  test <- olsdiagnosticR:::influence_observation(e = e, X = X)
  testthat::expect_equal(length(test), 4)
})

testthat::test_that('Elements inf Influence.observation has the
                    right of entries',{
  set.seed(123)
  n <- 1000
  x <-  matrix(runif(n, min=-1), ncol=10)
  b <- as.vector(seq(1, 10))
  e <- as.vector(rnorm(100))
  y <- x %*% b + e
  mod <- lm(y ~., data=data.frame(x))
  e <- mod$residuals
  X <- model.matrix(mod)
  test <- olsdiagnosticR:::influence_observation(e = e, X = X)
  testthat::expect_equal(length(test[[1]]), 100L)
  testthat::expect_equal(length(test[[2]]), 100L)
  testthat::expect_equal(length(test[[3]]), 100L)
  testthat::expect_equal(length(test[[4]]), 100L)
})

testthat::test_that('Influence.observation is a list', {
  set.seed(123)
  n <- 100
  x <- matrix(rnorm(n), ncol=2)
  b <- as.vector(c(3, 4))
  e <- rnorm(n/2)
  y <- x %*% b + e
  mod <- lm(y ~., data=data.frame(x))
  e <- mod$residuals
  X <- model.matrix(mod)
  test <- olsdiagnosticR:::influence_observation(e = e, X = X)
  testthat::expect_true(typeof(test) == 'list')
})
