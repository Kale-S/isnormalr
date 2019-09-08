context('Test the functons form homoskedasticity.R')


# bp_test
test_that('bp_test returns a list', {
  set.seed(137)
  X <- matrix(rnorm(100), ncol=5)
  b <- as.vector(c(2, 3, 4, 8, 9))
  e <- rnorm(20)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  bp <- isnormalr:::bp_test(e, model.matrix(mod))
  testthat::expect_type(bp, 'list')
  testthat::expect_s3_class(bp, 'htest')
})

test_that('bp_test has the right length', {
  set.seed(1234)
  X <- matrix(rnorm(100), ncol=5)
  b <- as.vector(c(2, 3, 4, 8, 9))
  e <- rnorm(20)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  bp <- isnormalr:::bp_test(e, model.matrix(mod))
  testthat::expect_equal(length(bp), 5)
})

test_that('bp_test hat the right entries', {
  set.seed(123)
  X <- matrix(rnorm(100), ncol=4)
  b <- as.vector(c(2, 3, 4, 8))
  e <- rnorm(25)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  bp <- isnormalr:::bp_test(e, model.matrix(mod))
  testthat::expect_equal(bp,
                   structure(list(
                     statistic = c(LM = 3.48128818494243),
                     p.value = 0.480729215253017,
                     parameter = c(df = 4L),
                     method = "studentized Breusch-Pagan heteroskedasticity test",
                     data.name = "model.matrix(mod)"),
                     class = "htest"))
  set.seed(333)
  X <- matrix(rnorm(100), ncol=2)
  b <- as.vector(c(2, 3))
  e <- rnorm(50)
  y <- X %*% b  + e
  mod <- lm(y ~ X)
  bp <- isnormalr:::bp_test(e, model.matrix(mod))
  testthat::expect_equal(bp,
                   structure(
                     list(statistic = c(LM = 2.29008521608363),
                          p.value = 0.318210359181279,
                          parameter = c(df = 2L),
                          method = "studentized Breusch-Pagan heteroskedasticity test",
                          data.name = "model.matrix(mod)"),
                          class = "htest"))

})
