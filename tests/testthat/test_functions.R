context('Test the statistical Functions')


########### Tests for the normal_assumption.R file ##################

testthat::test_that('sturge_rule get the right number as output',{
  expect_equal(sturge_rule(4), 3)
  expect_equal(sturge_rule(12), 5)
  expect_equal(sturge_rule(2893), 12)
})
testthat::test_that('sturge_rule returns a double',{
  expect_equal(typeof(sturge_rule(4)), 'double')
  expect_equal(typeof(sturge_rule(12)), 'double')
})
testthat::test_that('sturge_rule returns warnings by negativ numbers', {
  expect_warning(sturge_rule(-9))
  expect_warning(sturge_rule(-80))
})

######### Tests for the multikolonarity.R file ######################

testthat::test_that('VIF returms the right output', {
  set.seed(123)
  X <- data.frame(matrix(rnorm(100), ncol=4))
  expect_equal(VIF(X), c(1.05065590203148, 1.15607503324735,
                         1.063129114148, 1.07183494555832))
  set.seed(123)
  X <- data.frame(matrix(runif(99), ncol=3))
  expect_equal(VIF(X), c(1.00136992025217, 1.01861120174133,
                   1.01998947635563))
})
testthat::test_that('VIF has the right dimension', {
  set.seed(123)
  X <- data.frame(matrix(rnorm(2000), ncol=20))
  expect_equal(length(VIF(X)), 20)

  set.seed(123)
  X <- data.frame(matrix(rexp(200), ncol=4))
  expect_equal(length(VIF(X)), 4)

})


######### Test Outlier ##############################################
#testthat::test_that('Influence.observation has the right dimanions',{
#  set.seed(123)
#  n <- 1000
#  x <- matrix(runif(n, min=-1), ncol=10)
#  b <- as.vector(seq(1, 10))
#  e <- as.vector(rnorm(100))
#  y <- x %*% b + e
#  mod <- lm(y ~., data=data.frame(x))
#  test <- isnormalr:::influence.observation(mod)
#  expect_equal(length(test), 4L)
#})
#testthat::test_that('Elements in Influence.observation has the right number of entries', {
#  set.seed(123)
#  n <- 1000
#  X <- matrix(runif(n, min=-1), ncol=10)
#  b <- as.vector(seq(1, 10))
#  e <- as.vector(rnorm(100))
#  y <- X %*% b + e
#  mod <- lm(y ~., data=data.frame(X))
#  expect_equal(length(isnormalr:::influence.observation(mod)[[1]]), 100L)
#  expect_equal(length(isnormalr:::influence.observation(mod)[[2]]), 100L)
#  expect_equal(length(isnormalr:::influence.observation(mod)[[3]]), 100L)
#  expect_equal(length(isnormalr:::influence.observation(mod)[[4]]), 100L)
#})
#testthat::test_that('Influence.observation is a list', {
#  set.seed(123)
#  n <- 100
#  X <- matrix(rnorm(n), ncol=2)
#  b <- as.vector(c(3, 4))
#  e <- rnorm(n/2)
#  y <- X %*% b + e
#  mod <- lm(y ~., data=data.frame(X))
#  expect_true(is.list(isnormalr:::influence.observation(mod)))
#})


######### Test for homoskedasticity.R ###############################
test_that('bp_test has the right length', {
  set.seed(1234)
  X <- data.frame(matrix(rnorm(99), ncol=3))
  e <- rnorm(33)
  expect_equal(length(bp_test(e, X)), 4)
})
test_that('bp_test hat the right entries', {
  set.seed(123)
  X <- data.frame(matrix(rexp(99), ncol=3))
  e <- rnorm(33)
  expect_equal(bp_test(e, X),
               structure(list(LM.statistic = 1.06496485330392,
                              LM.kritical.value = 7.81472790325118,
                              F.statistic = -0.00270916683295958,
                              F.kritical.value = 2.93402988966417),
                         class = "bp-Test"))
  set.seed(333)
  X <- data.frame(matrix(rexp(22), ncol=2))
  e <- rnorm(11)
  expect_equal(bp_test(e, X),
               structure(list(LM.statistic = 0.534879623985584,
                              LM.kritical.value = 5.99146454710798,
                              F.statistic = -0.00834481380979516,
                              F.kritical.value = 4.45897010752451),
                         class = "bp-Test"))

})
test_that('bp_test returns a list', {
  set.seed(137)
  X <- data.frame(matrix(rexp(50), ncol=5))
  e <- rnorm(10)
  expect_true(is.list(bp_test(e, X)))
})
