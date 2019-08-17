context('Test the functons form homoskedasticity.R')


# bp_test
test_that('bp_test returns a list', {
  set.seed(137)
  X <- data.frame(matrix(rexp(50), ncol=5))
  e <- rnorm(10)
  bp <- isnormalr:::bp_test(e, X)
  testthat::expect_type(bp, 'list')
  testthat::expect_s3_class(bp, 'htest')
})

test_that('bp_test has the right length', {
  set.seed(1234)
  X <- data.frame(matrix(rnorm(99), ncol=3))
  e <- rnorm(33)
  bp <- isnormalr:::bp_test(e, X)
  testthat::expect_equal(length(bp), 5)
})

test_that('bp_test hat the right entries', {
  set.seed(123)
  X <- data.frame(matrix(rexp(99), ncol=3))
  e <- rnorm(33)
  bp <- isnormalr:::bp_test(e, X)
  testthat::expect_equal(bp,
                    structure(list(statistic = c(LM = 1.06496485330392),
                        p.value = 0.785537718398467,
                        parameter = 3L,
                        method = "studentized Breusch-Pagan heteroskedasticity test",
                        data.name = "X"),
                    class = "htest"))
  set.seed(333)
  X <- data.frame(matrix(rexp(22), ncol=2))
  e <- rnorm(11)
  bp <- isnormalr:::bp_test(e, X)
  testthat::expect_equal(bp,
                    structure(list(statistic = c(LM = 0.534879623985584),
                        p.value = 0.765336393308958,
                        parameter = 2L,
                        method = "studentized Breusch-Pagan heteroskedasticity test",
                        data.name = "X"),
                    class = "htest"))

})
