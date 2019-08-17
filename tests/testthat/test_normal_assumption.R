context('Test the functons form normal_assumption.R')


#### Shapiro_Wilk.test
testthat::test_that('Shapiro_Wilk.test`s output is:
                    list and s3 object', {
                      set.seed(137)
                      error <- rnorm(20)
                      sw <- isnormalr:::Shapiro_Wilk.test(error)
                      testthat::expect_type(sw, 'list')
                      testthat::expect_s3_class(sw, 'htest')
                    })
testthat::test_that('Shapiro_Wilk.test has the right length', {
  set.seed(137)
  error <- rnorm(39)
  sw <- isnormalr:::Shapiro_Wilk.test(error)
  testthat::expect_true(length(sw) == 4)
})
testthat::test_that('Shapiro_Wilk.test get the right resutl', {
  set.seed(1373)
  error <- rnorm(100)
  sw <- isnormalr:::Shapiro_Wilk.test(error)
  testthat::expect_equal(sw,
                         structure(list(
                           statistic = c(w = 0.986630627162333),
                           p.value = structure(0.413269236169449,
                                               .Dim = c(1L, 1L)),
                           method = "Shapiro-Wilk normality test",
                           data.name = "error"),
                           class = "htest"))

  set.seed(1373)
  error <- rexp(100, 0.2)
  sw <- isnormalr:::Shapiro_Wilk.test(error)
  testthat::expect_equal(sw,
                         structure(list(
                           statistic = c(w = 0.835614640946988),
                           p.value = structure(3.61296548234691e-09,
                                               .Dim = c(1L, 1L)),
                           method = "Shapiro-Wilk normality test",
                           data.name = "error"),
                           class = "htest"))
})


#### Jarque.bera
testthat::test_that('jarque.bera`s output is:
                    list and s3 object',{
  set.seed(123)
  error <- rnorm(20)
  jb <- isnormalr:::jarque.bera(error)
  testthat::expect_true(typeof(jb) == 'list')
  testthat::expect_s3_class(jb, 'htest')
})
testthat::test_that('jarque.bera has the right length', {
  set.seed(123)
  error <- rnorm(30)
  jb <- isnormalr:::jarque.bera(error)
  testthat::expect_true(length(jb) == 5)
})

testthat::test_that('Elements inf jarque.bera has the
                    right of entries', {
  set.seed(137)
  error <- rnorm(200)
  jb <- isnormalr:::jarque.bera(error)
  testthat::expect_equal(jb,
                 structure(list(
                   statistic = c(JB = 7.16502968309851),
                   p.value = 0.0278056833661366,
                   parameter = c(df = 2),
                   method = "Jarque-Bera normality test",
                   data.name = "error"),
                 class = "htest"))
  set.seed(137)
  error <- rexp(200)
  jb <- isnormalr:::jarque.bera(error)
  testthat::expect_equal(jb,
                     structure(list(
                       statistic = c(JB = 994.215814661187),
                       p.value = 0,
                       parameter = c(df = 2),
                       method = "Jarque-Bera normality test",
                       data.name = "error"),
                     class = "htest"))

})


#### cramerv_mises.test
testthat::test_that('cramerv_mises.test output is:
                    list and s3 object', {
  set.seed(137)
  error <- rnorm(200)
  k <- isnormalr:::cramerv_mises.test(error)
  testthat::expect_type(k, 'list')
  testthat::expect_s3_class(k, 'htest')
})
testthat::test_that('cramerv_mise.test hass the right length', {
  set.seed(137)
  error <- rnorm(200)
  k <- isnormalr::cramerv_mises.test(error)
  testthat::expect_true(length(k) == 4)
})
# because the pvalue dosent work
#testthat::test_that('cramerv_mises.test get the right results', {
#  set.seed(137)
#  error <- rnorm(200)
#  k <- isnormalr:::cramerv_mises.test(error)
#  testthat::expect_equal(k,
#                      )
#
#  set.seed(137)
#  error <- rexp(200)
#  k <- isnormalr:::cramerv_mises.test(error)
#  testthat::expect_equal(k,
#                      )
#})

#### sturge_rule
testthat::test_that('sturge_rule get the right number as output',{
  testthat::expect_equal(sturge_rule(4), 3)
  testthat::expect_equal(sturge_rule(12), 5)
  testthat::expect_equal(sturge_rule(2893), 12)
})
testthat::test_that('sturge_rule returns a double',{
  testthat::expect_equal(typeof(sturge_rule(4)), 'double')
  testthat::expect_equal(typeof(sturge_rule(12)), 'double')
})
testthat::test_that('sturge_rule returns warnings by negativ numbers', {
  testthat::expect_warning(sturge_rule(-9))
  testthat::expect_warning(sturge_rule(-80))
})
