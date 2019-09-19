context('Test the functons form normal_assumption.R')


#### Shapiro_Wilk_test
testthat::test_that('Shapiro_Wilk_test`s output is:
                    list and s3 object', {
                      set.seed(137)
                      error <- rnorm(20)
                      sw <- olsdiagnosticR:::Shapiro_Wilk_test(error)
                      testthat::expect_type(sw, 'list')
                      testthat::expect_s3_class(sw, 'htest')
                    })
testthat::test_that('Shapiro_Wilk_test has the right length', {
  set.seed(137)
  error <- rnorm(39)
  sw <- olsdiagnosticR:::Shapiro_Wilk_test(error)
  testthat::expect_true(length(sw) == 4)
})
testthat::test_that('Shapiro_Wilk_test get the right resutl', {
  set.seed(1373)
  error <- rnorm(100)
  sw <- olsdiagnosticR:::Shapiro_Wilk_test(error)
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
  sw <- olsdiagnosticR:::Shapiro_Wilk_test(error)
  testthat::expect_equal(sw,
                         structure(list(
                           statistic = c(w = 0.835614640946988),
                           p.value = structure(3.61296548234691e-09,
                                               .Dim = c(1L, 1L)),
                           method = "Shapiro-Wilk normality test",
                           data.name = "error"),
                           class = "htest"))
})


#### jarque_bera
testthat::test_that('jarque_bera`s output is:
                    list and s3 object',{
  set.seed(123)
  error <- rnorm(20)
  jb <- olsdiagnosticR:::jarque_bera(error)
  testthat::expect_true(typeof(jb) == 'list')
  testthat::expect_s3_class(jb, 'htest')
})
testthat::test_that('jarque_bera has the right length', {
  set.seed(123)
  error <- rnorm(30)
  jb <- olsdiagnosticR:::jarque_bera(error)
  testthat::expect_true(length(jb) == 5)
})

testthat::test_that('Elements inf jarque_bera has the
                    right of entries', {
  set.seed(137)
  error <- rnorm(200)
  jb <- olsdiagnosticR:::jarque_bera(error)
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
  jb <- olsdiagnosticR:::jarque_bera(error)
  testthat::expect_equal(jb,
                     structure(list(
                       statistic = c(JB = 994.215814661187),
                       p.value = 0,
                       parameter = c(df = 2),
                       method = "Jarque-Bera normality test",
                       data.name = "error"),
                     class = "htest"))

})

#### anderson_darling_tes
testthat::test_that('anderson_darling_test`s output is:
                    list and s3 object',{
  set.seed(123)
  error <- rnorm(20)
  ad <- olsdiagnosticR:::anderson_darling_test(error)
  testthat::expect_true(typeof(ad) == 'list')
  testthat::expect_s3_class(ad, 'htest')
})

testthat::test_that('anderson_darling_test has the right length', {
  set.seed(123)
  error <- rnorm(30)
  ad <- olsdiagnosticR:::anderson_darling_test(error)
  testthat::expect_true(length(ad) == 4)
})

testthat::test_that('Elements inf anderson_darling_test has the
                    right of entries', {
  set.seed(137)
  error <- rnorm(200)
  ad <- olsdiagnosticR:::anderson_darling_test(error)
  testthat::expect_equal(ad,
                         structure(list(
                         statistic = c(A = 0.49207792204021),
                         p.value = 0.215969387994989,
                         method = "Anderson-Darling normality test",
                         data.name = "error"),
                         class = "htest"))
   set.seed(137)
   error <- rexp(200)
   ad <- olsdiagnosticR:::anderson_darling_test(error)
   testthat::expect_equal(ad,
                          structure(list(
                          statistic = c(A = 11.8226573559874),
                          p.value = 3.7e-24,
                          method = "Anderson-Darling normality test",
                          data.name = "error"),
                          class = "htest"))
})

#### Square_root
testthat::test_that('Square_root get the right number as output',{
  testthat::expect_equal(olsdiagnosticR:::Square_root(4), 2)
  testthat::expect_equal(olsdiagnosticR:::Square_root(12), 3)
  testthat::expect_equal(olsdiagnosticR:::Square_root(2893), 54)
})
testthat::test_that('Square_root returns a double',{
  testthat::expect_equal(typeof(olsdiagnosticR:::Square_root(4)), 'double')
  testthat::expect_equal(typeof(olsdiagnosticR:::Square_root(12)), 'double')
})
testthat::test_that('Square_root returns warnings by negativ numbers', {
  testthat::expect_warning(olsdiagnosticR:::Square_root(-9))
  testthat::expect_warning(olsdiagnosticR:::Square_root(-80))
})
