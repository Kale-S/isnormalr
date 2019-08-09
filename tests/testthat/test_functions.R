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




