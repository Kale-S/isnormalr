#' @title
#' Breusch-Pagan Test
#'
#' @param model
#'
#' @return kritical value
#' @export
#' @import
#' lmtest
#' @examples
bp_test <- function(u, X){

  ## implementing the LM statistic ##
  u2 <- u^2  # squerd residuals

  n <- dim(X)[1]
  k <- dim(X)[2]

  mod <- lm(u2 ~., data=X)  # fit a linear model

  r2 <- summary(mod)$r.squared

  # Calculate the test staitisc
  LM <- n * r2

  # calculate critical value
  LM.k <- qchisq(0.95, df=k)

  ## implementing the F statistic ##
  # Calculate the test statistic
  F.s <- (r2 / k) / ((1 - r2) / n - k - 1)

  # calculate the critical value
  F.k <- qf(0.95, k, n - k - 1)

  ## generate the return S3 class ##
  # definition of S3 class
  bp <- list(LM.statistic = LM, LM.kritical.value = LM.k,
            F.statistic = F.s, F.kritical.value = F.k)
  class(bp) <- 'bp-Test'


  return(bp)


}

#' Spread-Level Plot
#'
#' @param model
#'
#' @return
#' @export
#' @import
#' car
#' @examples
Spread.level.plot <- function(model){
  res <- car::spreadLevelPlot(model)

}
