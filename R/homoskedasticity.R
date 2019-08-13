#' @title
#' Breusch-Pagan Test
#'
#' @param model
#'
#' @return kritical value
#' @export
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
#' ggolot2
#' @examples
Spread.level.plot <- function(fitted.value, t){
  # generated two plots
  # 1. Studentizied Residuals vs. Fitted Value
  # 2. log(abs(Studentizied Residuals)) vs log(fitted Values)

  df <- data.frame(y_hat = fitted.value,
                   #log.y_hat = log10(fitted.value),
                   rstudent = t,
                   abs.rstudent = abs(t))

  # Plot 1
  p1 <- ggplot(df, aes(x=y_hat, y=t)) +
          geom_point() +
          geom_hline(yintercept = 0, lty=2, col = 'blue')

  p2 <- ggplot(df, aes(x=y_hat, y = abs.rstudent)) +
          geom_point() +
          geom_smooth(method='lm',formula=y~x)

  # Generate an s3 object
  sl <- list('Spread.level' = p2,
             'Student.Fitted' = p1)

  class(sl) <- 'Spread Level'

  return(sl)
}
