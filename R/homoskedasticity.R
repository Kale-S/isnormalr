#' @title
#' Breusch-Pagan Test
#' @keywords
#' htest
#' @param u a vector with error terms
#' @param X a dataframe with k parameter and n observations
#'
#'
#' @return
#' A list with class htest cointaining the following components:
#'
#' statisic
#'   the value of the statistic.
#'
#' p.value
#'   the p-value of the test
#'
#' parameter
#'   degrees of freedom.
#'
#' method
#'   a character string indicating what type of test was performed.
#'
#' data.name
#'   a character sting giving the name of the data.
#'
#' @references
#' T.S. Breusch & A.R. Pagan (1979), A Simple Test for Heteroscedasticity and Random Coefficient Variation.
#' Econometrica 47, 1287--1294
#'
#' R. Koenker (1981), A Note on Studentizing a Test for Heteroscedasticity. Journal of Econometrics 17, 107--112.
#' @examples
#'  \dontrun{
#' ## generate a regressor
#' X <- rep(c(1, 2), 50)
#' ## generate homoskedastic errors
#' err1 <- rnorm(100)
#' ## generate heteroskedastic errors
#' err2 <- rnorm(100, sd = x)
#'
#' # perform the Breusch-Pagan test
#' bp_test(err1, X)
#' bp_test(err2, X)
#' }
bp_test <- function(u, X){

  DNAME <- deparse(substitute(X))

  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
  }
  X <- data.frame(X)

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



  pval <- 1 - pchisq(LM, df=k)
  ## generate the return S3 htest class ##
  RVAL <- list(statistic = c(LM = LM),
               p.value = pval,
               parameter = c(df = k),
               method = "studentized Breusch-Pagan heteroskedasticity test",
               data.name = DNAME)
  class(RVAL) <- "htest"


  return(RVAL)
}



#' Spread-Level Plot
#'
#' @param model
#'
#' @return
#' @export
#' @import
#' ggplot2
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
          geom_hline(yintercept = 0, lty=2, col = '#666666',
                     size = 1) +
          scale_x_continuous(name='Fitted Values') +
          scale_y_continuous(name='Studentized Residuals')
  # add nd title
  p1 <- p1 +
        ggtitle('Spread-Level Plot_Other')

  p2 <- ggplot(df, aes(x=y_hat, y = abs.rstudent)) +
          geom_point() +
          geom_smooth(method='lm',formula=y~x, color='#666666') +
    scale_x_continuous(name='Fitted Values') +
    scale_y_continuous(name='Absolute Studentized Residuals') +
    ggplot2::ggtitle('Spread-Level Plot')

  #add the theme
  p1 <- p1 + theme_isnormalr()

  p2 <- p2 + theme_isnormalr()

  # Generate an s3 object
  sl <- list('Spread.level' = p2,
             'Student.Fitted' = p1)

  class(sl) <- 'Spread Level'

  return(sl)
}
