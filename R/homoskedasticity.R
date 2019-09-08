#' @title
#' Breusch-Pagan Test
#'
#' @description
#' Calculate a Breusch-Pegan Test for homoscedasticity
#'
#' @aliase \code{isnormalr:::bp_test(u, X)}
#'
#' @param u a vector with error terms
#' @param X a dataframe with k parameter and n observations
#'
#' @details
#' The Breusch Pagan Test and its special case, the White-Test, are
#' statistical tests to test heteroskedasticity. In particular, they
#' are used to verify the assumption of homoskeletal elasticity in
#' regression analysis.
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
  # Calculate the p-value
  pval <- pchisq(LM, k, lower.tail = FALSE)

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
Spread.level.plot <- function(fitted.value, StudRes , CookD, Leverage){
  # generated two plots
  # 1. Studentizied Residuals vs. Fitted Value
  # 2. log(abs(Studentizied Residuals)) vs log(fitted Values)

  df <- data.frame(y_hat = fitted.value,
                   #log.y_hat = log10(fitted.value),
                   rstudent = StudRes,
                   abs.rstudent = abs(StudRes))

  # Plot 1
  p1 <- ggplot(df, aes(x=y_hat, y=rstudent)) +
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

  # Extream Value
  thresh_Lev <- 2 * mean(Leverage)
  thresh_CookD <- 1

  df2 <- data.frame('Hat.Values'=Leverage,
                    'Studentized.Residuals'=StudRes,
                    'Cooks.Distance'=CookD)
  ## add possible influencial observations
  all.bool <- rep(FALSE, times = n)
  hatval <- which(df2$Hat.Values >=  thresh_Lev)
  rstud <- order(abs(df2$Studentized.Residuals),
                 decreasing = TRUE)[1:2]
  cook <- which(df2$Cooks.Distance > 1)
  all <- sort(union(rstud, union(hatval, cook)))
  all.bool[all] <- TRUE
  names <- rownames(df2)

  # Colour the points and add name
  p2 <- p2 + ggrepel::geom_text_repel(aes(label=
                                            ifelse(all.bool,
                                                   names, "")),
                                      size = 3, color = 'red') +
    #add the color
             ggplot2::geom_point(data = df[all.bool,],
                                 aes(x=y_hat,
                                    y=abs.rstudent),
                                 colour = 'red')

  #add the theme
  p1 <- p1 + theme_isnormalr()

  p2 <- p2 + theme_isnormalr()

  # Generate an s3 object
  sl <- list('Spread.level' = p2,
             'Student.Fitted' = p1)

  class(sl) <- 'Spread Level'

  return(sl)
}
