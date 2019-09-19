#' @title
#' Breusch-Pagan Test
#'
#' @description
#' Calculate a Breusch-Pegan test for heteroscedasticity
#'
#' @param e a vector with error terms
#' @param X a dataframe with k parameter and n observations
#'
#' @details
#' The Breusch-Pagan test fits a linear regression model to the
#' residuals of a linear regression model and rejects if too
#' much of the variance is explained by the additional explanatory
#' variables. Under H0 the test statistic of the Breusch-Pagan test
#' follows a chi-squared distribution with k degrees of freedom.
#'
#' @return
#' An htest object is returned. The critical value and the p-value are
#' contained in the object.
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
#' olsdiagnosticR:::bp_test(err1, X)
#' olsdiagnosticR:::bp_test(err2, X)
#' }
bp_test <- function(e, X){
  # save the name of the input variable
  DNAME <- deparse(substitute(X))
  # deleting the intercept
  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
  }
  # crate a DataFrame
  X <- data.frame(X)


  e2 <- e^2  # squared residuals

  n <- dim(X)[1]  # number of observations
  k <- dim(X)[2]  # number of parameters

  mod <- stats::lm(e2 ~., data=X)  # fit a linear model

  # get the R2
  r2 <- summary(mod)$r.squared

  # calculation of the test statistic
  LM <- n * r2
  # calculate the pvalue
  pval <- stats::pchisq(LM, k, lower.tail = FALSE)

  # generate the return S3 htest class
  RVAL <- list(statistic = c(LM = LM),
               p.value = pval,
               parameter = c(df = k),
               method = 'Breusch-Pagan heteroskedasticity test',
               data.name = DNAME)
  class(RVAL) <- 'htest'

  return(RVAL)
}



#' @title
#' Spread-Level Plot
#'
#' @description
#' Creates plots for examining the possible dependence of spread on
#' level, or an extension of these plots to the studentized residuals
#' from linear models.
#'
#' @param fitted.value
#' a single numeric vector of data values
#'
#' @inheritParams qq_plot
#'
#' @details
#' Plots abs(studentized residuals) vs. fitted values.
#'
#' @return
#' qqplot-object
#'
#' @references
#' Fox, J. (1997) Applied Regression, Linear Models, and Related Methods.
#' Sage. Hoaglin, D. C., Mosteller, F. and Tukey, J. W. (Eds.) (1983)
#' Understanding Robust and Exploratory Data Analysis. Wiley.
#'
#' @examples
#' \dontrun{
#' y_hat <- rnorm(100)
#' X <- (matrix(rnorm(1000), nrow = 100))
#' resid <- rnorm(100)
#' inf_obs <- olsdiagnosticR:::influence_observation(X = X, e = error)
#' olsdiagnosticR:::Spread_level_plot(fitted.value = y_hat,
#'                                    influence_obs = inf_obs)
#' }
#'
#'
#' @import
#' ggplot2
Spread_level_plot <- function(fitted.value, influence_obs){

  n <- length(fitted.value)
  # create a DataFrame
  df <- data.frame(y_hat = fitted.value,
                   rstudent = influence_obs$studentized.residuals,
                   abs.rstudent =
                     abs(influence_obs$studentized.residuals))

  # ------------- generate plot
  p2 <- ggplot(df, aes(x=y_hat, y = abs.rstudent)) +
          geom_point() +
          geom_smooth(method='lm',formula=y~x, color='#666666') +
    scale_x_continuous(name='Fitted Values') +
    scale_y_continuous(name='Absolute Studentized Residuals') +
    ggplot2::ggtitle('Spread-Level Plot')

  # create a DataFrame for the necessary values
  df2 <- data.frame('Hat.Values'=influence_obs$leverage.value,
                    'Studentized.Residuals'=
                      influence_obs$studentized.residuals,
                    'Cooks.Distance'= influence_obs$cooks.distance)

  # initialization of a vector for the influential obs.
  all.bool <- rep(FALSE, times = n)
  # find the possible influential for Hat Values
  hatval <- order(df2$Hat.Values, decreasing = TRUE)[1:2]
  # find the possible influential obs for rstud
  rstud <- order(abs(df2$Studentized.Residuals), decreasing = TRUE)[1:2]
  # find the possible influential obs. for CookD
  cook <-order(df2$Cooks.Distance, decreasing = TRUE)[1:2]
  # keep only all influential obs. one time
  all <- union(rstud, union(hatval, cook))
  # define the influential obs.
  all.bool[all] <- TRUE
  # save the rownames
  names <- rownames(df2)

  # color the points and add name
  p2 <- p2 + ggrepel::geom_text_repel(aes(label=
                                            ifelse(all.bool,
                                                   names, '')),
                                      size = 3, color = 'red') +
    #add the color
             ggplot2::geom_point(data = df[all.bool,],
                                 aes(x=y_hat,
                                    y=abs.rstudent),
                                 colour = 'red')

  #add the theme
  p2 <- p2 + theme_olsdiagnosticR()

  # add footnote
  p2 <- p2 + labs(caption = 'possible outlier')


  return(p2)
}
