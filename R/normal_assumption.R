#' @title
#' Histogramm of the residuals
#'
#' @description
#' This function generates a histogramm of the residuals with
#' the theoretical normal density.
#'
#' @param X
#' a single numeric vector of data values.
#'
#' @return
#' ggplot-object.
#'
#' @examples
#' set.seed(1234)
#' X <- rnorm(200)
#' olsdiagnosticR:::resid_hist(X = X)
#'
#' @import
#' ggplot2

resid_hist <- function(X){
  # save the number of observations to calculate the bins
  n <- length(X)
  # calculate the number of bins
  bins <- Square_root(n)

  # safe the mean of the residuals
  X.mean <-  mean(X)
  # safe the standard deviation of the residuals
  X.sd <- stats::sd(X)
  # safe the input as data.frame
  X <- data.frame(X)

  # ------------- generate plot
  # define the x grid
  x.grid <- seq(-5, 5, length.out=10000)
  # calculation of the standard normal density
  dens <- stats::dnorm(x.grid, 0, 1)
  # save all results in a data.frame
  df <- with(X, data.frame(x=x.grid, y=dens))

  # creating the plot
  h <- ggplot2::ggplot(X, aes(x = X)) +
    # add the histogram
    ggplot2::geom_histogram(aes(y=..density..),
                                col=I('black'),
                          alpha=0.6, bins=bins)
  # add the standard normal density
  h <- h + ggplot2::geom_line(data=df, aes(x=x.grid, y=y),
                              color='red')

  # add ylabel, xlabel and title
  h <- h + scale_x_continuous(name = 'Residuals') +
           scale_y_continuous(name = 'Density') +
           ggtitle('Residual Histogram')
  # add theme
  h <- h + theme_olsdiagnosticR()

  return(h)

}


#' @title
#' QQ-Plot of the residuals
#'
#' @description
#' Plots  studentized residuals from a linear model, against
#' theoretical quantiles of a comparison distribution.
#'
#' @param influence_obs
#' a list with following values:
#' leverage.value, cooks.distance, studentized.residuals
#'
#' @details
#' Draws theoretical quantile-comparison plots for variables and for
#' studentized residuals from a linear model. A comparison line is
#' drawn on the plot either through the quartiles of the two
#' distributions, or by robust regression.
#'
#' @return
#' qqplot-object
#'
#' @references
#' Fox, J. 2016 Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
#'
#' Fox, J. and Weisberg, S. 2019 An R Companion to Applied Regression, Third Edition, Sage.
#' Atkinson, A. C. 1985 Plots, Transformations, and Regression. Oxford.
#'
#' @examples
#' \dontrun{
#' X <- data.frame(matrix(rnorm(1000), nrow = 100))
#' resid <- rnorm(100)
#' inf_obs <- olsdiagnosticR:::influence_observation(X = X, e = error)
#' olsdiagnosticR:::qq_plot(influence_obs = inf_obs)
#' }
#'
#' @import
#' ggplot2
#'
qq_plot <- function(influence_obs){

  # calculation of the number of observations
  n <- length(influence_obs$studentized.residuals)

  # compute n + 1 evenly spaced points on the interval (0, 1)
  prob <- stats::ppoints(n)
  # calculate the theoretical quantiles
  normal.quantiles <- stats::qnorm(prob, mean = 0L, sd = 1L)

  # calculation of the abline slope an intercept
  y <- stats::quantile(influence_obs$studentized.residuals, c(0.25, 0.75))
  x <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  intercept <- y[1L] - slope * x[1L]

  # build a data.frame
  df <- data.frame(normal.quantiles = sort(normal.quantiles),
                   sample.quantiles =
                     sort(influence_obs$studentized.residuals))

  names <- names(sort(influence_obs$studentized.residuals))


  # ------------- generate plot
  # create the plot
  qq <- ggplot2::ggplot(df, aes(x=normal.quantiles,
                                y=sample.quantiles)) +
        # add the points
        ggplot2::geom_point() +
        # add a gray abline
        ggplot2::geom_abline(intercept = intercept, slope = slope,
                             color='#666666', size=1.2)

  # ------------- extream values
  # create a DataFrame for the necessary values
  df2 <- data.frame('Hat.Values'=influence_obs$leverage.value,
                   'Studentized.Residuals'=
                     influence_obs$studentized.residuals,
                   'Cooks.Distance'=influence_obs$cooks.distance)
  # initialization of a vector for the influential obs.
  all.bool <- rep(FALSE, times = n)
  # find the possible influential for Hat Values
  hatval <- order(df2$Hat.Values, decreasing = TRUE)[1:2]
  # find the possible influential obs for rstud
  rstud <- order(abs(df2$Studentized.Residuals), decreasing = TRUE)[1:2]
  # find the possible influential obs. for CookD
  cook <-order(df2$Cooks.Distance, decreasing = TRUE)[1:2]
  # Keep only all influential obs. one time
  all <- union(rstud, union(hatval, cook))
  # define the influential obs.
  all.bool[all] <- TRUE
  # sort the boolean
  all.bool <- all.bool[order(df2$Studentized.Residuals)]


  # ------------- add the influential obs
  # add the label of the influential obs
  qq <- qq + ggrepel::geom_text_repel(aes(label=
                                      ifelse(all.bool,
                                             names, '')),
                                      size = 3, color = 'red') +
             # add the color to the influential obs.
             ggplot2::geom_point(data = df[all.bool,],
                                 aes(x=normal.quantiles,
                                     y=sample.quantiles),
                                     colour = 'red')


  # add xlabel, ylabel and title
  qq <- qq +
    ggplot2::scale_x_continuous(name = 'Theoretical Quatiles') +
    ggplot2::scale_y_continuous(name = 'Studentiued residuals') +
    ggplot2::ggtitle('Normal Q-Q')
  # add the theme
  qq <- qq + theme_olsdiagnosticR()

  # add footnote
  qq  <- qq + labs(caption = 'possible outlier')
  return(qq)
}

#' @title
#' Shapiro-Wilk Normality Test
#'
#' @description
#' Performs the Shapiro-Wilk test for normality.
#'
#' @param X
#' a single numeric vector of data values, the number of
#' observations must be between 3 and 5000.
#'
#' @details
#' The Shapiro-Wilk test is a statistical significance test that
#' tests the hypothesis that the underlying population of a sample
#' is normally distributed. The test was developed by Samuel Shapiro
#' and Martin Wilk and first presented in 1965.
#' The null hypothesis H0 assumes that there is a normal distribution
#' of the population. On the other hand, the alternative hypothesis
#' H1 that there is no normal distribution. If the value of the test
#' statistic W is greater than the critical value Wcritical, the null
#' hypothesis is not rejected, and a normal distribution is assumed.
#' Alternatively, if the p-value of the test is determined, the null
#' hypothesis is usually not rejected if the p-value is greater than
#' the specified significance level alpha.
#' The test can be used to check univariate samples with 3 to 5000
#' observations. In addition to other known tests for normal
#' distribution, such as the Kolmogorov-Smirnow test or the
#' Chi-square test, the Shapiro-Wilk test is distinguished by its
#' comparatively high-test strength in numerous test situations,
#' especially when testing smaller samples with n<50.
#'
#' @return
#' An htest object is returned. The critical value and the p-value are
#' contained in the object.
#'
#' @references
#' Sam S. Shapiro, Martin Bradbury Wilk: An analysis of variance
#' test for normality (for complete samples), Biometrika, 52(3/4),
#' 1965, pp. 591–611, doi:10.1093/biomet/52.3-4.591, JSTOR 2333709.
#'
#' Patrick Royston (1982). An extension of Shapiro and Wilk's W test
#' for normality to large samples. Applied Statistics, 31, 115–124.
#' doi: 10.2307/2347973.
#'
#' Patrick Royston (1982). Algorithm AS 181: The W test for Normality.
#' Applied Statistics, 31, 176–180. doi: 10.2307/2347986.
#'
#' Patrick Royston (1995). Remark AS R94: A remark on Algorithm AS 181:
#' The W test for normality. Applied Statistics, 44, 547–551.
#' doi: 10.2307/2986146.
#'
#' @examples
#' \dontrun{
#' z <- rnorm(100)
#' olsdiagnosticR:::Shapiro_Wilk_test(X = z)
#'
#' y <- rexp(100)
#' olsdiagnosticR:::Shapiro_Wilk_test(X = y)
#' }
Shapiro_Wilk_test <- function(X){
  # save the name of the input variable
  DNAME <- deparse(substitute(X))
  # sort the X values
  X <- sort(X[stats::complete.cases((X))])
  # define n
  n <- length(X)

  # define a hf that calculates the inverse normal distribution
  hf <- function(i, n){
    v <- (i - (3/8)) / (n + 0.25)
    return(stats::qnorm(v))
  }
  # calculate the inversnormal
  m <- sapply(seq(1, n), hf, n=n)
  # calculate the sqrt sum of m
  m2 <- t(m) %*% m

  # calculate u
  u <- 1 / sqrt(n)

  # initialization of a vector to store the weight
  a <- rep(0, times=n)
  # calculate the last weight
  a[n] <- -2.706056 * u^5 + 4.434685 * u^4 - 2.071190 * u^3 -
    0.147981 * u^2 + 0.221157 * u + m[n] * m2^-0.5
  # calculate the second last weight
  a[n-1] <- -3.582633 * u^5 + 5.682633 * u^4 - 1.752461 * u^3 -
    0.293762 * u^2 + 0.042981 * u + m[n-1] * m2^-0.5
  # calculate the first weight
  a[1] <- -a[n]
  # calculate the second weight
  a[2] <- -a[n-1]

  ## calculate e
  e <- (m2 - 2 * m[n]^2 - 2 * m[n-1]^2) / (1 - 2 * a[n]^2 - 2 * a[n-1]^2)

  # calculate the other weights
  for(i in seq(3, n-2)){
    a[i] <- m[i] / sqrt(e)
  }

  # calculate the W statistic
  W <- (t(a) %*% sort(X))^2 / sum((X - mean(X))^2)

  # calculation of mu for the Z test statistic
  mu <- 0.0038915 * log(n)^3 - 0.083751 * log(n)^2 - 0.31082 * log(n) -
        1.5861
  # calculation of sigma for the Z test statistic
  sig <- exp(0.0030302 * log(n)^2 - 0.082676 * log(n) - 0.4803)
  # calculation of the Z test statistic
  z <- (log(1 - W) - mu) / sig
  # calculation of the pvalue
  pval <- 1 - stats::pnorm(z)

  # save the as an s3 htest object
  RVAL <- list(statistic = c(w = W),
               p.value = pval,
               method = 'Shapiro-Wilk normality test',
               data.name = DNAME)
  class(RVAL) <- 'htest'
  return(RVAL)
}


#' @title
#' Jarque-Bera-Test
#'
#' @description
#' Performs the Jarque-Bera test for normality.
#'
#' @inheritParams resid_hist
#'
#' @details
#' The Jarque-Bera test statistic tests the null that the data is normally
#' distributed against an alternative that the data follow some other distribution.
#' The test statistic is based on two moments of the data, the skewness, and the kurtosis,
#' and has an asymptotic chi^2 distribution.
#' The test statistic is defined:
#'
#'    JB = n(S^2/6+(K-3)^2/24)
#'
#' where n is the number of data points, S is the sample skewness,
#' and K is the sample kurtosis of the data.
#'
#' @return
#' An htest object is returned. The critical value and the p-value
#' are contained in the object.
#'
#' @references
#' Jarque, C. M. and Bera, A. K. (1980). Efficient test for normality,
#' homoscedasticity and serial independence of residuals.
#' Economic Letters, 6(3), pp. 255-259.
#'
#' @examples
#' \dontrun{
#' z <- rnorm(100)
#' olsdiagnosticR:::jarque_bera(X = z)
#'
#' y <- rexp(100)
#' olsdiagnosticR:::jarque_bera(X = y)
#' }
jarque_bera <- function(X){
  DNAME <- deparse(substitute(X))
  n <- length(X)
  x_bar <- mean(X)

  # test statistics
  S <- (sum((X - x_bar)^3)/n) / (sum((X - x_bar)^2)/n)^(3/2)
  K <- (sum((X - x_bar)^4)/n) / (sum((X - x_bar)^2)/n)^2

  JB <- n * S^2 / 6 + n * (K - 3)^2 / 24
  # critical value
  jb <- stats::qchisq(0.95, 2)

  # pvalue
  pval <- 1 - stats::pchisq(JB, df = 2)
  df = 2
  # generation of an s3 htest object
  RVAL <- list(statistic = c(JB = JB),
               p.value = pval,
               parameter = c(df = 2),
               method = 'Jarque-Bera normality test',
               data.name = DNAME)
  class(RVAL) <- 'htest'
  return(RVAL)
}

#' @title
#' Anderson-Darling-Test
#'
#' @description
#' Performs the Anderson-Darling-Test test for normality.
#'
#' @inheritParams resid_hist
#'
#' @details
#' The Anderson-Darling tests the null hypothesis that a sample is
#' drawn from a population that follows a particular distribution.
#' For the Anderson-Darling test, the critical values depend on which
#' distribution is being tested against. This function works for
#' normal distributions
#'
#' @return
#' An htest object is returned. The critical value and the p-value
#' are contained in the object.
#'
#' @references
#' Stephens, M. A. (1974). EDF Statistics for Goodness of Fit and Some
#' Comparisons, Journal of the American Statistical Association,
#' Vol. 69, pp. 730-737.
#'
#' Stephens, M. A. (1976). Asymptotic Results for Goodness-of-Fit
#' Statistics with Unknown Parameters, Annals of Statistics, Vol. 4,
#' pp. 357-369.
#'
#' Stephens, M. A. (1977). Goodness of Fit for the Extreme Value
#' Distribution, Biometrika, Vol. 64, pp. 583-588.
#'
#' Stephens, M. A. (1977). Goodness of Fit with Special Reference
#' to Tests for Exponentiality , Technical Report No. 262,
#' Department of Statistics, Stanford University, Stanford, CA.
#'
#' Stephens, M. A. (1979). Tests of Fit for the Logistic Distribution
#' Based on the Empirical Distribution Function, Biometrika,
#' Vol. 66, pp. 591-595.
#'
#'
#' @examples
#' \dontrun{
#' z <- rnorm(100)
#' olsdiagnosticR:::anderson_darling_test(X = z)
#'
#' y <- rexp(100)
#' olsdiagnosticR:::anderson_darling_test(X = y)
#' }
anderson_darling_test <- function(X){
  DNAME <- deparse(substitute(X))
  X <- sort(X[stats::complete.cases(X)])
  n <- length(X)
  x_bar <- mean(X)
  standev <- stats::sd(X)

  par1 <- stats::pnorm((X - x_bar) / standev, log.p = TRUE)
  par2 <- stats::pnorm(-(X - x_bar) / standev, log.p = TRUE)

  d <- (2 * seq(1:n) - 1) * (par1 + rev(par2))
  A <- -n - mean(d)

  # Computation of pval form Table 4.9 in Stephens (1986)
  Z <- (1 + 0.75 / n + 2.25 / n^2) * A
  if (Z < 0.2) {

    pval <- 1 - exp(-13.436 + 101.14 * Z - 223.73 * Z^2)

  } else if (Z < 0.34) {

    pval <- 1 - exp(-8.318 + 42.796 * Z - 59.938 * Z^2)

  } else if (Z < 0.6) {

    pval <- exp(0.9177 - 4.279 * Z - 1.38 * Z^2)

  } else if (Z < 10L) {

    pval <- exp(1.2937 - 5.709 * Z + 0.0186 * Z^2)

  } else {

    pval <- 3.7e-24

  }
  # generation of an s3 htest object
  RVAL <- list(statistic = c(A = A),
               p.value = pval,
               method = 'Anderson-Darling normality test',
               data.name = DNAME)
  class(RVAL) <- 'htest'

  return(RVAL)
}


Square_root <- function(n){
  bw <- round(sqrt(n))
}

