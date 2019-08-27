#' @title
#' Histogramm of the residuals
#'
#' @description
#' This function genertates a Histogramm of the residuals with a normal Density
#'
#'
#' @param X a numeric vector of data values
#'
#' @return Plot a histogram of the residuals with a normal density
#' @export
#'
#' @import
#' ggplot2
#'
#' @examples
#' set.seed(1234)
#' X <- matrix(c(rnorm(200), rnorm(200)), ncol=2)
#' e <- rnorm(200)
#' b <- matrix(x(2, 5), ncol=1)
#' y <- X %*% b + e
#' resid_hist(lm(y ~ X))

resid_hist <- function(X){
  # safe the number of obs. for the calculation of the bins
  n <- length(X)
  bins <- isnormalr:::Square.root(n)
  # safe the input as data.frame
  X <- data.frame(X)
  # safe the mean of the residuals
  X.mean <-  colMeans(X)
  # safe the standard diviation of the residals
  X.sd <- apply(X, 2, sd, na.rm=TRUE)

  # ------------- generate plot
  x.grid <- seq(-5, 5, length.out=10000)
  dens <- dnorm(x.grid, 0, 1)
  df <- with(X, data.frame(x=x.grid, y=dens))
  #bins <- isnormalr::sturge_rule(n)

  h <- ggplot2::ggplot(X, aes(x = X)) +
    ggplot2::geom_histogram(aes(y=..density..),
                                col=I('black'),
                          alpha=0.6, bins=bins)
  h <- h + ggplot2::geom_line(data=df, aes(x=x.grid, y=y), color='red')

  # add ylabel, xlabel and title
  h <- h + scale_x_continuous(name = 'Residuals') +
           scale_y_continuous(name = 'Density') +
           ggtitle('Residual Histogram')
  # add theme
  h <- h + isnormalr:::theme_isnormalr()

  return(h)

}


#' @title
#' QQ-Plot of the residuals
#'
#' @description
#' Plots  studentized residuals from a linear model, against
#' theoretical quantiles of a comparison distribution.
#'
#' @keywords
#' \link{distribution}
#' \link{regression}
#'
#' @usage
#' \code{isnormalr:::qq_plot(X)}
#'
#' @details
#' Draws theoretical quantile-comparison plots for variables and for
#' studentized residuals from a linear model. A comparison line is
#' drawn on the plot either through the quartiles of the two
#' distributions, or by robust regression.
#'
#' @inheritParams resid_hist
#'
#' @return
#' An QQ-Plot
#'
#' @references
#' Fox, J. 2016 Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
#' Fox, J. and Weisberg, S. 2019 An R Companion to Applied Regression, Third Edition, Sage.
#' Atkinson, A. C. 1985 Plots, Transformations, and Regression. Oxford.
#'
#' @examples
#' \dontrun{
#' z <- rnorm(100)
#' isnormalr:::qq_plot(z)
#'
#' y <- rexp(100)
#' isnormalr:::qq_plot(y)
#'
#' }
#'
#' @import
#' ggplot2
#'
qq_plot <- function(X){
  n <- length(X)
#  mu <- mean(error)
#  sd <- sd(error)

  # Compute n + 1 evenly spaced points on the interval (0, 1)
  prob <- ppoints(n)
  # Calculate the theroetical quantiles
  normal.quantiles <- qnorm(prob, mean = 0L, sd = 1L)

  # calculation of the abline lobe an intecept
  y <- quantile(X, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  intercept <- y[1L] - slope * x[1L]

  # build a data.frame
  df <- data.frame(normal.quantiles = sort(normal.quantiles),
                   sample.quantiles = sort(X))
  names <- names(sort(X))


## plot the results
  qq <- ggplot2::ggplot(df, aes(x=normal.quantiles,
                                y=sample.quantiles)) +
        ggplot2::geom_point() +
        # gray abline
        ggplot2::geom_abline(intercept = intercept, slope = slope,
                             color="#666666", size=1.2)

  # Plot the extreme values
  extreme <- sum(df$sample.quantiles >= 2 | df$sample.quantiles <= -2)

  if(extreme >= 1 & extreme <= 7){
          # red text
    qq <- qq + ggrepel::geom_text_repel(aes(label=
                                  ifelse(sample.quantiles >= 2 |
                                         sample.quantiles <= -2,
                                         names,'')),
                                   size=3, color = 'red') +
         # red Poins
         ggplot2::geom_point(data = df[df$sample.quantiles >= 2, ],
                         aes(x=normal.quantiles,
                             y=sample.quantiles),
                         colour = 'red') +

          ggplot2::geom_point(data = df[df$sample.quantile <= -2, ],
                         aes(x=normal.quantiles,
                             y=sample.quantiles),
                         colour = 'red')

  }else if(extreme >= 7){ # default method if there are to many observations
     which.rstud <- order(abs(df$sample.quantiles), decreasing = TRUE)[1:2]
     all.bool <- rep(FALSE, times = n)
     all.bool[which.rstud] <- TRUE
     # add the Name and the colur
     qq <- qq + ggrepel::geom_text_repel(aes(label=
                                               ifelse(all.bool,
                                                      names, "")),
                                         size = 3, color = 'red') +
     # add the color
                ggplot2::geom_point(data = df[all.bool,],
                                    aes(x=normal.quantiles,
                                        y=sample.quantiles),
                                    colour = 'red')
  }
  # add xlabel, ylabel and title
  qq <- qq +
    ggplot2::scale_x_continuous(name = 'Theoretical Quatiles') +
    ggplot2::scale_y_continuous(name = 'Studentiued residuals') +
    ggplot2::ggtitle('Normal Q-Q')
  # add the theme
  qq <- qq + theme_isnormalr()
  return(qq)
}

#' @title
#' Shapiro-Wilk Normality Test
#'
#' @description
#' Performs the Shapiro-Wilk test for normality.
#'
#' @keywords
#' \link{htest}
#'
#' @usage
#' \code{isnormalr:::Shapiro_Wilk.test(X)}
#'
#' @param
#' a numeric vector of data values, the number of which must be
#' between 3 and 5000. Missing values are allowed.
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
#'
#' @examples
#' \dontrun{
#' z <- rnorm(100)
#' isnormalr:::Shapiro_Wilk.test(z)
#'
#' y <- rexp(100)
#' isnormalr:::Shapiro_Wilk.test(y)
#' }
Shapiro_Wilk.test <- function(X){
  DNAME <- deparse(substitute(X))
  # sort the X values
  X <- sort(X[complete.cases((X))])
  # define n
  n <- length(X)

  # define a hf thats calculets the
  # inverse normal distriburtion for v
  hf <- function(i, n){
    v <- (i - (3/8)) / (n + 0.25)
    return(qnorm(v))
  }
  # calculate the inversnormal
  m <- sapply(seq(1, n), hf, n=n)
  # calculate the sqrt sum of m
  m2 <- t(m) %*% m

  # calculate u
  u <- 1 / sqrt(n)

  # calculate the wights for the test
  a <- rep(0, times=n)

  a[n] <- -2.706056 * u^5 + 4.434685 * u^4 - 2.071190 * u^3 -
    0.147981 * u^2 + 0.221157 * u + m[n] * m2^-0.5

  a[n-1] <- -3.582633 * u^5 + 5.682633 * u^4 - 1.752461 * u^3 -
    0.293762 * u^2 + 0.042981 * u + m[n-1] * m2^-0.5
  a[1] <- -a[n]
  a[2] <- -a[n-1]

  ## calculate e
  e <- (m2 - 2 * m[n]^2 - 2 * m[n-1]^2) / (1 - 2 * a[n]^2 - 2 * a[n-1]^2)

  for(i in seq(3, n-2)){
    a[i] <- m[i] / sqrt(e)
  }

  # Calculate the W statsitc
  W <- (t(a) %*% sort(X))^2 / sum((X - mean(X))^2)


  # calculation of the P-value via normal distributed test statstic
  mu <- 0.0038915 * log(n)^3 - 0.083751 * log(n)^2 - 0.31082 * log(n) -
        1.5861
  sig <- exp(0.0030302 * log(n)^2 - 0.082676 * log(n) - 0.4803)

  z <- (log(1 - W) - mu) / sig

  pval <- 1 - pnorm(z)

  RVAL <- list(statistic = c(w = W),
               p.value = pval,
               method = 'Shapiro-Wilk normality test',
               data.name = DNAME)
  class(RVAL) <- 'htest'
  return(RVAL)
}

#' Jarque-Bera-Test
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
jarque.bera <- function(X){
  DNAME <- deparse(substitute(X))
  n <- length(X)
  x_bar <- mean(X)

  # Test statsiti
  S <- (sum((X - x_bar)^3)/n) / (sum((X - x_bar)^2)/n)^(3/2)
  K <- (sum((X - x_bar)^4)/n) / (sum((X - x_bar)^2)/n)^2

  JB <- n * S^2 / 6 + n * (K - 3)^2 / 24
  # Critical value
  jb <- qchisq(0.95, 2)

  # pvalue
  pval <- 1 - pchisq(JB, df = 2)
  df = 2
  # generation of an s3 object
  RVAL <- list(statistic = c(JB = JB),
               p.value = pval,
               parameter = c(df = 2),
               method = 'Jarque-Bera normality test',
               data.name = DNAME)
  class(RVAL) <- 'htest'
  return(RVAL)
}

#' Anderson-Darling-Test
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
anderson.darling.test <- function(X){
  DNAME <- deparse(substitute(X))
  X <- sort(X[complete.cases(X)])
  n <- length(X)
  x_bar <- mean(X)
  standev <- sd(X)

  par1 <- pnorm((X - x_bar) / standev, log.p = TRUE)
  par2 <- pnorm(-(X - x_bar) / standev, log.p = TRUE)

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
  # generation of an s3 object
  RVAL <- list(statistic = c(A = A),
               p.value = pval,
               method = "Anderson-Darling normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"

  return(RVAL)
}

#' Cramér-von-Mises-Test
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
cramerv_mises.test <- function(X){
  DNAME <- deparse(substitute(X))
  X <- sort(X[complete.cases(X)])
  n <- length(X)

  d <- (pnorm(X) - ((2 * seq(1, n) - 1) / (2*n)))^2
  t <- sum(d) + (1/(12 * n))

  pval <- 1 - pnorm(t) # here is an error

  # generation of an s3 object
  RVAL <- list(statistic = c(CVM = t),
               p.value = pval,
               method = "Cramér-von-Mises normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"

  return(RVAL)

}


Square.root <- function(n){
  bw <- round(sqrt(n))
}

