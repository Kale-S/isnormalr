#' @title
#' Histogramm of the residuals
#'
#' @description
#' This function genertates a Histogramm of the residuals with a normal Density
#'
#'
#' @param df Dataframe
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

resid_hist <- function(error){
  # safe the number of obs. for the calculation of the bins
  n <- length(error)
  # safe the input as data.frame
  error <- data.frame(error)
  # safe the mean of the residuals
  error.mean <-  colMeans(error)
  # safe the standard diviation of the residals
  error.sd <- apply(error, 2, sd, na.rm=TRUE)

  # ------------- generate plot
  x.grid <- seq(min(error), max(error), length.out=10000)
  dens <- dnorm(x.grid, error.mean, error.sd)
  df <- with(error, data.frame(x=x.grid, y=dens))
  bins <- isnormalr::sturge_rule(n)
  h <- ggplot2::ggplot(error) +
    ggplot2::geom_histogram(aes(x = error, y=..density..,
                                col=I('black')),
                            alpha=0.6, bins=bins)
  h <- h + ggplot2::geom_line(data=df, aes(x=x.grid, y=y), color='red')

  return(h)

}


#' @title
#' QQ-Plot of the residuals
#'
#' @inheritParams resid_hist
#'
#' @return
#' This function returns a QQ-plot
#' @import
#' ggplot2
#'
#' @export
qq_plot <- function(error){
  n <- length(error)
  mu <- mean(error)
  sd <- sd(error)

  # Compute n evenly spaced points on the interval (0, 1)
  prob <- (1:n) / (n + 1)
  # Calculate the theroetical quantiles
  normal.quantiles <- qnorm(prob, mu, sd)

  df <- data.frame(normal.quantiles = sort(normal.quantiles),
                   sample.quantiles = sort(error))
  # plot the results
  qq <- ggplot2::ggplot(df, aes(x=normal.quantiles, y=sample.quantiles)) +
          geom_point() +
          geom_abline(slope=1, color='magenta', size=1.2)

  return(qq)
}

#' @title
#' Shaprio Test
#'
#' @description
#' einfügen
#'
#' @inheritParams resid_hist
#'
#' @return
#' @export
Shapiro_Wilk.test <- function(x){
  DNAME <- deparse(substitute(x))
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

  RVAL <- list(statistic = c(w = W),
               method = 'Shapiro-Wilk normality test',
               data.name = DNAME)
  class(RVAL) <- 'htest'
  return(RVAL)
}

#' Struge Rule
#' @export
sturge_rule <- function(n){
  # Defines the bin sizes for the diagramms
  #K <- 2 * IQR(x[, 1]) / (dim(x)[1]^(1/3))
  K <- 1 + 3.322 * log10(n)
  K <- round(K)

}
