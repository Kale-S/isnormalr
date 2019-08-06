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

resid_hist <- function(resid){
  # safe the input as data.frame
  error <- data.frame(resid)
  # safe the mean of the residuals
  error.mean <-  colMeans(error)
  # safe the standard diviation of the residals
  error.sd <- apply(error, 2, sd, na.rm=TRUE)

  # ------------- generate plot
  x <- seq(min(error), max(error), length.out=10000)
  df <- with(error, data.frame(x=x, y=dnorm(x, error.mean, error.sd)))
  h <- ggplot2::ggplot(error) +
    ggplot2::geom_histogram(aes(x = resid, y=..density..))
  h <- h + ggplot2::geom_line(data=df, aes(x=x, y=y), color='red')

  return(h)

}


#' @title
#' QQ-Plot of the residuals
#'
#' @inheritParams resid_hist
#'
#' @return
#' This function returns a QQ-plot
#'
#' @export
qq_plot <- function(resid){
  # safe the input as error
  error <- data.frame(resid)

  # ------------- generate plot
  qq <- ggplot2::qplot(sample = resid, data = error)

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
shapirio_test <- function(x){
  n <- length(x)
  x <- sort(x)  # sorting the data in ascending order
  hf <- function(i, n){
    res <-qnorm((i - (3/8)) / (n + 1/4))
  }
  m <- sapply(1:n, FUN=hf, n = n)
  w <- rep(0, n)  # preallocating weights

  b <- 1/sqrt(t(m) %*% m) %*% m
  u <- 1/sqrt(n)

  p1 <- c(-2.706056, 4.434685, -2.071190, -0.147981, 0.221157, b[n])
  p2 <- c(-3.582633, 5.682633, -1.752461, -0.293762, 0.042981, b[n-1])

  w[n] <- t(p1) %*% as.vector(c(rev(poly(u, degree=length(p1) - 1,
                                         raw=TRUE)[1, ]), 1))
  w[1] <- -w[n]

  if(n == 3){
    w[1] <- 0.707106781
    w[n] <- -w[1]
  }
  if(n >= 6){
    w[n - 1] <- t(p2) %*% as.vector(c(rev(poly(u, degree=length(p2) - 1,
                                                 raw=TRUE)[1, ]), 1))

    w[2] <- -w[n - 1]

    ct <- 3
    phi <- (t(m) %*% m - (2 * m[n]^2) - (2 * m[n-1]^2)) /
            (1-(2*w[n]^2) - (2*w[n-1]^2))
  }else{
    ct = 2
    phi <- (t(m) %*% m - 2*m[n]^2) / (1-2*w[n]^2)
  }
  if(n==3){
    phi <- 1
  }
  w[seq(ct, n-ct+1)] <- m[seq(ct, n-ct+1)]/ rep(sqrt(phi), length(n - ct - 1))
  W <- (t(w) %*% w)^2 / (t((x-mean(x))) %*% (x-mean(x)))

  return(W)

}

