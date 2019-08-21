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
  bins <- isnormalr:::Square.root(n)
  # safe the input as data.frame
  error <- data.frame(error)
  # safe the mean of the residuals
  error.mean <-  colMeans(error)
  # safe the standard diviation of the residals
  error.sd <- apply(error, 2, sd, na.rm=TRUE)

  # ------------- generate plot
  x.grid <- seq(-5, 5, length.out=10000)
  dens <- dnorm(x.grid, 0, 1)
  df <- with(error, data.frame(x=x.grid, y=dens))
  #bins <- isnormalr::sturge_rule(n)

  h <- ggplot2::ggplot(error, aes(x = error)) +
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
#  mu <- mean(error)
#  sd <- sd(error)

  # Compute n + 1 evenly spaced points on the interval (0, 1)
  prob <- ppoints(n)
  # Calculate the theroetical quantiles
  normal.quantiles <- qnorm(prob, mean = 0L, sd = 1L)

  # calculation of the abline lobe an intecept
  y <- quantile(error, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  intercept <- y[1L] - slope * x[1L]

  # build a data.frame
  df <- data.frame(normal.quantiles = sort(normal.quantiles),
                   sample.quantiles = sort(error))
  names <- names(sort(error))


## plot the results
  qq <- ggplot2::ggplot(df, aes(x=normal.quantiles,
                                y=sample.quantiles)) +
          geom_point() +
          ggrepel::geom_text_repel(aes(label=
                                  ifelse(sample.quantiles >= 2,
                                         names,'')),
                                   size=3, color = 'red') +
          geom_abline(intercept = intercept, slope = slope,
                      color="#666666", size=1.2)


  if(sum(df$sample.quantiles >= 2) >= 1){
         qq <- qq + ggplot2::geom_point(data = df[df$sample.quantiles >= 2, ],
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
#' Shaprio Test
#'
#' @description
#' einfügen
#'
#' @inheritParams resid_hist
#'
#' @return
#' @export
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

