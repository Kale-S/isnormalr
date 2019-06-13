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
shaprio_test <- function(resid){

  st <- shapiro.test(resid)

  statistic <- st$statistic
  p.value <- st$p.value

  return(c(statistic, p.value))
}


