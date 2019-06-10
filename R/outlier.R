#' @title
#' Box-Whisker-Plot
#'
#' @description
#' Creat a Box-Whisker-Plot for each variable in X
#'
#' @param X design matrix
#'
#' @return
#' @import
#' ggplot2
#'
#' @export
box_plot_x <- function(X){
  n <- dim(X)[1]
  k <- dim(X)[2]


  #----------- generate plot
  bWp <- ggplot2::ggplot(stack(X), aes(X=ind, y = values)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ind, scales="free") +
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_blank())

  return(bWp)

}

#' @title
#' Histogram for each variable in design matrix
#'
#' @inheritParams box_plot_x
#'
#' @return
#' Returns a histogramm for each variablen in the design matrix
#'
#' @export
#'
#' @import
#' ggplot2
#' tidyr
#'
#' @examples
hist_x <- function(X){

  # Used to hold only numeric columns
  nums <- unlist(lapply(X, is.numeric))

  h <- X[, nums] %>%
    # Convert to key-value pairs
    gather() %>%
    # Plot the values
    ggplot2::ggplot(aes(value)) +
    # In separate panals
    ggplot2::facet_wrap(~ key, scales='free') +
    # As histogram
    ggplot2::geom_histogram()

  return(h)
}
