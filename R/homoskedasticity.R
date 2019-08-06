#' @title
#' Breusch-Pagan Test
#'
#' @param model
#'
#' @return kritical value
#' @export
#' @import
#' lmtest
#' @examples
bp_test <- function(model){
  res <- lmtest::bptest(model)
  return(res$statistic)
}

#' Spread-Level Plot
#'
#' @param model
#'
#' @return
#' @export
#' @import
#' car
#' @examples
Spread.level.plot <- function(model){
  res <- car::spreadLevelPlot(model)

}
