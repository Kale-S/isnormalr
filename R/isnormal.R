#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
is.normal <- function(object){

  # test if the input variable is a list
  if(typeof(object) != 'list'){
    print('Some error later')
  }
  #------------- inizelizing of variable
  X <- object$model[, -1]
  y <- object$model[, 1]
  error <- resid(object)
  y_hat <- fitted(object)
  n <- length(error)

  if(is.na(n) || n < 3L){
    stop('sample size must be larger then 3')  # nessercary for the
  }else if(n > 5000){                           # Shapiro-Test
    message('Shapiro-Wilk statistic might be inaccurate due to large sample size ( > 5000)')
  }

  qq <- qq_plot(error)
  res_hist <- resid_hist(error)
  #swt <- shapirio_test(error)
  #swt.s <- paste('Statistik: ',toString(swt[1]),
  #               'P-Value: ', toString(swt[2]))
  bwp <- box_plot_x(X)
  x_hist <- hist_x(X)
  v <- VIF(X)

  bp <- bp_test(error, X)
  slp <- Spread.level.plot(object)


  return(list(qq,
         res_hist,
         bwp,
         x_hist,
         #cat('--------- Shapiro Wilke Test-----\n',swt.s),
         cooks.dist(object), # the plot is plotted form inside the function (plot not saved in the variable
         v,
         bp))


}
