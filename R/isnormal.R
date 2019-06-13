#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @import
#' stringr
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

  qq <- qq_plot(error)
  res_hist <- resid_hist(error)
  swt <- shaprio_test(error)
  swt.s <- paste('Statistik: ',toString(swt[1]),
                 'P-Value: ', toString(swt[2]))
  bwp <- box_plot_x(X)
  x_hist <- hist_x(X)
  v <- VIF(object)

  return(list(qq,
         res_hist,
         bwp,
         x_hist,
         cat('--------- Shapiro Wilke Test-----\n',swt.s),
         cooks.dist(object), # the plot is plotted form inside the function (plot not saved in the variable
         v))


}
