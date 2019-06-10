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

  qq <- qq_plot(error)
  res_hist <- resid_hist(error)
  swt <- shaprio_test(error)
  bwp <- box_plot_x(X)
  x_hist <- hist_x(X)


  return(list(qq,
         res_hist,
         bwp,
         x_hist,
         paste('----Shapiro Wilke Test----',
         '\nStatistik: ', swt[1],
          '\nP-Value: ', swt[2])))


}
