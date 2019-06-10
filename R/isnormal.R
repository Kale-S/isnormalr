#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
is.normal <- function(object){
  #------------- inizelizing of variable
  X <- object$model[2]
  y <- object$model[1]
  error <- resid(object)
  y_hat <- fitted(object)

  qq <- qq_plot(error)
  res_hist <- resid_hist(error)
  swt <- shaprio_test(error)




  return(list(qq,
         res_hist,
         paste('----Shapiro Wilke Test----',
         '\nStatistik: ', swt[1],
          '\nP-Value: ', swt[2])))


}
