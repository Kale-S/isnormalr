#' @title
#' Variance Inflation Factor
#'
#' @description
#' Calculation of the Variance Inflation Factor for each column in
#' the design matrix.
#'
#' @param X Data.Frame
#'
#' @details
#' In order to discover multicollinearities between the independent
#' variables of a model, the Variance Inflation Factor (VIF) serves
#' as a tool. Variance Inflation Factors: The variance inflation
#' factor (VIF) is the quotient of the variance in a model with
#' several terms and the variance of a model with only one term.
#' It quantifies the severity of multicollinearity in an ordinary
#' regression analysis with smallest squares. It provides an index
#' that measures how much the variance (the square of the standard
#' deviation of the estimate) of an estimated regression coefficient
#' is increased due to collinearity. The basic idea is to try to
#' express a particular variable xk through a linear model of all
#' other independent variables. If this succeeds well (i.e. if the
#' coefficient of determination is high), one can assume that the
#' tested variable xk is (multi)collinear to one or more variables.
#' In general, you calculate the VIF for all independent variables
#' and then try to remove the variables with the highest values from
#' the model. As a rule of thumb, in a linear model the VIF values of
#' the independent variables should be less than 10 to avoid problems
#' with the interpretability of the coefficients. Mathematically,
#' the VIF measures the increase in variance compared to an
#' orthogonal base (Lohninger 2012).
#'
#' @return
#' Returns a vector with the Variance Inflation Factors.
#'
#' @references
#' Lohninger, H. 2012. Grundlagen der Statistik
#'
#' @examples
#' \dontrun{
#' X <- data.frame(matrix(rnorm(200), ncol = 5))
#' olsdiagnosticR:::VIF(X = X)
#' }
VIF <- function(X){

  # deleting the intercept
  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
    X <- data.frame(X)

  }else{
    # error message if there is no intercept in the design matrix
    return('No interccept: vifs may not be sensible.')

  }
  # return error message if we have less then two parameters
  if(dim(X)[2] < 2){
    return('model contains fewer then 2 terms')
  }else{


    # calculation of the correlation matrix
    Rx <- stats::cor(X)
    # calculation of the diagonal elements of the inverse
    # of the correlation matrix
    res <- diag(solve(Rx))
    return(res)
  }


}


VIF_df <- function(X, thresh = 10){
  ###################################################################
  #
  # Backword selection to get a vector of names from a Data Frame
  # which only contains columns where the VIF >= 10
  #
  #
  ###################################################################
  # delete the intercept if necessary
  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
    X <- data.frame(X)

  }
  # calculate VIF
  Rx <- stats::cor(X)
  vif <- diag(solve(Rx))
  vif_max <- max(vif)
  # backward selection of explanatory variables stops when all VIF
  # values are below 'thresh'
  while(vif_max >= thresh){
    # initialization
    vif_vals <- NULL
    var_names <- colnames(X)

    # calculate VIF
    Rx <- stats::cor(X)
    vif <- diag(solve(Rx))

    # save the highest VIF
    vif_max <- max(vif)
    # if all vif < thresh stop the loop
    if(vif_max < thresh){break}

    # find the column in the df with the highest VIF
    max_col <- which(vif == vif_max)
    # delete the column with the highest VIF
    X <- X[, -max_col]

  }
  # return the column names of the DF with only VIF > 10
  return(var_names)




}
