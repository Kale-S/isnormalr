#' @title
#' Variance Inflation Factor
#'
#' @param X Data Frame
#'
#' @return
#' Returns the observations that VIF is larger then 2
#' @export
VIF <- function(X){
  # Error hadeling

  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
    X <- data.frame(X)

  }else{
    return('No interccept: vifs may not be sensible.')

  }

  if(dim(X)[2] < 2){

    return('model contains fewer then 2 terms')
    stop()
  }else{


    #n <- dim(X)[1]
    #k <- dim(X)[2]
    #res <- rep(0, times = k)


    #for(i in 1:k){

      # Step 1: Fit linear model
    #  mod <- lm(X[ , i] ~ ., data=X[, -i])
    #  # Step 2: use the R2 to calculate the VIF
    #  r2 <- summary(mod)$r.squared
    #  res[i] <- 1/(1 - r2)
    #}
    Rx <- cor(X)
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
  # Calculate VIF
  Rx <- cor(X)
  vif <- diag(solve(Rx))
  vif_max <- max(vif)
  # backwards selection of explanatory variables, stops when all VIF
  # values are below 'thresh'
  while(vif_max >= thresh){
    # initialization
    vif_vals <- NULL
    var_names <- colnames(X)

    # calculate VIF
    Rx <- cor(X)
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
