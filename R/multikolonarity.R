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


    n <- dim(X)[1]
    k <- dim(X)[2]
    res <- rep(0, times = k)


    for(i in 1:k){

      # Step 1: Fit linear model
      mod <- lm(X[ , i] ~ ., data=X[, -i])
      # Step 2: use the R2 to calculate the VIF
      r2 <- summary(mod)$r.squared
      res[i] <- 1/(1 - r2)
    }

    return(res)
  }


}
