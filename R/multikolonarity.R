#' @title
#' Variance Inflation Factor
#'
#' @param lm lm object
#'
#' @return
#' Returns the observations that VIF is larger then 2
#' @export
#' @import
#' stringr
VIF <- function(X){

  n <- dim(X)[1]
  k <- dim(X)[2]
  res <- rep(0, times = k)


  for(i in 1:k){
    # Step 1
    mod <- lm(X[, i] ~ X[, -i])
    # Step 2
    r2 <- summary(mod)$r.squared
    res[i] <- 1/(1 - r2)
  }

  return(res)




  #v <- vif(lm)
  #multi <- as.numeric(v > 2)

  #if(sum(multi) > 0){
  #  num <- which(v>2, TRUE)
  #  res <- paste('For the variables', stringr::str_c(names(v[num]), collapse = ', '), 'is the VIF larger then 2',
  #              'this could be an indicator for Heteroskedastiziti in',
  #              'the Data')
  #}else{
  #  res <- paste('No VIF is larger then 2. This could be an indicator for',
  #             'no-multikolonarity')
  #}
  #return(res)

}
