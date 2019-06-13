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
VIF <- function(lm){
  v <- vif(lm)
  multi <- as.numeric(v > 2)

  if(sum(multi) > 0){
    num <- which(v>2, TRUE)
    res <- paste('For the variables', stringr::str_c(names(v[num]), collapse = ', '), 'is the VIF larger then 2',
                'this could be an indicator for Heteroskedastiziti in',
                'the Data')
  }else{
    res <- paste('No VIF is larger then 2. This could be an indicator for',
               'homoskedastizity')
  }
  return(res)

}
