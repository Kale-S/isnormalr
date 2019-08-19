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
  X <- object$model[, -1]  # without intercept
  y <- object$model[, 1]
  error <- resid(object)
  y_hat <- fitted(object)
  n <- dim(X)[1]
  p <- dim(X)[2]


  if(is.na(n) || n < 3L){
    stop('sample size must be larger then 3')  # nessercary for the
  }else if(n > 5000){                           # Shapiro-Test
    message('Shapiro-Wilk statistic might be inaccurate due to large sample size ( > 5000)')
  }
# test for normality
  qq <- qq_plot(error)
  res_hist <- resid_hist(error)
  swt <- Shapiro_Wilk.test(error)
  #swt.s <- paste('Statistik: ',toString(swt[1]),
  #               'P-Value: ', toString(swt[2]))
# test for outlier
  bwp <- box_plot_x(X)
  x_hist <- hist_x(X)
  inf.obs <- influence.observation(object)

  cd.plot <- isnormalr:::plot.cd(inf.obs$cooks.distance, p)
  inf.plot <- influence.plot(inf.obs$standardized.residuals,
                             inf.obs$leverage.value,
                             inf.obs$cooks.distance)


# test for multikolonarity
  v <- VIF(X)
# test for homooskedasticity
  bp <- bp_test(error, X)
  slp <- Spread.level.plot(y_hat, inf.obs$studentized.residuals)


  res <- list(
    Normality_Tests=list(
      Shapiro.Wilk = swt
      ),
    Outlier=list(
      Cooks.Distance = inf.obs$cooks.distance,
      Hat.Values = inf.obs$leverage.value,
      Studentized.Residuals = inf.obs$studentized.residuals,
      Standardized.Residuals = inf.obs$standardized.residuals
    ),
    Multikolonarity = list(
      VIF = v
    ),
    Homoskedasticity = list(
      Breusch.Pagan = bp
    ),
    Plots = list(
      QQ.Plot = qq,
      Residual.Hist = res_hist,
      Regressor.Hist = x_hist,
      Box.Plot = res_hist,
      Cooks.Distance.plot = cd.plot,
      Bubble.Plot = inf.plot,
      Spread.Level.Plot = slp
    )
  )
  new('isnormalr', res)
}
