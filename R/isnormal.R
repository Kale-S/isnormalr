#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @examples
is.normal <- function(object){
  # test if the input variable is a list
  if(typeof(object) != 'list'){
    print('Some error later')
  }

  #------------- inizelizing of variable
  X <- model.matrix(object)  # with intercept
  y <- object$model[, 1]
  error <- resid(object)
  y_hat <- fitted(object)
  n <- dim(X)[1]
  k <- dim(X)[2]
  p <- k - 1

  if(is.na(n) || n < 3L){
    stop('sample size must be larger then 3')  # nessercary for the
  }else if(n > 5000){                           # Shapiro-Test
    message('Shapiro-Wilk statistic might be inaccurate due to large sample size ( > 5000)')
  }

  inf.obs <- influence.observation(object)
# test for normality
  qq <- qq_plot(inf.obs$standardized.residuals,
                inf.obs$cooks.distance,
                inf.obs$studentized.residuals,
                inf.obs$leverage.value)
  res_hist <- resid_hist(error)
  swt <- Shapiro_Wilk.test(error)
  jb <- jarque.bera(error)
  ad <- anderson.darling.test(error)
  cm <- cramerv_mises.test(error)

# test for outlier
  bwp <- box_plot_x(X)
  x_hist <- hist_x(X)


  cd.plot <- isnormalr:::plot.cd(inf.obs$cooks.distance,
                                 inf.obs$studentized.residuals,
                                 inf.obs$leverage.value)
  inf.plot <- influence.plot(inf.obs$studentized.residuals,
                             inf.obs$leverage.value,
                             inf.obs$cooks.distance)


# test for multikolonarity
  v <- VIF(X)
  if(any(v >= 10)){
    Cointain.col <- isnormalr:::VIF_df(X)
  }else{
      Cointain.col <- NULL
  }
# test for homooskedasticity
  bp <- bp_test(error, X)
  slp <- Spread.level.plot(y_hat,
                           inf.obs$studentized.residuals,
                           inf.obs$cooks.distance,
                           inf.obs$leverage.value)


  ######################### Recomendations ###########################
# Information for Tips
  # Outlier
  all.bool <- rep(FALSE, times = n)
  m2 <- 2 * mean(inf.obs$leverage.value) # kritcal value
  hatval <- which(inf.obs$leverage.value >=  m2)
  rstud <- order(abs(inf.obs$studentized.residuals),
                 decreasing = TRUE)[1:2]
  cook <- which(inf.obs$cooks.distance > 1)
  all <- union(rstud, union(hatval, cook))
  all.bool[all] <- TRUE
  obs <- data.frame('CookD' =
                         inf.obs$cooks.distance[all.bool],
                    'Leverage'=
                         inf.obs$leverage.value[all.bool],
                    'StudRes'=
                         inf.obs$studentized.residuals[all.bool],
                    'StandRes'=
                         inf.obs$standardized.residuals[all.bool]
  )



  res <- list(
    Normality_Tests=list(
      Shapiro.Wilk = swt,
      Jarque.Bera = jb,
      Anderson.Darling = ad,
      cramer.v.mises = cm
      ),
    Outlier = list(
      Possible.Outlier = obs,
      Cooks.Distance = inf.obs$cooks.distance,
      Hat.Values = inf.obs$leverage.value,
      Studentized.Residuals = inf.obs$studentized.residuals,
      Standardized.Residuals = inf.obs$standardized.residuals
    ),
    Multikolonarity = list(
      VIF = v,
      Contain.Columns = Cointain.col
    ),
    Homoskedasticity = list(
      Breusch.Pagan = bp
    ),
    Plots = list(
      # Normal assumptions
      QQ.Plot = qq,
      Residual.Hist = res_hist,
      # Outlier
      Box.Plot.X = bwp,
      Regressor.Hist = x_hist,
      Cooks.Distance.plot = cd.plot,
      Bubble.Plot = inf.plot,
      # Homoskedasticity
      Spread.Level.Plot = slp
    )
  )
  new('isnormalr', res)
}
