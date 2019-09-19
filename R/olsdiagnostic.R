#' @title
#' olsdiagnosticR
#'
#' @description
#' Regression diagnostics for a linar regression model.
#'
#' @param object linear regression model from class 'lm'
#'
#' @param normtest
#' the type of normality test. Possible test are
#'
#' 'Shapiro-Wilk', 'Jarque-Bera' or 'Anderson-Darling'
#'
#' @param pval
#' p-value for the hypothesis test. Default is 0.05
#'
#' @details
#' olsdiagnostic was developed for the statistical consulting of the
#' University of Göttingen to assist students in checking common
#' assumptions of linear regression quickly and easily. It also
#' provides students with an automated recommendation for the further
#' procedure if an assumption is not fulfilled.
#'
#' @return
#' A olsdiagnosticR class with following entries:
#'
#'
#'  \strong{Normality_Tests:}
#'
#'  This could be the Shapiro-Wilk test, Jarque-Bera test,
#'  Anderson-Darling test or Cramer-von-mises test.
#'  Further information can be found in the documentation:
#'
#'  \code{\link{Shapiro_Wilk_test}}
#'
#'  \code{\link{jarque_bera}}
#'
#'  \code{\link{anderson_darling_test}}
#'
#'
#' \strong{Outlier:}
#'
#' Possible.Outlier:
#' a data.frame with possible outlier
#'
#' Cooks.Distance:
#' a vector with the cook's distance values
#'
#' Hat.Values
#' a vector with the lavarage values
#'
#' Studentized.Residuals
#' a vector with the studentized residuals
#'
#' Standardized.Residuals
#' a vector with the standardized residuals
#'
#' Further information can for these values be found in the
#' documentation:
#'
#' \code{\link{influence_observation}}
#'
#'
#' \strong{Multicollinearity:}
#'
#' VIF:
#' a vector of the variance inflation factors
#' Further information can be found in the
#' documentation:
#'
#' \code{\link{VIF}}
#'
#' Contain.Columns:
#' A string with the columns that would be avoid multicollinearity
#' in the data
#'
#'
#' \strong{Heteroskedasticity:}
#'
#' A Breusch-Pagan 'htest' object.
#'
#' Further information can be found in the
#' documentation:
#'
#' \code{\link{bp_test}}
#'
#'
#' \strong{Plots:}
#'
#' QQ_Plot: \code{\link{qq_plot}}
#'
#' Residual_Hist: \code{\link{resid_hist}}
#'
#' Box_Plot_X: \code{\link{box_plot_x}}
#'
#' Regressor_hist: \code{\link{hist_x}}
#'
#' Cooks_Distance_Plot: \code{\link{plot_cd}}
#'
#' Bubble_Plot: \code{\link{influence_plot}}
#'
#' Spread_Level_Plot: \code{\link{influence_plot}}
#'
#' @examples
#' \dontrun{
#' e_norm <- rnorm(n)
#' e_exp <- rexp(n)
#'
#' x1 = rnorm(n = n, mean = 80, sd = 10)
#' x2 = rnorm(n = n, mean = 70, sd = 5)
#' x3 = 2 * x1 + 4 * x2 + rnorm(n, mean = 20, sd = 5)
#'
#' y_n = 3 + x1 + x2 + e_norm
#' y_m_e = 4 + x1 + x2 + x3 + e_exp
#'
#' mod_n <- lm(y_n ~ x1 + x2)
#' mod_m_e <- lm(y_m_e ~ x1 + x2 + x3)
#'
#' olsdiagnosticR::olsdiagnostic(mod_n)
#' olsdiagnosticR::olsdiagnostic(mod_m_e, normtest = 'Jarque-Bera')
#' }
#'
#' @importFrom  methods new
#' @export
olsdiagnostic <- function(object, normtest = 'Shapiro-Wilk', pval = 0.05){
  #------------- initializing of variable
  X <- stats::model.matrix(object)  # with intercept
  y <- object$model[, 1]
  error <- stats::resid(object)
  y_hat <- stats::fitted(object)
  n <- dim(X)[1]
  k <- dim(X)[2]
  p <- k - 1
  #------------- validation of the input object
  ## check class and type if the object
  if(class(object) != 'lm' || typeof(object) != 'list'){
    stop('The input variable must be an "lm" object')
  }
  ## check the dimension of y and e
  if(ifelse(is.null(dim(y)[2]), FALSE,
     ifelse(dim(y)[2] != 1, TRUE, FALSE)) &
     ifelse(is.null(dim(error)[2]), FALSE,
     ifelse(dim(error)[2] != 1, TRUE, FALSE)) &
     ifelse(is.null(dim(y_hat)[2]), FALSE,
     ifelse(dim(y_hat)[2] != 1, TRUE, FALSE))){
    stop('The dimension of y, y_hat and e must be nx1.')
  }
  ## check the number of observations
  if(any(is.na(n)) || any(is.na(y)) || any(is.na(y_hat)) ||
     any(is.na(error)) || any(is.na(x))){
    stop('The lm object contains missing obeservation')
  }
  ## check if the normtest is Shapiro-Wilk
  if(normtest == 'Shapiro-Wilk'){
    if(n < 3L){
      stop('sample size must be larger then 3')  # nessercary for the
    }else if(n > 5000L){                           # Shapiro-Test
      stop('Shapiro-Wilk statistic might be inaccurate due to large sample size ( > 5000)')
    }
  }

  #------------- calculation of necessary values and functions
  ## calculation of necessary values
  inf_obs <- influence_observation(X = X, e = error)

  ## calculation of necessary functions
  ### normal assumption
  qq <- qq_plot(influence_obs = inf_obs)
  res_hist <- resid_hist(X = error)

  if(normtest == 'Shapiro-Wilk'){
    normT  <- Shapiro_Wilk_test(X = error)
  }else if(normtest == 'Jarque-Bera'){
    normT <- jarque_bera(X = error)
  }else if(normtest == 'Anderson-Darling'){
    normT <- anderson_darling_test(X = error)
  }else{
    stop('the specified normtest does not exist')
  }

  ### outlier
  bwp <- box_plot_x(X = X)
  x_hist <- hist_x(X = X)
  cd_plot <- plot_cd(influence_obs = inf_obs)
  inf_plot <- influence_plot(influence_obs = inf_obs)

  ### test for multicollinearity
  v <- VIF(X = X)
  if(any(v >= 10)){
    Cointain_col <- VIF_df(X = X)
  }else{
    Cointain_col <- NULL
  }
  ### test for heteroskedasticity
  bp <- bp_test(e = error, X = X)
  slp <- Spread_level_plot(fitted.value = y_hat,
                           influence_obs = inf_obs)


  #------------- recommendations
  ## outlier
  all_bool <- rep(FALSE, times = n)
  hatval <- order(inf_obs$leverage.value, decreasing = TRUE)[1:2]
  rstud <- order(abs(inf_obs$studentized.residuals),
                 decreasing = TRUE)[1:2]
  cook <-order(inf_obs$cooks.distance, decreasing = TRUE)[1:2]
  all <- union(rstud, union(hatval, cook))
  all_bool[all] <- TRUE
  obs <- data.frame('CookD' =
                      inf_obs$cooks.distance[all_bool],
                    'Leverage'=
                      inf_obs$leverage.value[all_bool],
                    'StudRes'=
                      inf_obs$studentized.residuals[all_bool],
                    'StandRes'=
                      inf_obs$standardized.residuals[all_bool]
  )


  #------------- save the result
  res <- list(
    Normality_Tests=normT,
    Outlier = list(
      Possible_Outlier = obs,
      Cooks_Distance = inf_obs$cooks.distance,
      Hat_Values = inf_obs$leverage.value,
      Studentized_Residuals = inf_obs$studentized.residuals,
      Standardized_Residuals = inf_obs$standardized.residuals
    ),
    Multicollinearity = list(
      VIF = v,
      Contain_Columns = Cointain_col
    ),
    Heteroskedasticity = bp,
    Plots = list(
      # Normal assumptions
      QQ_Plot = qq,
      Residual_Hist = res_hist,
      # Outlier
      Box_Plot_X = bwp,
      Regressor_Hist = x_hist,
      Cooks_Distance_Plot = cd_plot,
      Bubble_Plot = inf_plot,
      # Homoskedasticity
      Spread_Level_Plot = slp
    ),
    Inputs = list(
      normtest = normtest,
      pval = pval
    )
  )

  #------------- define the class olsdiagnosticR
  methods::new('olsdiagnosticR', res)
 }
