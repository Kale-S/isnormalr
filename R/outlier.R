#' @title
#' Box-Whisker-Plot
#'
#' @description
#' A Box-Whisker-Plot for each numeric column in the design matrix
#'
#' @inheritParams VIF
#'
#' @details
#' The box plot is a diagram that is used to graphically represent
#' the distribution of an at least ordinal scaled characteristic.
#' It combines different robust scattering and position measures in
#' one representation.  A box plot is intended to give a quick
#' impression of the area in which the data is located and how it is
#' distributed over this area. Therefore, all values of the so-called
#' five-point summary, i.e. the median, the two quartiles and the two
#' extreme values, are displayed. A box whisker plot, also called a
#' box plot, is a summary of a data set in five points. These five
#' points are the minimum, the lower quartile, the median, the upper
#' quartile and the maximum.
#'
#' @return
#' qqplot-object
#'
#'
#' @references
#' Franz Kronthaler: Statistik angewandt.
#' Datenanalyse ist (k)eine Kunst.
#'
#' Springer-Verlag, Berlin Heidelberg 2014, ISBN 978-3-642-53739-4,
#' S. 38.
#'
#' @examples
#' \dontrun{
#' Z <- matrix(rnorm(100), ncol = 10)
#' olsdiagnosticR:::box_plot_x(Z)
#'
#' Y <- matrix(rexp(100), ncol = 10)
#' olsdiagnosticR:::box_plot_x(Y)
#' }
#'
#' @import
#' ggplot2
box_plot_x <- function(X){

  # deleting the intercept
  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]

  }
  # create a DataFrame
  X <- data.frame(X)
  # save save the number of observation
  n <- dim(X)[1]
  # save the number of parameters
  k <- dim(X)[2]

  # find factor columns
  isfactor <- sapply(X, is.factor)
  # find numeric columns
  isnumeric <- sapply(X, is.numeric)
  # filter for factor columns or numeric columns
  filter <- isnumeric | !isfactor

  # apply the filter on X
  X <- X[, filter]

  #----------- generate plot
  bWp <- ggplot2::ggplot(utils::stack(X), aes(X=ind, y = values)) +
    ggplot2::geom_boxplot(fill='#666666', alpha=0.6, color='black',
                          outlier.colour='black') +
    ggplot2::facet_wrap(~ind, scales='free')

  # add xlab, ylab and title
  title <- ifelse(k > 1,
                  'Boxplot`s of the independend variable',
                  'Boxplot of the independend variable')
  bWp <- bWp + ggtitle(title) + xlab('X')


  # add the theme
  bWp <- bWp + theme_olsdiagnosticR() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank())

  return(bWp)
}

#' @title
#' Histograms
#'
#' @description
#' A histogram for each numeric column in the design matrix
#'
#' @inheritParams box_plot_x
#'
#' @details
#' A histogram is a graphical representation of the frequency
#' distribution of cardinally scaled characteristics. It requires the
#' division of the data into classes (bins), which can have a
#' constant or variable width. Directly adjacent rectangles of the
#' width of the respective class are drawn, whose areas represent the
#' (relative or absolute) class frequencies (Rönz 1994, S. 147;
#' Wassermann 2005, S. 127; Arens et al. 2008, S. 1226). The height
#' of each rectangle then represents the (relative or absolute)
#' frequency density, i.e. the (relative or absolute) frequency
#' divided by the width of the corresponding class (Freedman et al.
#' 1998).
#'
#' Further information about histograms can be found in the
#' documentation \code{\link{hist}}
#'
#' @return
#' qqplot-object
#'
#' @references
#' Bernd Rönz, Hans G. Strohe: Lexikon Statistik. Gabler Verlag,
#' 1994, S. 157
#'
#' Larry Wasserman: All of Nonparametric Statistics. Springer,
#' 2005, S. 127
#'
#' Arens et al.: Mathematik. Spektrum Akademischer Verlag,
#' 2008, S. 1226
#'
#' D. Freedman, R. Pisani, R. Purves: Statistics. Third edition.
#' W.W.Norton, 1998.
#'
#' @examples
#' \dontrun{
#' set.seed(1678)
#' X <- data.frame(matrix(rnorm(1000), ncol = 9))
#' olsdiagnostic:::hist_x(X = X)
#' }
#'
#' @import
#' ggplot2
hist_x <- function(X){

    # deleting the intercept
  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
  }
  # create a DataFrame
  X <- data.frame(X)
  # save number of observation
  n <- dim(X)[1]
  # save number of coefficients
  k <- dim(X)[2]
  # define the number of bins for the histograms
  bins <- Square_root(n)
  # filter only numeric values
  filter <- apply(X, 2, is.numeric)
  # apply the filter
  X <- X[, filter]

  # ------------- generate plot
  h <- ggplot2::ggplot(utils::stack(X), aes(values)) +
            ggplot2::facet_wrap(~ind, scales='free') +
            ggplot2::geom_histogram(col = 'black', alpha=0.6,
                                  bins = bins)



  #h <-  ggplot2::ggplot(X, aes(value)) +
  #    # In separate panals
  #    ggplot2::facet_wrap(~ key, scales='free') +
  #    # As histogram
  #    ggplot2::geom_histogram(col=('black'), alpha=0.6,
  #                            bins=bins)

  # add title
  title <- ifelse(k > 1,
                  'Histogram`s of the independend variable',
                  'Histogram of the independend variable')
  h <- h + ggtitle(title)

  # add theme and delete x title
  h <- h + theme_olsdiagnosticR() +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank())
  return(h)
}

#' @title
#' Influence Observation
#'
#' @inheritParams VIF
#'
#' @param e
#' a single numeric vector of data values.
#'
#' @description
#' This function is a suite of functions that can be used to compute
#' some of the regression (leve-one-out deletion) diagnostics for a
#' linear models discussed in Belsley, Kuh and Welsch (1980),
#' Cook and Weisberg (1982)
#'
#' @details
#' \strong{Hat_Values / Lavarage Values}
#'   are a measure of the effect of a particular observation
#'   on the regression predictions due to the position of that
#'   observation in the space of the inputs. In general, the farther
#'   a point is from the center of the input space, the more leverage
#'   it has. Because the sum of the leverage values is p, an
#'   observation i can be considered as an outlier if its leverage
#'   substantially exceeds the mean leverage value, p/n, for example,
#'   a value larger than 2*p/n.
#'
#'
#' \strong{Cooks distance}
#'   is a measure of an observation or instances’
#'   influence on a linear regression. Instances with a large influence
#'   may be outliers and datasets that have a large number of highly
#'   influential points might not be good predictors to fit linear models.
#'
#' The \strong{Standardized residuals}
#'   is the residual divided by its standard
#'   error. Standardization is a method of transforming data so that
#'   its mean value is zero and the standard deviation is one. If the
#'   distribution of residuals is approximately normal, then 95% of the
#'   standardized residuals should lie between -2 and +2, if many of the
#'   residuals lie outside + or - 2, then they might be considered
#'   unusual. However, about 5% of the residuals could happen to be
#'   outside this region.
#'
#' The \strong{Studentized residuals}
#'   take into account that the variance of
#'   the predicted value used in calculating residuals is not constant.
#'   The variability of cases close to the sample mean for an
#'   independent variable have smaller variance compared to cases
#'   further away from the mean. The studentized residual takes this
#'   change in variability into account by dividing the observed
#'   residual by an estimate of the standard deviation of the residual
#'   at that point. Norusis argues that this adjustment makes violation
#'   of regression assumptions more visible, so it is preferred to
#'   standardized residuals.
#'
#' @return
#'  a list with following values:
#'
#'  - Hat_Values / Lavarage Values (leverage.value)
#'
#'  - Cooks distance (cooks.distance)
#'
#'  - Standardized residuals(standardized.residuals)
#'
#'  - Studentized residuals (studentized.residuals)
#'
#'
#' @references
#' Cook, R. D. and Weisberg, S. (1984) Residuals and Influence in
#' Regression.
#'
#' Wiley. Fox, J. (1997) Applied Regression, Linear Models, and
#' Related Methods.
#'
#' Sage. Williams, D. A. (1987) Generalized linear model diagnostics
#' using the deviance and single case deletions. Applied Statistics
#' 36, 181--191.
#'
#' Stevenson, Wiliam B. (2008) Analyzing Residuals.
#'
#' @examples
#' \dontrun{
#' X <- data.frame(matrix(rnorm(1000), nrow = 100))
#' resid <- rnorm(100)
#' olsdiagnosticR:::influence_observation(X = X, e = error)
#' }
influence_observation <- function(X, e){

  # save the number of observations
  n <- dim(X)[1]
  # save the number of coefficients
  p <- dim(X)[2]
  # calculation of the variance
  s2 <- sum(e^2) / (n - p)

  # calculate the projection matrix
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  # extract the leverage Values
  h <- diag(H)
  # calculate the Cooks distance
  D <- (e^2 / (s2 * p)) * (h / (1 - h)^2)

  #  calculate the standardized residuals
  r <- e / (sqrt(s2) * sqrt(1-h))

  # calculate the studentized residuals
  t <- r * sqrt((n - (p - 1) - 2) /
                  (n - (p - 1) - 1 - r^2))


  # save the results in a list
  influence_obs <- list(leverage.value = h,
                        cooks.distance = D,
                        standardized.residuals = r,
                        studentized.residuals = t)

  return(influence_obs)
}

#' @title
#' Visualisation of the cook's distances
#'
#' @description
#' A barplot for cook's distances values
#'
#' @inheritParams qq_plot
#'
#' @details
#' Further information about cook's distances can be found in the
#' documentation \code{\link{influence_observation}}
#'
#'
#' @return
#' qqplot-object
#'
#'
#' @references
#' Cook, R. D. and Weisberg, S. (1984) Residuals and Influence in
#' Regression.
#'
#' Wiley. Fox, J. (1997) Applied Regression, Linear Models, and
#' Related Methods.
#'
#' Sage. Williams, D. A. (1987) Generalized linear model diagnostics
#' using the deviance and single case deletions. Applied Statistics
#' 36, 181--191.
#'
#' @examples
#' \dontrun{
#' X <- data.frame(matrix(rnorm(1000), nrow = 100))
#' resid <- rnorm(100)
#' inf_obs <- olsdiagnosticR:::influence_observation(X = X, e = error)
#' olsdiagnosticR:::plot_cd(influence_obs = inf_obs)
#' }
#'
#' @import
#' ggplot2
#' ggrepel
plot_cd <- function(influence_obs){
  # save the number of observations
  n <- length(influence_obs$standardized.residuals)

  # save the input variable in a DataFrame
  df <- data.frame('Hat.Values'=influence_obs$leverage.value,
                   'Studentized.Residuals'=
                     influence_obs$studentized.residuals,
                   'Cooks.Distance'=influence_obs$cooks.distance)

  # save the names of the observations
  names <- rownames(df)
  # initialization of a vector for the influential obs.
  all.bool <- rep(FALSE, times = n)
  # find the possible influential for Hat Values
  hatval <- order(df$Hat.Values, decreasing = TRUE)[1:2]
  # find the possible influential obs for rstud
  rstud <- order(abs(df$Studentized.Residuals), decreasing = TRUE)[1:2]
  # find the possible influential obs. for CookD
  cook <-order(df$Cooks.Distance, decreasing = TRUE)[1:2]
  # keep only all influential obs. one time
  all <- union(rstud, union(hatval, cook))
  # define the influential obs.
  all.bool[all] <- TRUE

  # create the plot
  cd.plot <- ggplot(df, aes(x = names, y = Cooks.Distance,
                            fill = all.bool)) +
    geom_bar(stat='identity') +
    scale_fill_manual(values = c('#757575', 'red'))

  # add thresh line
  cd.plot <- cd.plot +
    geom_abline(slope=0, intercept = 1,
                col = 'red', lty=2)
  # add possible influential observations
  cd.plot <- cd.plot +
    ggrepel::geom_text_repel(aes(label=
                             ifelse(all.bool,
                                    names, '')), size = 3,
                             color = 'red')

  # add xlab, ylab and title
  cd.plot <- cd.plot  +
    scale_x_discrete(name='Observation') +
    scale_y_continuous(name='Cooks Distance') +
    ggplot2::ggtitle('Cook`s Distance plot')
  # add the theme
  cd.plot <- cd.plot + theme_olsdiagnosticR()
  cd.plot <- cd.plot +
    theme(axis.text.x = element_blank(),#element_text(angle = 90, hjust = 1, vjust=0.3, size=10),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = NA),
          panel.grid.minor = element_line(colour = '#cccccc'),
          legend.position = 'none')
  # add footnote
  cd.plot <- cd.plot + labs(caption = 'possible outlier')

  return(cd.plot)
}


#' @title
#' Leverage values vs. Studentized residuals
#'
#' @description
#' Bubble-Plot of the Leverage values vs. Studentized residuals with the
#' scale of the bubble by the cook's distance
#'
#' @inheritParams qq_plot
#'
#' @details
#' Further information about Studentized residuals, Leverage values or
#' cook's distances,  can be found in the documentation
#' \code{\link{influence_observation}}
#'
#' @return
#' ggplot-object
#'
#' @examples
#' \dontrun{
#' set.seed(167)
#' X <- data.frame(matrix(rnorm(1000), nrow = 100))
#' resid <- rnorm(100)
#' inf_obs <- olsdiagnosticR:::influence_observation(X = X, e = error)
#' olsdiagnosticR:::influence_plot(influence_obs = inf_obs)
#' }
#'
#' @import
#' ggplot2
#' ggrepel
influence_plot <- function(influence_obs){

  # save the number of observations
  n <- length(influence_obs$standardized.residuals)

  # 2 * mean of the hat value
  m2 <- 2 * mean(influence_obs$leverage.value)
  # 3 * mean of the hat value
  m3 <- 3 * mean(influence_obs$leverage.value)
  # save the input variable in a DataFrame
  df <- data.frame('Hat.Values'=influence_obs$leverage.value,
                   'Studentized.Residuals'=
                     influence_obs$standardized.residuals,
                   'Cooks.Distance'=influence_obs$cooks.distance)
  # ------------- generate plot
  p <- ggplot(df, aes(x=Hat.Values, y=Studentized.Residuals,
                      size=Cooks.Distance)) +
    geom_point(shape=1) +  # make rings
    # Scale the size of the bubbles
    scale_size(range = c(0, min(2*n * max(df$Cooks.Distance), 25)))   # scale the size of the rings


  # add vertical lines
  if(m3 <= max(df$Hat.Values)){
    p <- p + geom_vline(xintercept=m3, lty=2) +
      geom_vline(xintercept = m2, lty=2)
  }else if(m2 <= max(df$Hat.Values)){
    p <- p + geom_vline(xintercept=m2, lty=2)
  }

  # add horizontal lines
  if(min(df$Studentized.Residuals) < 0 &
     max(df$Studentized.Residuals) > 0){
    p <- p + geom_hline(yintercept = 0, lty=2)
  }
  if(min(df$Studentized.Residuals) < -2 &
     max(df$Studentized.Residuals) >= 2){
    p <- p + geom_hline(yintercept = 2, lty=2) +
      geom_hline(yintercept = -2, lty=2)
  }else if(min(df$Studentized.Residuals) > -2 &
           max(df$Studentized.Residuals) >= 2){
    p <- p + geom_hline(yintercept = 2, lty=2)
  }else if(min(df$Studentized.Residuals) <= -2 &
           max(df$Studentized.Residuals) <= 2){
    p <- p + geom_hline(yintercept = -2, lty=2)
  }

  # save the names of the observations
  names <- rownames(df)
  # initialization of a vector for the influential obs.
  all.bool <- rep(FALSE, times = n)
  # find the possible influential for Hat Values
  hatval <- order(df$Hat.Values, decreasing = TRUE)[1:2]
  # find the possible influential obs for rstud
  rstud <- order(abs(df$Studentized.Residuals), decreasing = TRUE)[1:2]
  # find the possible influential obs. for CookD
  cook <-order(df$Cooks.Distance, decreasing = TRUE)[1:2]
  # keep only all influential obs. one time
  all <- union(rstud, union(hatval, cook))
  # define the influential obs.
  all.bool[all] <- TRUE

  # ------------- add the influential obs
  # add the labels of the influential obs
  p <- p + ggrepel::geom_text_repel(aes(label=
                                      ifelse(all.bool,
                                             names, '')), size = 4,
                                    colour = 'red')

  # add ylab, ylab and title
  p <- p + scale_x_continuous(name = 'Leverage') +
    scale_y_continuous(name = 'Studentized Residuals') +
    ggplot2::ggtitle('Leverage vs. Studentized Residuals')
  # add the theme
  p <- p + theme_olsdiagnosticR() +
    theme(legend.position = 'none') # delete the legend

  # add footnote
  p <- p + labs(caption = 'possible outlier')

  # add color for further information
  p <- p + ggplot2::geom_rect(aes(xmin = -Inf, xmax = m2,
                                 ymin = -2, ymax = 2),
                             fill = 'green', alpha=0.003) +
    ggplot2::geom_rect(aes(xmin = -Inf, xmax = m2,
                           ymin = -Inf, ymax = -2 - 1e-08),
                       fill = 'orange', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = -Inf, xmax = m2,
                           ymin = 2 + 1e-08, ymax = Inf),
                       fill = 'orange', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = m2 + 1e-08, xmax = Inf,
                            ymin = -2, ymax = 2),
                        fill = 'orange', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = m2 + 1e-08, xmax = Inf,
                           ymin = 2 + 1e-08, ymax = Inf),
                       fill = 'red', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = m2 + 1e-08, xmax = Inf,
                           ymin = -Inf, ymax = -2 - 1e-08),
                       fill = 'red', alpha = 0.003)

  return(p)

}

