#' @title
#' Box-Whisker-Plot
#'
#' @description
#' A Box-Whisker-Plot for each numeric column in the design matrix
#'
#'
#' @usage \code{isnormalr:::box_plot_x(X)}
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
#'
#' @param X design matrix (nxk)
#'
#'
#' @return
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
#' isnormalr:::box_plot_x(Z)
#'
#' Y <- matrix(rexp(100), ncol = 10)
#' isnormalr:::box_plot_x(Y)
#' }
#'
#' @import
#' ggplot2
box_plot_x <- function(X){

  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]

  }
  X <- data.frame(X)
  n <- dim(X)[1]
  k <- dim(X)[2]

  # only use numeric and non factor variable
  isfactor <- sapply(X, is.factor)
  isnumeric <- sapply(X, is.numeric)
  filter <- isnumeric | !isfactor

  # apply the filter on X
  X <- X[, filter]

  #----------- generate plot
  bWp <- ggplot2::ggplot(stack(X), aes(X=ind, y = values)) +
    ggplot2::geom_boxplot(fill="#666666", alpha=0.6, color='black',
                          outlier.colour="black") +
    #theme(axis.text.x=element_blank(),
    #      axis.title.y = element_blank())
    #geom_jitter(shape=13, position=position_jitter(0.2)) +
    ggplot2::facet_wrap(~ind, scales="free")

  # add xlab, ylab and title
  title <- ifelse(k > 1,
                  'Boxplot`s of the independend variable',
                  'Boxplot of the independend variable')
  bWp <- bWp + ggtitle(title) + xlab('X')


  # add the theme
  bWp <- bWp + theme_isnormalr() +
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
#' @keywords
#'
#' @usage
#' \code{isnormalr:::hist_x(X)}
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
#' @inheritParams box_plot_x
#'
#'
#'
#' @return
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
#'
#' }
#'
#' @import
#' ggplot2
#' tidyr
hist_x <- function(X){

  if(colnames(X)[1] == '(Intercept)'){
    X <- X[, -1]
  }
  X <- data.frame(X)
  n <- dim(X)[1]
  k <- dim(X)[2]
  bins <- isnormalr:::Square.root(n)
  # Used to hold only numeric columns
  nums <- apply(X, 2, is.numeric)

    X <- X[, nums]
    # Convert to key-value pairs
  X <- tidyr::gather(X)
    # Plot the values
  h <-  ggplot2::ggplot(X, aes(value)) +
      # In separate panals
      ggplot2::facet_wrap(~ key, scales='free') +
      # As histogram
      ggplot2::geom_histogram(col=('black'), alpha=0.6,
                              bins=bins)

  # add title
  title <- ifelse(k > 1,
                  'Histogram`s of the independend variable',
                  'Histogram of the independend variable')
  h <- h + ggtitle(title)

  # add theme
  h <- h + theme_isnormalr() +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank())
  return(h)
}

#' @title
#' Influence Observation
#'
#' @param lm a lm model
#'
#' @description
#' This function is a suite of functions that can be used to compute
#' some of the regression (leve-one-out deletion) diagnostics for a
#' linear models discussed in Belsley, Kuh and Welsch (1980),
#' Cook and Weisberg (1982)
#'
#'
#' @usage
#' \code{isnormalr:::influence.observation(object)}
#'
#' @details
#' def of Standardized residuals
#' def of Studentized residuals
#' deg of Hat_Values / Lavarage Values
#' def of Cooks distance
#'
#'
#'
#' @return
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
influence.observation <- function(object){
## Cook's distance as levrage representation

  # second case
  X <- model.matrix(object)
  e <- object$residuals
  n <- dim(X)[1]
  p <- dim(X)[2]
  s2 <- sum(e^2) / (n - p)

  # Calculate the projection matrix
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  # Exctract the lavarage Values
  h <- diag(H)
  # Calculate the Cooks distance
  D <- (e^2 / (s2 * p)) * (h / (1 - h)^2)

  #  Calculate the standardized residuals
  r <- e / (sqrt(s2) * sqrt(1-h))

  # Calculate the studentized residuals
  t <- r * sqrt((n - (p - 1) - 2) /
                  (n - (p - 1) - 1 - r^2))


  # Create an s3 object
  influence.obs <- list(leverage.value = h,
                        cooks.distance = D,
                        standardized.residuals = r,
                        studentized.residuals = t)

  class(influence.obs) <- 'influential.observation'

  return(influence.obs)
}

#' @title
#' Visualisation of the cook's distances
#'
#' @param cd Cook's distance
#' @param p number of coefficents p = k - 1
#'
#' @return Barplot
#'
#'
#'
#'
#' @import
#' ggplot2
#' ggrepel
#' @examples
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
#'
plot.cd <- function(cd, p){
  df_cd <- data.frame("cd"=cd)
  names <- rownames(df_cd)
  cd.plot <- ggplot(df_cd, aes(x = names, y = cd)) +
    geom_bar(stat = 'identity', width=0.2, col=I('black'),
             alpha = 0.6)

## add the threshold value
  th.value <- qf(0.5, p, dim(df_cd)[1] - p) # 3 * mean(df_cd$cd)
  large.obs <- which(df_cd$cd >= th.value)
  names.large.obs <- names[large.obs]

  cd.plot <- cd.plot +
    geom_abline(slope=0, intercept=th.value, col = "red", lty=2)

  if(th.value <= max(df_cd$cd)){
    cd.plot <- cd.plot +
      ggrepel::geom_text_repel(aes(label=ifelse(df_cd$cd>th.value,
                                                names,'')), size=2)
  }

  # add xlab, ylab and title
  cd.plot <- cd.plot  +
    scale_x_discrete(name='Observation') +
    scale_y_continuous(name='Cooks Distance',
                       limits=c(0,max(df_cd$cd) + 0.1)) +
    ggplot2::ggtitle('Cook`s Distance plot')
  # add the theme
  cd.plot <- cd.plot + theme_isnormalr() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3, size=10),
          axis.ticks = element_blank())

  return(cd.plot)
}


#' @title
#' Plots the influence of observation on the regression line
#'
#' @description
#'
#' @usage \code{isnormalr:::influence.plot(t, h, d)}
#'
#' @description
#'
#'
#' @param t numeric vector of studentized residuals
#' @param h numeric vector of hat values
#' @param d numeric vector of cook distance values
#'
#'
#'
#' @return
#' A plot
#'
#' @references
#'
#'
#'
#' @import
#' ggplot2
#' ggrepel
influence.plot <- function(t, h, d){ # r or t?
  n <- length(d)
  m2 <- 2 * mean(h)
  m3 <- 3 * mean(h)
  df <- data.frame('Hat.Values'=h,
                   'Studentized.Residuals'=t,
                   'Cooks.Distance'=d)
  # scale of the bubbels
  scale <- 10 / max(df$Cooks.Distance)
  p <- ggplot(df, aes(x=Hat.Values, y=Studentized.Residuals,
                      size=Cooks.Distance)) +
    geom_point(shape=1) +  # make rings
    scale_size(range = c(0, min(2*n * max(d), 25)))   # scale the size of the rings


  # add vertical lines
  if(m3 <= max(h)){
    p <- p + geom_vline(xintercept=m3, lty=2) +
      geom_vline(xintercept = m2, lty=2)
  }else if(m2 <= max(h)){
    p <- p + geom_vline(xintercept=m2, lty=2)
  }
  #ifelse(m3 <= max(h),
  #       p <- p + geom_vline(xintercept=m3, lty=2) +
  #         geom_vline(xintercept = m2, lty=2),
  #       ifelse(m2 <= max(h),
  #              p <- p + geom_vline(xintercept=m2, lty=2)))
  # add horizontal lines
  if(min(t) < 0 & max(t) > 0){
    p <- p + geom_hline(yintercept = 0, lty=2)
  }
  if(min(t) < -2 & max(t) >= 2){
    p <- p + geom_hline(yintercept = 2, lty=2) +
      geom_hline(yintercept = -2, lty=2)
  }else if(min(t) > -2 & max(t) >= 2){
    p <- p + geom_hline(yintercept = 2, lty=2)
  }else if(min(t) <= -2 & max(t) <= 2){
    p <- p + geom_hline(yintercept = -2, lty=2)
  }
  #ifelse(min(t) < 0 & max(t) > 0,
  #       p <- p + geom_hline(yintercept = 0, lty=2))
  #ifelse(min(t) < -2 & max(t) >= 2,
  #       p <- p + geom_hline(yintercept = 2, lty=2) +
  #         geom_hline(yintercept = -2, lty=2),
  #       ifelse(min(t) > -2 & max(t) >= 2,
  #              p <- p + geom_hline(yintercept = 2, lty=2),
  #              ifelse(min(t) <= -2 & max() <= 2,
  #                     p <- p + geom_hline(yintercept = -2, lty=2)),
  #              ''))

  # add the names of the influence observations
  names <- names(d)
  if(sum(h >= m2 | t >= 2 | t<= -2) < 10){
     p <- p + ggrepel::geom_text_repel(aes(label=ifelse(h >= m2,
                                                     names,'')),
               size=4) +
              ggrepel::geom_text_repel(aes(label=
                                             ifelse(t >= 2 | t <= -2,
                                              names,'')), size=4)
  }else{ #default method if many observations
    all.bool <- rep(FALSE, times = n)
    hatval <- order(df$Hat.Values, decreasing = TRUE)[1:2]
    rstud <- order(abs(df$Studentized.Residuals), decreasing = TRUE)[1:2]
    cook <- order(df$Cooks.Distance, decreasing = TRUE)[1:2]
    all <- union(rstud, union(hatval, cook))
    all.bool[all] <- TRUE
    p <- p + ggrepel::geom_text_repel(aes(label=
                                        ifelse(all.bool,
                                               names, "")), size = 4)
  }

  # add ylab, ylab and title
  p <- p + scale_x_continuous(name = 'Leverage') +
    scale_y_continuous(name = 'Studentized Residuals') +
    ggplot2::ggtitle('Leverage vs. Studentized Residuals')
  # add the theme
  p <- p + theme_isnormalr() +
    theme(legend.position = "none") # delete the legend

  # add information
  p <- p + ggplot2::geom_rect(aes(xmin = -Inf, xmax = m2,
                                 ymin = -2, ymax = 2),
                             fill = 'deepskyblue1', alpha=0.003) +
    ggplot2::geom_rect(aes(xmin = -Inf, xmax = m2,
                           ymin = -Inf, ymax = -2.00001),
                       fill = 'orange', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = -Inf, xmax = m2,
                           ymin = 2.00001, ymax = Inf),
                       fill = 'orange', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = m2 + 0.0001, xmax = Inf,
                            ymin = -2, ymax = 2),
                        fill = 'orange', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = m2 + 0.0001, xmax = Inf,
                           ymin = 2.0001, ymax = Inf),
                       fill = 'red', alpha = 0.003) +
    ggplot2::geom_rect(aes(xmin = m2 + 0.0001, xmax = Inf,
                           ymin = -Inf, ymax = -2.0001),
                       fill = 'red', alpha = 0.003) #+
    # add legend
  p <- p + scale_fill_manual(values = c('deepskyblue1',
                                        'orange',
                                        'red'),
                             labels =
                               c('Low residuals and\nLow leverage',
                                 'High residuals or\nHigh leverage',
                                 'High residuals and\nHigh leverage'))
  p <- p + theme(legend.position = 'bottom')
    #ggplot2::annotate('text', x = 0.1, y = 0, label = 'Low residuals\nLow leverage',
    #                  color = 'deepskyblue1') +
    #ggplot2::annotate('text', label = 'Low leverage\nHigh residuals',
    #                  x = 0.1, y = 2.2, color = 'orange') +
    #ggplot2::annotate('text', label = 'Low leverage\nHigh residuals',
    #                  x = 0.1, y = -2.2, color = 'orange') +
    #ggplot2::annotate('text', label = 'Low residuals\nHigh leverage',
    #                  x = m2 + 0.03, y = 0, color = 'orange') +
    #ggplot2::annotate('text', label = 'High residuals\nHigh leverage',
    #                  x = m2 + 0.03, y = 2.2, color = 'red') +
    #ggplot2::annotate('text', label = 'High residuals\nHigh leverage',
    #                  x = m2 + 0.03, y = -2.2, color = 'red')


  return(p)

}

