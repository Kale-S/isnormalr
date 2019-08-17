#' @title
#' Box-Whisker-Plot
#'
#' @description
#' Creat a Box-Whisker-Plot for each variable in X
#'
#' @param X design matrix
#'
#' @return
#' @import
#' ggplot2
#'
#' @export
box_plot_x <- function(X){
  n <- dim(X)[1]
  k <- dim(X)[2]

  # only use numeric and non factor variable
  isfactor <- apply(X, 2, is.factor)
  isnumeric <- apply(X, 2, is.numeric)
  filter <- isnumeric == !isfactor

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
#' Histogram for each variable in design matrix
#'
#' @inheritParams box_plot_x
#'
#' @return
#' Returns a histogramm for each variablen in the design matrix
#'
#' @export
#'
#' @import
#' ggplot2
#' tidyr
#' @examples
hist_x <- function(X){
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
#' Plots the cooks distance
#'
#' @param lm a lm model
#'
#' @return
#' A plot of the cooks distance
#'
#' @export
influence.observation <- function(mod){
## Cook's distance as levrage representation

  # second case
  X <- model.matrix(mod)
  e <- mod$residuals
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

#' Visualisation of the cook's distances
#'
#' @param cd Cook's distance
#'
#' @return Barplot
#' @export
#' @import
#' ggplot2
#' ggrepel
#' @examples
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
#' @param lm linear model
#'
#' @return
#' A plot
#' @export
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
  p <- ggplot(df, aes(x=Hat.Values, y=Studentized.Residuals,
                      size=Cooks.Distance)) +
    geom_point(shape=1) +  # make rings
    scale_size(range = c(0, min(2*n * max(d), 25)))   # scale the size of the rings


  # add vertical lines
  ifelse(m3 <= max(h),
         p <- p + geom_vline(xintercept=m3, lty=2) +
           geom_vline(xintercept = m2, lty=2),
         ifelse(m2 <= max(h),
                p <- p + geom_vline(xintercept=m2, lty=2)))
  # add horizontal lines
  ifelse(min(t) < 0 & max(t) > 0,
         p <- p + geom_hline(yintercept = 0, lty=2))
  ifelse(min(t) < -2 & max(t) >= 2,
         p <- p + geom_hline(yintercept = 2, lty=2) +
           geom_hline(yintercept = -2, lty=2),
         ifelse(min(t) > -2 & max(t) >= 2,
                p <- p + geom_hline(yintercept = 2, lty=2),
                ifelse(min(t) <= -2 & max() <= 2,
                       p <- p + geom_hline(yintercept = -2, lty=2))))

  # add the names of the influence observations
  names <- names(d)
  p <- p + ggrepel::geom_text_repel(aes(label=ifelse(h >= m2,
                                                     names,'')),
               size=4) +
    ggrepel::geom_text_repel(aes(label=ifelse(t >= 2 | t <= -2,
                                              names,'')), size=4)

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
                       fill = 'red', alpha = 0.003) +
    # add text
    ggplot2::annotate('text', x = 0.1, y = 0, label = 'Low residuals\nLow leverage',
                      color = 'deepskyblue1') +
    ggplot2::annotate('text', label = 'Low leverage\nHigh residuals',
                      x = 0.1, y = 2.2, color = 'orange') +
    ggplot2::annotate('text', label = 'Low leverage\nHigh residuals',
                      x = 0.1, y = -2.2, color = 'orange') +
    ggplot2::annotate('text', label = 'Low residuals\nHigh leverage',
                      x = m2 + 0.03, y = 0, color = 'orange') +
    ggplot2::annotate('text', label = 'High residuals\nHigh leverage',
                      x = m2 + 0.03, y = 2.2, color = 'red') +
    ggplot2::annotate('text', label = 'High residuals\nHigh leverage',
                      x = m2 + 0.03, y = -2.2, color = 'red')


  return(p)

}

