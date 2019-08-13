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


  #----------- generate plot
  bWp <- ggplot2::ggplot(stack(X), aes(X=ind, y = values)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ind, scales="free") +
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
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
#'
#' @examples
hist_x <- function(X){

  # Used to hold only numeric columns
  nums <- unlist(lapply(X, is.numeric))

  X <- X[, nums]
    # Convert to key-value pairs
  X <- gather(X)
    # Plot the values
  h <-  ggplot2::ggplot(X, aes(value)) +
      # In separate panals
      ggplot2::facet_wrap(~ key, scales='free') +
      # As histogram
      ggplot2::geom_histogram()

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
# Cook's distance as levrage representation

  # Extract the Variable
  #X <- data.frame(mod$model)
  #y_hat <- predict(mod, newdata=X)
  #formula <- mod$call$formula

  # second case
  X <- model.matrix(mod)
  e <- mod$residuals
  n <- dim(X)[1]
  p <- dim(X)[2]
  s2 <- sum(e^2) / (n - p)

  # Define a Vector for the results
  #D <- rep(0, n)

  #for(i in 1:n){
  #  X_without <- X[-i, ]
  #  model_without <- lm(formula=formula, data=X_without)
  #  fit_without <- predict(model_without, newdata=X)
  #  D[i] <- sum((y_hat - fit_without)^2) / (p * s2)
  #}
  #return(D)

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
    geom_bar(stat = 'identity', width=0.2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3),
          axis.ticks = element_blank()) +
    scale_x_discrete(name='Observation') +
    scale_y_continuous(name='Cooks Distance',
                       limits=c(0,max(df_cd$cd) + 0.1))

## add the threshold value
  th.value <- qf(0.5, p, dim(df_cd)[1] - p) # 3 * mean(df_cd$cd)
  large.obs <- which(df_cd$cd >= th.value)
  names.large.obs <- names[large.obs]

  cd.plot <- cd.plot +
    geom_abline(slope=0, intercept=th.value, col = "red", lty=2)

  if(th.value <= max(df_cd$cd)){
    cd.plot <- cd.plot +
      ggrepel::geom_text_repel(aes(label=ifelse(df_cd$cd>th.value,
                                                names,'')),
                size=2)
  }
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
    scale_size(range = c(0, min(2*n * max(d), 25))) +  # scale the size of the rings
    theme(legend.position = "none") # delete the legend

  # add vertical lines
  ifelse(m3 <= max(h),
         p <- p + geom_vline(xintercept=m3, lty=2) +
           geom_vline(xintercept = m2, lty=2),
         ifelse(m2 <= max(h),
                p <- p + geom_vline(xintercept=m2, lty=2)))
  # add horizontal lines
  ifelse(min(r) < 0 & max(r) > 0,
         p <- p + geom_hline(yintercept = 0, lty=2))
  ifelse(min(r) < -2 & max(r) >= 2,
         p <- p + geom_hline(yintercept = 2, lty=2) +
           geom_hline(yintercept = -2, lty=2),
         ifelse(min(r) > -2 & max(r) >= 2,
                p <- p + geom_hline(yintercept = 2, lty=2),
                ifelse(min(r) <= -2 & max(r) <= 2,
                       p <- p + geom_hline(yintercept = -2, lty=2))))

  # add the names of the influence observations
  names <- names(d)
  p <- p + ggrepel::geom_text_repel(aes(label=ifelse(h >= m2,
                                                     names,'')),
                 size=4) +
    ggrepel::geom_text_repel(aes(label=ifelse(t >= 2 | t <= -2,
                                              names,'')),
                 size=4)


  return(p)

}

