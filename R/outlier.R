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
#' @import
#' car
cooks.dist <- function(mod){
# Cook's distance as levrage representation

  # Extract the Variable
  X <- data.frame(mod$model)
  y_hat <- predict(mod, newdata=X)
  formula <- mod$call$formula
  e <- mod$residuals
  n <- dim(X)[1]
  p <- dim(X)[2]
  s2 <- sum(e^2) / (n - p)
  # Define a Vector for the results
  D <- rep(0, n)

  for(i in 1:n){
    X_without <- X[-i, ]
    model_without <- lm(formula=formula, data=X_without)
    fit_without <- predict(model_without, newdata=X)
    D[i] <- sum((y_hat - fit_without)^2) / (p * s2)
  }
  #return(D)

  # second case
  X <- model.matrix(mod)
  e <- mod$residuals

  H <- X %*% solve(t(X) %*% X) %*% t(X)
  h <- diag(H)

  D2 <- (e^2 / (s2 * p)) * (h / (1 - h)^2)

  return(list(D,
              D2))
}

#' Visualisation of the cook's distances
#'
#' @param cd Cook's distance
#'
#' @return Barplot
#' @export
#' @import
#' ggplot2
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
      geom_text(aes(label=ifelse(df_cd$cd>th.value, names,'')),
                size=2, angle=45, hjust=0.1)
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
#' car
influence.obs <- function(lm){
  io <-   influencePlot(lm, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  return(io)
}

