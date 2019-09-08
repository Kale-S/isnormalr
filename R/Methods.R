# set the class name and its representation is a list
setClass('isnormalr', representation('list'))

# Show method
setMethod('show', 'isnormalr', function(object){

  cat('==========================================================================\n')
  cat('---------------- Summary table for the residual analysis -----------------\n')
  cat('==========================================================================\n')

  cat('\n\nAssumption of Normality\n')
  cat(' ', object$Normality_Test$Shapiro.Wilk$method, '\n')
  cat('  \n   statistics\t degrees\t p.value\t reject H0?\n')
  cat('  ', round(object$Normality_Test$Shapiro.Wilk$statistic, 4), '\t\t\t',
      round(object$Normality_Test$Shapiro.Wilk$p.value, 5), '\t', ifelse(
        object$Normality_Test$Shapiro.Wilk$p.value < 0.05, 'Reject HO',
        'Don´t reject H0'
      ))

  cat('\n\n\nMultikolonarity\n')
  cat(' ', 'Variance Inflation Factor\n')
  cat(ifelse(is.character(object$Multikolonarity$VIF),
             object$Mulitkolonarity$VIF,
             ifelse(sum(object$Multikolonarity$VIF >= 2) >= 1,
                '\n  There are VIF that are larger then 2, that is an indicator \n  for multikolonarity in the data',
                '\n  There is no VIF > 2, that is an indicator for \n  no multikolonarity in the data')))
  cat('\n\n\nHeteroscedasticity\n')
  cat(' ', object$Homoskedasticity$Breusch.Pagan$method, '\n')
  cat('  \n   statistics\t degrees \t p.value\t reject H0?\n')
  cat('  ', round(object$Homoskedasticity$Breusch.Pagan$statistic, 4), '\t',
      round(object$Homoskedasticity$Breusch.Pagan$parameter, 4), '\t\t',
      round(object$Homoskedasticity$Breusch.Pagan$p.value, 5), '\t', ifelse(
        object$Homoskedasticity$Breusch.Pagan$p.value < 0.05, 'Reject HO',
        'Don´t reject H0'
      ))

  #cat('\n\n\n==========================================================================\n')
  #cat('--------------------------------------------------------------------------\n')
  #cat('==========================================================================')

  cat('\n\n\n\n')

  cat('---------------------------- RECOMMENDATIONS -----------------------------')
  cat('\n\n')

  cat('Assumption of Normality')
  cat('\n  ')
  if(object$Normality_Tests$Shapiro.Wilk$p.value <= 0.05){

      cat(paste0(c(
        'With the Shapiro-Wilk test the H0 hypothesis can be rejected.',
        'This is an indicator that the error terms may not be normally distributed.',
        'A transformation of the dependent variable could be considered.',
        'For this we recommend a transformation from the Box-Cox family.'),
         collapse = '\n  '))
  }else{

      cat(paste0(c(
        'The Shapiro-Wilk test does not show that the error terms are not',
        'normally distributed.',
        'No further action is necessary.'),
        collapse = '\n  '))
  }


  cat('\n\n\n')
  cat('Multikolonarity')
  cat('\n  ')
  # Multicollinearity
  if(any(object$Multikolonarity$VIF > 10)){

      cat(paste0(c(
        'The independent variables contain variables with a VIF > 10.',
        'We recommend to use the most important columns for information gain:'),
        collapse = "\n  "),
        '\n  (', object$Multikolonarity$Contain.Columns, ')')

  }else{

      cat(paste0(c(
        'There are no variables with a VIF > 10 in the data.',
        'No further action is necessary.'),
        collapse = '\n  '))
  }

  cat('\n\n\n')
  cat('Heteroscedasticity')
  cat('\n  ')
  # Heteroskedasticity
  if(any(object$Homoskedasticity$Breusch.Pagan$statistic < 0.05)){

      cat(paste0(c(
        ' With the Breusch Pagan test the H0 hypothesis can be rejected.',
        ' This is an indicator that the error terms may not be homoscedastic.',
        ' A different estimator could be considered.',
        ' For this we recommend the general least squares estimator'),
        collapse = '\n  '))

  }else{
      cat(paste0(c(
        'The Breusch Pagan test does not show that the error terms are not',
        'heteroscedastic',
        'No further action is necessary.'),
        collapse = '\n  '))
  }

  cat('\n\n')
  cat('Outlier')
  cat('\n  ')
  cat('Possible Outlier')
  cat('\n  ')
  print(object$Outlier$Possible.Outlier)
  cat('It is recommended to take a closer look at these',
     dim(object$Outlier$Possible.Outlier)[1], 'observations.')
  cat('\n\n')
  cat('==========================================================================')
  cat('\n')
  cat('--------------------------------------------------------------------------')
  cat('\n')
  cat('==========================================================================')

  ## add info page
  plot.new()
  lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)
  arrows(1, 0.021, 0.9, 0.021, code = 1, lwd = 2, col = '#757575')
  text(0.66, 0.026, 'Check Assumption of\n Normality', lwd = 2,
       col = '#757575')
  # Normal assumption
  print(object$Plots$QQ.Plot)
  print(object$Plots$Residual.Hist, nrow=2)
  # Outlier
  ## add info page
  plot.new()
  arrows(0, 0.99, 0.1, 0.99, code = 1, lwd = 2, col = '#757575')
  text(0.33, 0.99, 'Check Assumption of\n Normality', lwd = 2,
       col = '#757575')
  lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)
  arrows(1, 0.021, 0.9, 0.021, code = 1, lwd = 2, col = '#757575')
  text(0.76, 0.026, 'Detection of\nOutlier', lwd = 2,
       col = '#757575')

  print(object$Plots$Box.Plot.X)
  print(object$Plots$Regressor.Hist, nrow=2)
  print(object$Plots$Cooks.Distance.plot)
  print(object$Plots$Bubble.Plot, nrow=2)

  ## add info page
  plot.new()
  arrows(0, 0.99, 0.1, 0.99, code = 1, lwd = 2, col = '#757575')
  text(0.25, 0.99, 'Detection of\nOutlier', col = '#757575',
       lwd = 2)
  lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)
  arrows(1, 0.021, 0.9, 0.021, code = 1, lwd = 2, col = '#757575')
  text(0.7, 0.026, 'Detection of\nHeteroscedasticity', lwd = 2,
       col = '#757575')
  # Homoskedasticity
  print(object$Plots$Spread.Level.Plot$Spread.level)
  ## add help page
  plot.new()
  arrows(0, 0.99, 0.1, 0.99, code = 1, lwd = 2, col = '#757575')
  text(0.3, 0.99, 'Detection of\nHeteroscedasticity', lwd = 2,
       col = '#757575')
  lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)





})








