# set the class name and its representation is a list
setClass('olsdiagnosticR', representation('list'))

# Show method
setMethod('show', 'olsdiagnosticR', function(object){

  cat('==========================================================================\n')
  cat('---------------- Summary table for the residual analysis -----------------\n')
  cat('==========================================================================\n')

  cat('\n\nAssumption of Normality\n')
  cat(' ', object$Normality_Test$method, '\n')
  cat('  \n   statistics\t\t\t p.value\t reject H0?\n')
  cat('  ', round(object$Normality_Test$statistic, 4), '\t\t\t',
      round(object$Normality_Test$p.value, 5), '\t', ifelse(
        object$Normality_Test$p.value < object$Inputs$pval, '\t YES',
        '\t NO'
      ))

  cat('\n\n\nMulticollinearity\n')
  cat(' ', 'Variance Inflation Factor\n')
  cat(ifelse(is.character(object$Multicollinearity$VIF),
             object$Mulitkolonarity$VIF,
             ifelse(sum(object$Multicollinearity$VIF >= 10) >= 1,
                '\n  There are VIF that are larger then 10, that is an indicator \n  for multicollinearity in the data',
                '\n  There is no VIF > 10, that is an indicator for \n  no multicollinearity in the data')))
  cat('\n\n\nHeteroscedasticity\n')
  cat(' ', object$Heteroskedasticity$Breusch.Pagan$method, '\n')
  cat('  \n   statistics\t degrees \t p.value\t reject H0?\n')
  cat('  ', round(object$Heteroskedasticity$statistic, 4), '\t',
      round(object$Heteroskedasticity$parameter, 4), '\t\t',
      round(object$Heteroskedasticity$p.value, 5), '\t', ifelse(
        object$Heteroskedasticity$p.value < object$Inputs$pval, '\t YES',
        '\t NO'
      ))

  #cat('\n\n\n==========================================================================\n')
  #cat('--------------------------------------------------------------------------\n')
  #cat('==========================================================================')

  cat('\n\n\n\n')

  cat('---------------------------- RECOMMENDATIONS -----------------------------')
  cat('\n\n')

  cat('Assumption of Normality')
  cat('\n  ')
  if(object$Normality_Tests$p.value <= object$Inputs$pval){

      cat(paste0(c(
        paste('With the', object$Normality_Test$method, 'the H0 hypothesis can be rejected.'),
        'This is an indicator that the error terms may not be normally distributed.',
        'A transformation of the data could be considered.',
        'For this, we recommend a transformation from the Box-Cox family.'),
         collapse = '\n  '))
  }else{

      cat(paste0(c(
        paste(object$Normality_Test$method, 'does not show that the error terms are not'),
        'normally distributed.',
        'No further action is necessary.'),
        collapse = '\n  '))
  }


  cat('\n\n\n')
  cat('Multicollinearity')
  cat('\n  ')
  # Multicollinearity
  if(any(object$Multicollinearity$VIF > 10)){

      cat(paste0(c(
        'The independent variables contain variables with a VIF > 10.',
        'We recommend to use the most important columns for information gain:'),
        collapse = "\n  "),
        '\n  (', object$Multicollinearity$Contain_Columns, ')')

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
  if(any(object$Heteroskedasticity$p.value < object$Inputs$pval)){

      cat(paste0(c(
        ' With the Breusch Pagan test the H0 hypothesis can be rejected.',
        ' This is an indicator that the error terms may not be homoscedastic.',
        ' A different estimator could be considered.',
        ' For this, we recommend the general least squares estimator'),
        collapse = '\n  '))

  }else{
      cat(paste0(c(
        'The Breusch Pagan test does not show that the error terms are not',
        'heteroscedastic.',
        'No further action is necessary.'),
        collapse = '\n  '))
  }

  cat('\n\n')
  cat('Outlier')
  cat('\n  ')
  cat('Possible Outlier')
  cat('\n  ')
  print(object$Outlier$Possible_Outlier)
  cat('It is recommended to take a closer look at these',
     dim(object$Outlier$Possible_Outlier)[1], 'observations.')
  cat('\n\n')
  cat('==========================================================================')
  cat('\n')
  cat('--------------------------------------------------------------------------')
  cat('\n')
  cat('==========================================================================')

  ## add info page
  graphics::plot.new()
  graphics::lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)
  graphics::arrows(1, 0.021, 0.9, 0.021, code = 1, lwd = 2, col = '#757575')
  graphics::text(0.66, 0.026, 'Check Assumption of\n Normality', lwd = 2,
       col = '#757575')
  # normal assumption
  print(object$Plots$QQ_Plot)
  print(object$Plots$Residual_Hist, nrow=2)
  # outlier
  ## add info page
  graphics::plot.new()
  graphics::arrows(0, 0.99, 0.1, 0.99, code = 1, lwd = 2, col = '#757575')
  graphics::text(0.33, 0.99, 'Check Assumption of\n Normality', lwd = 2,
                 col = '#757575')
  graphics::lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)
  graphics::arrows(1, 0.021, 0.9, 0.021, code = 1, lwd = 2, col = '#757575')
  graphics::text(0.76, 0.026, 'Detection of\nOutlier', lwd = 2,
                 col = '#757575')

  print(object$Plots$Box_Plot_X)
  print(object$Plots$Regressor_Hist, nrow=2)
  print(object$Plots$Cooks_Distance_Plot)
  print(object$Plots$Bubble_Plot, nrow=2)

  ## add info page
  graphics::plot.new()
  graphics::arrows(0, 0.99, 0.1, 0.99, code = 1, lwd = 2, col = '#757575')
  graphics::text(0.25, 0.99, 'Detection of\nOutlier', col = '#757575',
                 lwd = 2)
  graphics::lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)
  graphics::arrows(1, 0.021, 0.9, 0.021, code = 1, lwd = 2, col = '#757575')
  graphics::text(0.7, 0.026, 'Detection of\nHeteroscedasticity', lwd = 2,
                 col = '#757575')
  # Heteroskedasticity
  print(object$Plots$Spread_Level_Plot)
  ## add help page
  graphics::plot.new()
  graphics::arrows(0, 0.99, 0.1, 0.99, code = 1, lwd = 2, col = '#757575')
  graphics::text(0.3, 0.99, 'Detection of\nHeteroscedasticity', lwd = 2,
                 col = '#757575')
  graphics::lines(c(0, 1), c(0, 1), col = '#757575', lwd = 3)

})







