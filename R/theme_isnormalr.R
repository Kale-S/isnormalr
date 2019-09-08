theme_isnormalr <- function(){
  ltgray <- '#cccccc'
  dkgray <- '#757575'
  dkgray2 <- '#666666'
  theme_minimal(base_size = 12) +
    theme(
      rect = element_rect(colour = 'black', fill = 'white'),
      line = element_line(colour = 'black'), text = element_text(colour = dkgray),

      plot.title = element_text(face = 'plain', size = rel(20/12),
                                hjust = 0, colour = dkgray),
      plot.subtitle = element_text(hjust = 0,
                                   size = rel(1), face = 'plain', colour = dkgray),
      plot.caption = element_text(hjust = 0, size = rel(1),
                                  face = 'plain', colour = dkgray),

      strip.text = element_text(hjust = 0, size = rel(1),
                                colour = dkgray2, face = 'plain'),
      strip.background = element_rect(colour = NA, fill = NA),

      axis.title = element_text(face = 'plain', colour = dkgray2,
                                size = rel(1)),
      axis.text = element_text(face = 'plain', colour = dkgray, size = rel(1)),
      axis.line = element_line(colour = 'black'),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),

      panel.grid.major = element_line(colour = ltgray),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.border = element_rect(fill = NA, colour = 'black', size=1),

      legend.background = element_rect(colour = NA),
      legend.text = element_text(size = rel(1), colour = dkgray),
      legend.title = element_text(size = rel(1), colour = dkgray2,
                                  face = 'plain'),
      legend.key = element_rect(colour = NA),
      legend.position = 'right', legend.direction = 'vertical',

      plot.margin = unit(c(1, 2, 1, 1), 'cm')
    )
}
