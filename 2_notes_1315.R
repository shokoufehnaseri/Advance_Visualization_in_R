p1 +
  theme(
    panel.background = element_rect(fill = 'black', color='lightgreen', size=2),
    plot.background = element_rect(fill = 'lightblue', color='#90549c', size=2),
    axis.line = element_line(linewidth=1.2, arrow = arrow(), color='white'),
    plot.title = element_text(size=16, color='navy', hjust=.5),
    panel.grid.major = element_line(linetype = 'dotted'),
    panel.grid.minor = element_blank(),
    axis.text = element_text(angle=45,hjust=1)
  )

mytheme <- theme(
  panel.background = element_rect(fill = 'black', color='lightgreen', size=2),
  plot.background = element_rect(fill = 'lightblue', color='#90549c', size=2),
  axis.line = element_line(linewidth=1.2, arrow = arrow(), color='white'),
  plot.title = element_text(size=16, color='navy', hjust=.5),
  panel.grid.major = element_line(linetype = 'dotted'),
  panel.grid.minor = element_blank()
)

p1 + mytheme

save(movies, p1, mytheme, file='something for graphs.RData')

p1 +
  theme_dark()

p1 +
  theme_minimal()

p1 +
  theme_void() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), units = 'cm') #  top, right, bottom, and left 
  )

p1 +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), units = 'cm') #  top, right, bottom, and left 
  ) +
  theme_void()

install.packages('ggthemes')
library(ggthemes)

p1 + theme_economist()
p1 + theme_wsj()
p1 + theme_sta

