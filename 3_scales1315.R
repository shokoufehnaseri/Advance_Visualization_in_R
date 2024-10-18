# ----------------------------------------------------- #
#             Advanced Visualisation in R               #
#                   Piotr Ćwiakowski                    #
#           Faculty of Economic Sciences UW             #
#              Scale editing in ggplot2                 #
# ----------------------------------------------------- #

# Read database
setwd("..")
movies <- read.csv('Data/top250.csv',sep=';',dec=',')

# Install libraries
# install.packages("ggplot2")
# install.packages("RColorBrewer")
# install.packages("scales")
# install.packages("dplyr")
# install.packages("tidyr")

# Read libraries
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Exemplary graph
p <- ggplot(data = movies, aes(x = votes, y = budget))
p1 <- p + 
  labs(x = "Votes", y = "Budget", title = "Budget vs Year of production") + 
  geom_point() 
p1

# 1. Continuous scale ----------------------------------------------------------

# 1.1 Continuous scale vs scientific notation.
# 
# The `scale_` keyword allows to configure the axis labels:
# * their frequency can be changed with `breaks` argument, 
# * the way they are displayed on the plot - the `labels` argument.
#
# Default parameter in ANY scale function is:
# * name
# * breaks
# * labels
# * limits

p1 + 
  scale_y_continuous(name = "Budget [thousands of USD]",
                     breaks = seq(0, 200000000, by = 10000000)
                     )

options(scipen=12)

p1 + 
  scale_y_continuous(name = "Budget [thousands of USD]",
                     breaks = seq(0, 200000000, by = 10000000)
  )

p1 + 
  scale_y_continuous(name = "Budget [thousands of USD]",
                     breaks = seq(0, 200000000, by = 10000000), # use function seq() - operate on original scale values
                     labels = format(seq(0, 200000, by = 10000))
  )

p1 + 
  scale_y_continuous(name = "Budget [thousands of USD]",
                     breaks = seq(0, 200000000, by = 10000000), # use function seq() - operate on original scale values
                     labels = format(seq(0, 200000, by = 10000), big.mark = " ")) # put scaled labels with space as bigval separator

p + 
  labs(x = "Votes", y = "Budget", title = "Budget vs Year of production") + 
  geom_point() + 
  scale_y_continuous(name = "Budget [thousands of USD]",
                     breaks = seq(0,200000000,by = 10000000), # use function seq() - operate on original scale values
                     labels = format(seq(0,200000,by=10000), big.mark = " "), # put scaled labels with space as bigval separator
                     limits = c(10000000, 200000000))
p + 
  labs(x = "Votes", y = "Budget", title = "Budget vs Year of production") + 
  geom_point() + 
  scale_y_continuous(name = "Budget [thousands of USD]",
                     breaks = seq(0,200000000,by = 10000000), # use function seq() - operate on original scale values
                     labels = format(seq(0,200000,by=10000), big.mark = " "), # put scaled labels with space as bigval separator
                     limits = c(10e6, 200e6))

# In arguments of `labels` keyword we have used the `format` function. 
# It substitutes the values of a numerical vector with values of a character 
# vector and allows to e.g. specify how to separate large values (i.e. thousands)
# with the `big.mark` parameter. In our case we will only have one space.

# 1.2 Continuous scale vs percent values.
# 
# There are several ways to change fractional values to percent values.
# For example, we can use the `paste0` function which glues its argument together
# without any separator:

p <- ggplot(data = movies, aes(x = budget, y = p10))

p2 <- p + 
  labs(x= "Budget", y = "Percentage of 10s", title = "Budget vs Percent of highest mark received") +
  geom_point()
p2

p2 + scale_x_continuous(name = "Budget [thousands of USD]", 
                        breaks = seq(0,200000000,by=50000000),
                        labels = format(seq(0,200000,by=50000), big.mark = " ")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),
                     labels = paste0(format(seq(0, 100, by = 10), decimal.mark = ",", nsmall=1),"%"))

p2 + scale_x_continuous(name = "Budget [thousands of USD]", 
                        breaks = seq(0,200000000,by=50000000),
                        labels = format(seq(0,200000,by=50000), big.mark = " "),
                        limits = c(0, 50e06)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),
                     labels = paste0(format(seq(0, 100, by = 10), decimal.mark = ","),"%"))

# There is a different way to format percentages:
ggplot(data = movies, aes(x = budget, y = p10/100)) + 
  labs(x= "Budget", y = "Percentage of 10s", title = "Budget vs Percent of highest mark received") +
  geom_point() +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.05),
    labels = scales::percent,
    # limit = c(0, .6)
  )


percent(.5)
percent(c(.5, .55))
# percent(.5)
# percent(c(.5, .55))
# percent(breaks)
# labels(breaks)

example <- function(x, FUN, ...){
  print('simple wrapper')
  FUN(x, ...)
}

FUN

example(1:10, mean, na.rm = T)

# library(scales)
# percent(0.10)
# percent(c(.10, .15))
# 
# called
# percent(breaks)

# Practice

# On the following graph:

cw <- ggplot(data = movies, aes(x = budget, y = votes)) + 
  labs(x= "Budget", y = "Number of Votes", title = "Budget vs Percent of highest mark received") + 
  geom_point()
cw

# add percentage symbols and increase amount of axis ticks/labels on X and Y axis.
# Additionally eradicate scientific notation on X axis and add dollar symbol.

cw +
  scale_y_continuous(breaks = seq(0, 160e3, by = 20e3),
                     labels = paste0(seq(0, 160, by=20), 'K')) +
  scale_x_continuous(breaks=seq(0, 250e6, by=25e6),
                     labels=scales::dollar) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# 1.3 Continuous scale vs color change.
# It is very intuitive to change colors of the scale using the `scale` function. 
# In the case of two colors we have:
p1 + 
  geom_point(aes(color = rating)) + 
  scale_color_gradient(low = "blue", high = "yellow")

# To present scale with more than two colors we need to use the *scale_color_gradient2* variant:
p1 + 
  geom_point(aes(color = rating)) + 
  scale_color_gradient2(low = 'red', mid = 'orange',   high="yellow", 
                        midpoint = quantile(movies$rating, .5))

p1 + 
  geom_point(aes(color = rating)) + 
  scale_color_gradient2(low = 'red', mid = 'orange',   high="yellow", 
                        midpoint = quantile(movies$rating, .9))

# p1 +
#   geom_point(aes(color = rating)) +
#   scale_color_gradientn()

### Practice 

# On the following graph:

cw <- ggplot(data = movies, aes(x = budget, y = p10)) + 
  labs(x= "Budget", y = "Percentage of 10s", title = "Budget vs Percent of highest mark received") + 
  geom_point()
cw

# Map variable length on color scale and configure gradient in order to present 
# correctly the variance of variable length. Put the gradient bar (legend) at the 
# top of a chart. Additionally change its height, width, number of breaks and position 
# of the title (also move to the top and to the center). You might need to use 
# function guide_colorbar within guides() function.
cw +
  aes(color=length) +
  guides(color = guide_colourbar(theme = theme(
    legend.key.width  = unit(25, "lines"),
    legend.key.height = unit(.5, "lines"), 
   
  ),  position ='top')) +
  scale_color_continuous(breaks=seq(0, 250, 25), 
                         low='red', high='green2') +
  labs(
    color=NULL
  )

# 2. Discrete scale. -----------------------------------------------------------

# Let's first prepare a plot that we will use for next examples:
movies$decade <- cut(movies$year,breaks = seq(1930,2010,by = 10),dig.lab = 4) # create year ranges
p3 <- ggplot(movies, aes(rank, p10/100*votes, color = decade)) + 
      geom_point()
p3

# 2.1 Manipulating the discrete scale.
# Let's try to edit the discrete `colour` scale formatting. 
# We can use the `limits` argument to take into account only some specific 
# ranges of values. With the `breaks` and `labels` argument we only control 
# looks of the legend.

# Color only some chosen points
p3 + scale_colour_discrete(name = "Production\ndecade", 
                           limits = c('(1930,1940]','(1950,1960]','(1980,1990]'),
                           labels = c('\'30','\'50','\'80')) 

# or:
p3 + scale_colour_discrete(name = "Production \ndecade",
                           limits = c(levels(movies$decade)[1],
                                      levels(movies$decade)[3],
                                      levels(movies$decade)[6]),
                           labels = c('\'30','\'50','\'80'))

# Now lets format only the legend:
p3 + scale_colour_discrete(name = "Production \ndecade",
                           breaks = c('(1930,1940]','(1950,1960]','(1980,1990]'),
                           labels = c('\'30','\'50','\'80'))

# 2.2 Discrete scale vs color change.
# It is possible to change discrete scale color with instructions ending with 
# `manual`, `hue` or `brewer`. We highly recommend to color your plots using 
# the predefined `RcolorBrewer` palettes as they are very user-friendly and 
# quite pleasant visually:
colors()
# input colors manually -- not recommended:
p3 + scale_colour_manual(values = c("red","blue","yellow","orange","#68cc83",
                                    "purple","grey","green"))

# Palette example
library(RColorBrewer)
display.brewer.all(n = 12, exact.n = FALSE)

#Get hue names from the pallete:
brewer.pal(3,'Set1')
brewer.pal(5,'Set1')
brewer.pal(9,'Set1')
brewer.pal(9,'Set1')[c(1,5,7)]

# Or get 30 hues from the Blues palette:
blues <- colorRampPalette(brewer.pal(9,'Blues'))
blues(25)
# blues(10)
# blues(30)

# use predefined brewer palette:
p3 + scale_colour_brewer(palette = "YlOrBr")
p3 + scale_colour_manual(values = brewer.pal(9,'YlOrBr')[c(2:9)])
p3 + scale_color_viridis_d()
# p3 + scale_color_viridis_b()

# similar:
p3 + scale_colour_manual(values = colorRampPalette(brewer.pal(9,'Blues'))(8))

# Or:
p3 + scale_colour_manual(values = brewer.pal(9,'Set1')[c(1:8)])

# Remove the legend with:
p3 + guides(color = FALSE)

# Practice
# 
# On the following graph:

cw <- ggplot(data = movies, aes(x = budget, y = p10)) + 
  labs(x= "Budget", y = "Percentage of 10s", title = "Budget vs Percent of highest mark received") + 
  geom_point()
cw

# map variable mpaa aesthetic color. Configure colors of the points according to 
# your preferences.



# ----------
# In some examples/applications we may have seen usage of scale function with suffix 
# identity. It is used whenever dataset consists actual information about how to
# configure given aesthetic, let's take a look:

movies$colour <- plyr::mapvalues(movies$mpaa, 
                                 c("R", "PG-13", '', 'PG'), 
                                 c("#009E73", "#D55E00", "#0072B2", "#000000"))

ggplot(data = movies, aes(x = budget, y = WR, color = colour)) +
  geom_point() +
  scale_color_identity()

# Please notice, that legend is not produced. Nevertheless, sometimes this func-
# tionality is useful.
 

# ------------
# another interesting example
# https://teunbrand.github.io/ggh4x/

library(ggh4x)
library(scales)

df <- transform(
  iris, 
  Nester = ifelse(Species == "setosa", "Short Leaves", "Long Leaves")
)

# Basic plot
g <- ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  theme_classic() +
  theme(strip.background = element_blank())
g

g <- g + 
  geom_point(aes(SW = Sepal.Width),
             data = ~ subset(., Species == "setosa")) +
  geom_point(aes(PL = Petal.Length),
             data = ~ subset(., Species == "versicolor")) +
  geom_point(aes(PW = Petal.Width),
             data = ~ subset(., Species == "virginica"))

g <- g +
  scale_colour_multi(
    aesthetics = c("SW", "PL", "PW"),
    name = list("Blue", "Pink", "Orange"),
    colours = list(
      brewer_pal(palette = "YlGnBu")(6),
      brewer_pal(palette = "RdPu")(6),
      brewer_pal(palette = "YlOrRd")(6)
    ),
    guide = guide_colorbar(barheight = unit(50, "pt"))
  )
g

# We can make a facet wherein duplicated strip labels are merged into one strip
g <- g + 
  facet_nested(~ Nester + Species, scales = "free",
               nest_line = TRUE)

# Like we did for colours, we might also want to set position scales for every
# panel individually. We set these in the same order the facets appear in.
position_scales <- list(
  scale_x_reverse(guide = "axis_minor"),
  scale_x_continuous(labels = dollar, guide = "axis_truncated"),
  scale_x_continuous(breaks = c(3, 4), expand = c(0,0))
)

# Adding the list of scales to the plot
g <- g + facetted_pos_scales(x = position_scales)

# Setting the sizes of panels individually
size <- 2 / (1 + sqrt(5))
g <- g + force_panelsizes(cols = c(1, size, size ^ 2), respect = TRUE)
g

# 3. Size scale ----------------------------------------------------------------
 
# The point size as an aesthetic for some kind of variable is quite often used
# in practice. We use the `scale_size()` function to control the legend size. 
# Apart from arguments common to all `scale` functions, the `scale_size()` 
# function has also two unique parameters: `range` and `trans`. 
# First let's build an example point plot for two nominal variables. 
# Such plots typically involve overlapping points. We can use this feature:
p4 <- ggplot(movies,aes( y = p10, 
                         x = rating, 
                         size = budget/10^6)) + geom_point(alpha=0.4)
p4

# Now let's call the `scale_size()` function and its basic params:
p4 + scale_size(name = 'Budget\n[mln]', breaks = seq(0, 200, by = 25))

# Now, with `range =` we describe the ratio between smallest and largest points:
p4 + scale_size(range = c(1, 12))

# The `trans =` argument, on the other hand, allows to choose a method which 
# is used to transform the numerical values into the point sizes. 
# In this way we can manipulate the relative differences between the point sizes,
# e.g. we can use a `log` or `sqrt` function to flatten the differences between 
# point sizes or magnify it with the `exp` function. 
# The built-in arguments are: `"asn"`, `"atanh"`, `"boxcox"`, `"exp"`, 
# `"identity"`, `"log"`, `"log10"`, `"log1p"`, `"log2"`, `"logit"`, 
# `"probability"`, `"probit"`, `"reciprocal"`, `"reverse"` and `"sqrt"`.
p4 <- ggplot(movies,aes( y = p10, 
                         x = rating, 
                         size = budget/10^6)) + geom_point(alpha=0.4)
p4

# Examples:
# Use the exponential scale to magnify the contrast between small and large values.
p4 + scale_size(trans = "exp")

# Use the exponential scale to flatten the contrast between small and large values.
p4 + scale_size(trans = "log")

# 4. Shape scale ---------------------------------------------------------------

p4 + 
  aes(size = NULL, shape = mpaa) +
  scale_shape_manual(values = c(5, 10, 19, 21))

# 5. "Gap plot" ----------------------------------------------------------------

# Sometimes users needs omit some parts of axis, because there is some gap in data
# or simply some range of values are relatively unimportant for the distribution,
# For obvious reason (arbitrariness of such operation) such an option is not 
# possible in ggplot2. However recently a package with such functionality was 
# published. 

# install.packages('ggbreak')
library(ggbreak)

ggplot(data = movies, aes(x = rank, y = votes*p10/100, color = decade)) +
  geom_point() +
  scale_x_break(breaks = c(100, 150))

# 6. Multiple gradient scales --------------------------------------------------

# Basic plot
g <- ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  theme_classic() +
  theme(strip.background = element_blank())
g

g <- g + 
  geom_point(aes(SW = Sepal.Width),
             data = ~ subset(., Species == "setosa")) +
  geom_point(aes(PL = Petal.Length),
             data = ~ subset(., Species == "versicolor")) +
  geom_point(aes(PW = Petal.Width),
             data = ~ subset(., Species == "virginica"))

g +
  scale_colour_multi(
    aesthetics = c("SW", "PL", "PW"),
    name = list("Blue", "Pink", "Orange"),
    colours = list(
      brewer_pal(palette = "YlGnBu")(6),
      brewer_pal(palette = "RdPu")(6),
      brewer_pal(palette = "YlOrRd")(6)
    ),
    guide = guide_colorbar(barheight = unit(50, "pt"))
  )



# Exercises -------------------------------------------------------------------

# 1. Prepare a scatter plot which shows the dependence of movie duration on its 
# position in the ranking. The point sizes should correspond to movie budget. 
# The legend should present point sizes for 50, 100 and 150 mln USD. 
# The horizontal axis ticks should present values which are multiples of 25, while 
# vertical - multiples of 10.

# 2. Prepare a scatter plot which shows the dependence of movie budget vs the 
# percentage of '10's received. Add plot and axis titles and axis labels. Color 
# of the points should express movie duration and since it's cold outside, the 
# color palette should contain warm colors. Remove the scientific notation by 
# adjusting the axis scale.

# 3. Use the result of exercise 1 (i.e. the plot of movie length vs its rank). 
# Use the `themes()` function to change settings of the font and the legend background. 
# Use the appropriate documentation, either materials from previous labs, 
# ggplot2 docs, `themes()` function vignettes, or online help.







# Homework ---------------------------------------------------------------------

# Find interesting dataset and prepare short report (in R Markdown) which will 
# consists:
# * short description of the dataset,
# * 3 scatterplots which will present interesting relationships between variables,
# * brief comments which describes obtained results.
# 
# Then, edit theme of the graphs and all scales of the graph and prepare 
# publication-ready plots. 