# ----------------------------------------------------- #
#             Advanced Visualisation in R               #
#                   Piotr Ćwiakowski                    #
#           Faculty of Economic Sciences UW             #
#               Introduction to ggplot2                 #
# ----------------------------------------------------- #


# Preliminaries -----------------------------------------------------------

# libraries
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')

library(ggplot2)
library(dplyr)
library(tidyr)

# read datasets
boston <- read.csv('Data/boston.csv', header = T, dec = ',', sep = ';')
movies <- read.csv('Data/top250.csv', sep = ';', dec = ',')

# Datasets description

# 1. Boston Housing

# Source and metadata: 
# http://archive.ics.uci.edu/ml/machine-learning-databases/housing/?C=D;O=A

# Variable | Description
# CRIM     | per capita crime rate by town
# ZN       | proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS    | proportion of non-retail business acres per town
# CHAS     | Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# NOX      | nitric oxides concentration (parts per 10 million)
# RM       | average number of rooms per dwelling
# AGE      | proportion of owner-occupied units built prior to 1940
# DIS      | weighted distances to five Boston employment centres
# RAD      | index of accessibility to radial highways
# TAX      | full-value property-tax rate per $10,000
# PTRATIO  | pupil-teacher ratio by town
# B        | 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT    | % lower status of the population
# MEDV     | Median value of owner-occupied homes in $1000's

# 2. IMDB TOP250

# Variable        | Description of variable
# rank            | ranking on imdb
# title           | title of a film
# year            | year of production
# budget          | budget in USD
# length          | length of a film (in minutes)
# rating          | general opinion in a scale (1-10) on IMBD
# votes           | number of users which graded a film
# p1-10           | percentile share of grades (1-10) in a whole number of votes
# mpaa            | grade according to MPAA: https://en.wikipedia.org/wiki/Motion_Picture_Association_of_America_film_rating_system
# Action          | binary variable, 1 if an action film  0 in other cases
# Animation       | binary variable, 1 if an animation  0 in other cases
# Comedy          | binary variable, 1 if a comedy  0 in other cases
# Drama           | binary variable, 1 if a drama  0 in other cases
# Documentary     | binary variable, 1 if a documentary 0 in other cases
# Romance         | binary variable, 1 if a romantic film 0 in other cases
# Short           | binary variable, 1 if a short film  0 in other cases
# WR              | alternative ranking to IMDB


# 1. Creating your first plot ------------------------------------------------

# Let's plot a scatter plot to visualize dependence between the number of rooms 
# and median estate value. Notice that in ggplot2 we chain instructions by adding 
# "+" between subsequent function calls. Sounds intuitive!
ggplot(data = boston, aes(y = MEDV, x = RM)) + geom_point()

# Same as above but using different style:
ggplot(data = boston, aes(y = MEDV, x = RM)) + # constructor (data + aesthetic mappings)
  geom_point() # point plot

# It is worth to mention that we can also write:
ggplot() + geom_point(data = boston, aes(y = MEDV, x = RM))

# A possibility to declare the same arguments in different *ggplot2* functions 
# deep sense and in fact, is quite often used. When we are designing a plot, we 
# often think about a few steps in the same time, e.g about choosing data and 
# geometry type, or want to explore the possibilities becuse we are not yet sure 
# about which type of visualization will be the optimal one. So why not to allow 
# such instructions in R?

# Let us examine a few different cases:

# Variant I: The second layer is shown on the screen if the data and
# mapping are defined in the constructor
ggplot(data = boston, aes(y = MEDV, x = RM)) + 
  geom_point() +
  geom_smooth()

# Varian II: If we define our data in a layer, it's visible only
# for this layer
ggplot() + 
  geom_point(data = boston, aes(y = MEDV, x = RM)) +
  geom_smooth()

ggplot() + 
  geom_point(data = boston, aes(y = MEDV, x = RM)) +
  geom_smooth(data = boston[a$RM > 4, ], aes(y = MEDV, x = RM))
  
# In *ggplot2* each plot is treated as an object which can be assigned to a variable. 
# Let's try:

p <- ggplot(data = boston, aes(y = MEDV, x = RM))  + geom_point() # assign a plot to variable 'p'

# We do not see the plot being executed. But if we type in...
p

# ...the visualization appears. Moreover, we can stack up new options directly to 
# the variable p and we do not have to care about semantics of previous steps. 
# In such a way, we can avoid copy-pasting of frequently used code parts, as e.g., 
# the `ggplot()` instruction. We will test this later.

p + geom_smooth()
  
# Exercise

# 1. Using data from __TOP250.csv__ create a point plot which illustrates dependence 
# between the movie budget and the number of votes received. What do you think of 
# the results? Is really true, that the higher the budget the more popular movie? 




# 2. Review of aesthetics -------------------------------------------------

# At this point let us examine different aesthetics (ways of mapping variables) 
# starting with colour aesthetics. In *ggplot2* we can assign different colours 
# to canvas and fillings. The first aesthetics is called *color*, the second *fill*. 
# Since points do not have filling (unless they do), the *fill* aesthetics is not 
# available for the point plot (we'll return to this when we tackle different 
# geometries).

# 2.1. Color

# See example below:
  
# Mapping of variable CHAS (i.e. are we near the river):  
ggplot() + geom_point(data = boston, aes(y = MEDV, x = RM, colour = CHAS)) 

# Map the CHAS (near the river) variable (discrete var):
ggplot() + geom_point(data = boston, aes(y = MEDV, x = RM, colour = factor(CHAS))) 

# We can see that in this case a continuous scale is still used. This is due to 
# the fact that _CHAS_ is stored as the *Integer* type variable (type in `str(a)` 
# to see for yourself). So it might be a good idea to change the type of _CHAS_ 
# to the *factor* type (discrete and nominal). Let's try to reuse the `p` object 
# to avoid excessive copy-pasting:
a$CHAS <- as.factor(a$CHAS)

p <- ggplot(data = boston, aes(y = MEDV, x = RM))
p + geom_point(aes(color = CHAS))

# Here, R uses the default color palette. We will learn how to modify it a bit later.

# Please note that the mapping executes always inside the function `aes()`. If we 
# use the `color` argument ouside of `aes()` we can see that it works in a different 
# way: 
ggplot() + geom_point(data = boston, aes(y = MEDV, x = RM), colour = "magenta")

# We have painted all the points violet without mapping of any variable. Let's
# experiment a bit more:
ggplot() + geom_point(data = boston,  aes(y = MEDV, x = RM, color = "magenta")) 

# It is crucial to remember a difference between yields of the above instructions. 
# In the former case, *ggplot* sets the point color as given in the argument. 
# In the latter case, the quoted argument is treated as a nominal variable which 
# has but a single level --- 'magenta', and *ggplot* picks the first default color 
# from a discrete scale to map the received value. Which brings us to conclusion: 
# You always have to put variables inside of aes()

# Finally, let us examine two following possibilities:
ggplot(data = boston, aes(y = MEDV, x = RM), color = "magenta") + geom_point()

ggplot(data = boston, aes(y = MEDV, x = RM)) + geom_point(color = "magenta") 
ggplot(data = boston, aes(y = MEDV)) + geom_point(color = "magenta", aes(x = RM)) 
ggplot(data = boston) + geom_point(color = "magenta") + aes(y = MEDV, x = RM)

# The first instruction does not work. The `ggplot()` constructor 'does not know' 
# to what exactly the 'color' parameter is referring. This is why we need to 
# define this parameter at the geometry layer.

## 2.2. Shape

# It's probably easy to guess but we have to write it: The shape geometry is 
# responsible for the shape of points on the plot. Clearly, in *ggplot* there is 
# a posibility to define your own shapes, but for now we will focus only on the 
# functionality provided by default.
p + geom_point(aes(shape = CHAS))


# The shape aesthetic is not really the best one for this task due to the large 
# amount of points to be plotted. Let's try (also 'on the fly') to plot only 100 
# observations from database:
p + geom_point(data = boston[50:150,], aes(shape = CHAS))

# Still not good -- points are too small to be recognizable. So let's enlarge them:
p + geom_point(data = boston[50:150,], aes(shape = CHAS), size = 3)

# We can add some color to improve the plot look:
p + geom_point(data = boston, 
               aes(shape = CHAS, 
                   color = CHAS), 
               size = 2
)

## 2.3. Alpha

# This aesthetic is quite an interesting one. *Alpha* is responsible for controlling 
# the degree of transparency of a given object. In the following case, the more 
# visible the color, the more polluted the air is:
p + geom_point(aes(alpha = NOX))

# If we use this parameter outside of the aes() function, then we can see that the 
# points get more color-intensive as they overlap with different objects present 
# on the plot (e.g. other points). This way we can identify clusters of the most 
# frequent values -- the dominants.

# However, our data are not that dense -- let's enlarge the points to see the overlap effect.
p + geom_point(alpha = 0.2, size = 4)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.01)
 
## 2.4. Size

# Above we have used the *size* parameter several times outside of the aes() function. 
# So now let's map a variable to this aesthetic. Let's only pick 100 observations 
# for a better view:
p + geom_point(data = boston[1:100,],
               aes(size = MEDV)) # Point sizes proportional to median of the market value


## 2.5. Combining aesthetics

# Intuitively, one may assume that we could stack some more aesthetics on a single 
# plot. As a direct confirmation let us see:
p + geom_point(data = boston[1:100,],
               aes(size = MEDV, color = CHAS, alpha = NOX, shape = CHAS))

# It is worth to notice that for each aesthetic (except for x and y), a legend has 
# been created to describe the applied **scale** (for x and y aesthetics, the 
# scales are axes and ticks on them). For now, the values are default, but in 
# the following chapters we will learn how to change them.


# 3. Exporting plots ---------------------------------------------------------

# Use `ggsave()` to export *ggplot* plots to external files. The function allows 
# the user to define plot size (height and width), resolution, format, and filename
# and directory to which it should write the output. An example call:

# Write to default directory path:
ggsave(filename = 'Plot_1.png', # file name
       plot = p, # object to export
       device = 'png', # file format (png is the default)
       width = 10, # width
       height = 7.5, # height
       dpi = 500 # resolution in dots per inch
)

# 4. Labels --------------------------------------------------------------------

# Labels on the graph can be edited in a few ways, let's get to know the first 
# one:
ggplot(data = boston, aes(y = MEDV, x = RM)) + 
  geom_point() +
  labs(title = 'Interesting title',
       subtitle = 'Interesting subtitle',
       caption = 'Source: Own study.',
       x = 'Average number of rooms',
       y = 'Median Value of houses')

# 5. Assignments ---------------------------------------------------------------

# 1. Plot a point plot to illustrate percent of people from the lower class as a 
# function of weighted distance to 5 main employment places. Then, using color 
# express the median of market value of estates taken over for the observation. The 
# size of points should be related to the percent crime rate per resident. Using the 
# shape of the points, inform the viewer whether in the area the percent of non-retail 
# businesses in the area is greater than average in the dataset or not.




# 2. Is there any relationship between the year of production of a movie and its 
# duration? How this relation looks in the case of comedies and romance vs the 
# other types? Make your answer on the ground on an appropriate plot.

# Hint: There isn't one correct answer in this excercise. You may use different
# approach to answet this question.



# 3. Plot a point plot which will illustrate a dependence between the movie rank 
# and a number of '10' marks received by it (not in percent - in absolute num-
# bers). Color all the points in the hues of blue. 
# 
# For volunteers: Create a discrete variable which will set the point color accor-
# ding to the decade in which the movie has been produced. Then put this variable 
# on the plot by using aesthetic mapping.
  
# Hint: Movies in the database are ordered by the IMDB rank.
# Hint: Remember that the color argument has a different meaning outside of aes() 
# function.



# 6. Homework (for students which are not present during classes) --------------

# Find interesting dataset and prepare short report (in R Markdown) which will 
# consists:
# * short description of the dataset,
# * 3 scatterplots which will present interesting relationships between variables,
# * brief comments which describes obtained results.

# Please remember about:
# - using different aesthetics to show properly the insights from the data
# - adding smoothing line to the graphs
# - labeling graph with proper title, subtitle and description of the axis.

# The deadline for the homework is until next classes and cannot be postponed 
# under any circumstances.