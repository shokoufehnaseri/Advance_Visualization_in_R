# ----------------------------------------------------- #
#             Advanced Visualisation in R               #
#                   Piotr Ćwiakowski                    #
#           Faculty of Economic Sciences UW             #
#                 Barplots in ggplot2                   #
# ----------------------------------------------------- #

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

setwd('...')

# Read data
rs <- read.csv2('data/responses.csv', header = TRUE, sep = ",", dec = ".") 
# Data description: https://www.kaggle.com/miroslavsabo/young-people-survey

# 1. Introduction --------------------------------------------------------------

# Barplots are ones among the most basic types of plots in data visualization 
# packages. Despite the fact that they are not the most efficient in terms of 
# data presentation (one could often get away with just a nice table), 
# they are very often used in numerous works. They are also a great opportunity
# to continue our journey with ggplot2.

# 2. Basic plot ----------------------------------------------------------------

# In our examples we will use the 'responses' database which contains results 
# of a student poll in which students' hobbies, world-views, etc are investigated. 
# Let's start with the age distribution in the investigated population:
ggplot(data = rs, aes(x = Age)) + 
  geom_bar() 

ggplot(data = rs, aes(x = Age)) + 
  geom_bar() +
  scale_x_continuous(breaks=15:30)

ggplot(data = rs, aes(x = Age)) + 
  geom_bar() +
  scale_x_continuous(breaks=15:30) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 160))

# Accessing y variable:
ggplot(data = rs, aes(x = Age)) + 
  geom_bar(stat = 'count', aes(y = ..count..)) 

ggplot(data = rs, aes(x = Age)) + 
  geom_bar(stat = 'count', aes(y = after_stat(count))) 

# Percentage plot
ggplot(data = rs, aes(x = Age)) + 
  geom_bar(stat = 'count', aes(y = after_stat(count/sum(count)))) +
  scale_y_continuous(labels = percent)

ggplot(data = rs, aes(x = Age)) + 
  geom_bar(stat = 'count', aes(y = after_stat(prop))) +
  scale_y_continuous(labels = percent)

# In many organizations, ways of work with margins between axes and labels might
# be different. Let's see how we can e.g. remove the margins. Additionally, 
# we will set larger ranges for `x` and `y` axes:
ggplot(data = rs, aes(x = Age)) + 
  geom_bar() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 200)) +
  scale_x_continuous(expand = c(0,0), limits = c(14, 31),
                     breaks = seq(15, 30)) +
  theme_classic()

# To make the plot more colorful, we need to use the `fill` parameter 
# either inside the `aes()` function or outside of it:
ggplot() + 
  geom_bar(data = rs, aes(x = Age), fill = 'skyblue4')

ggplot() + 
  geom_bar(data = rs, aes(x = Age, fill = Gender))

# 3. Editing the plot parameters -----------------------------------------------

# The `position` parameter allows us to change the position of the bars:
ggplot(data = rs, aes(x = Age, fill = Gender)) +
  geom_bar(position = "stack")

ggplot(data = rs, aes(x = Age, fill = Gender)) +
  geom_bar(position = "dodge", stat = 'count')

# ---------------- Digression 
# Above graph does not look good - comparing shapes of the subsamples distribu-
# tion is hard (too many, too thin stripes). Let's try to present the same infor
# mation with less ink. We will use stat_count function for that.

# First, let's achieve same graph with new function
ggplot(data = rs, aes(x = Age, fill = Gender)) +
  stat_count(geom = 'bar', position = 'dodge')

# Now let's change geometry to line plot
ggplot(data = rs, aes(x = Age, color = Gender)) +
  stat_count(geom = 'line', position = 'dodge', size = 1.2)

ggplot(data = rs, aes(x = Age, color = Gender)) +
  stat_count(geom = 'line', position = position_dodge(width = .2)) +
  scale_x_continuous(breaks = 15:30)

# The best option - geom step:
ggplot(data = rs, aes(x = Age, color = Gender)) +
  stat_count(geom = 'step', position = 'dodge', size = 1)

ggplot(data = rs, aes(x = Age, color = Gender)) +
  stat_count(geom = 'step', position = position_dodge(.3), size=1)

# --------------------- End of digression

# Last position we may use to present bars is fill:
ggplot(data = rs, aes(x = Age, fill = Gender)) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = seq(15, 30))

ggplot(data = rs, aes(x = Age, fill = Gender)) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = unique(rs$Age))

# The `width` parameter allows to set an optimal bar width:
ggplot(data = rs, aes(x = Age, fill = Gender)) +
  geom_bar(position = "stack", width = 1, color = 'grey78')

ggplot(data = rs, aes(x = Age, fill = Gender)) +
  geom_bar(position = "stack", width = 0.5)

# The `show.legend` parameter allows to either display or remove the legend. 
# The `FALSE` value hides the legend, while `TRUE` forces the legend to display:
ggplot(data = rs, aes(x = Age, fill = Gender)) + 
  geom_bar(position = "dodge", width = 1, show.legend = FALSE)

# Sometimes we want to present our data in the aggregated form -- to get averages,
# medians, observation numbers, etc. To do so we have to slightly tweak
# the `geom_bar` function with the `stat` parameter. Assigning to it the `identity`
# value results in the fact that the `geom_bar` function now will not count 
# the observations but will directly visualize values given in the `y` variable.

# But let's analyze an example first. We should slightly transform our dataset 
# and then prep a plot. We transform our data to group all the people according
# to their age and then to calculate mean preferences with respect to doing 
# shopping in large malls.

# Group the data:
rs2 <- rs %>% 
  group_by(Age) %>% 
  summarise(N = n(),
            Mean.shopping = mean(Shopping.centres),
            Mean.saving = mean(Finances))
rs2

# ggplot(data = rs2, aes(x = Age, y = N)) +
#   geom_bar()

# If we use this data for geom_bar it does not work:
ggplot(data = rs2, aes(x = Age, y = N)) +
  geom_bar(stat = 'identity')

# Basically, it is geom_bar, but with different default for stat:
ggplot(data = rs2, aes(x = Age, y = N)) +
  geom_col()

# Let's plot the average responses of preferences toward shopping in subgroups
# (and add some formatting:
ggplot(data = rs2, aes(x = Age, y = Mean.shopping)) +
  geom_col(width = 0.5) +
  labs(title = 'Do you like shopping in large malls?',
       subtitle = '1 - I don\'t like; 5 - I like very much',
       caption = 'Source: based on responses')

# Let's see what will happen, if we map continuous variable to fill aesthetic:
ggplot(data = rs2, aes(x = Age, y = Mean.shopping, fill = Mean.saving)) +
  geom_col(width = 0.5) +
  labs(title = 'Do you like shopping in large malls?',
       subtitle = '1 - I don\'t like; 5 - I like very much',
       caption = 'Source: based on responses')

# 4. Formatting the plot -------------------------------------------------------

# As a reminder, let's look at some different variants of plot formatting:

# Change the fill:
ggplot(data = rs, aes(x = Age)) +
  geom_bar(position = 'dodge', width = 1, color = 'black', fill = 'red') +
  labs(title = 'Age distribution in statistical sample',
       x = 'Age',
       y = 'Population') +
  theme_minimal()

# Change colors and legend:
ggplot(data = rs, aes(x = Age, fill = factor(Age))) +
  geom_bar(position = 'dodge') +
  labs(title = 'Age distribution in statistical sample',
       x = 'Age',
       y = 'Population') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE))

# 5. Changing the order of axis labels -----------------------------------------

# Let's assume that we have a plot from which we want to remove outliers, e.g.:
ggplot(data = rs, aes(x = factor(Number.of.siblings))) +
  geom_bar(position = 'stack', color = 'black') + 
  theme_minimal() +
  ggtitle('Distribution of number of siblings') +
  xlab('Number of siblings') +
  ylab('Population')

# We can remove the outliers by a proper definiton of the `limits` function:
ggplot(data = rs, aes(x = factor(Number.of.siblings))) +
  geom_bar(position = 'stack', color = 'black') + 
  theme_minimal() +
  ggtitle('Distribution of number of siblings') +
  xlab('Number of siblings') +
  ylab('Population') +
  scale_x_discrete(limits = c('0','1','2','3'))

# Swap the `x` axis values:
ggplot(data = rs, aes(x = factor(Number.of.siblings))) +
  geom_bar(position = 'stack', color = 'black') +
  theme_minimal() +
  ggtitle('Distribution of number of siblings') +
  xlab('Number of siblings') +
  ylab('Population') +
  scale_x_discrete(limits = c('2','1','0', '3'))

# Now lets display the number of siblings vs the frequency of its occurence:
rs %>% 
    group_by(Number.of.siblings) %>% 
    summarise(N = n()) %>% 
    mutate(Number.of.siblings = factor(Number.of.siblings)) -> result
  
result$Number.of.siblings2 <- reorder(result$Number.of.siblings, result$N)
result$Number.of.siblings3 <- reorder(result$Number.of.siblings, desc(result$N))
  
ggplot(data = result, aes(x = Number.of.siblings2, y = N)) +
    geom_bar(position = 'stack', color = 'black', stat = 'identity') +
    theme_minimal()
  
ggplot(data = result, aes(x = Number.of.siblings3, y = N)) +
    geom_bar(position = 'stack', color = 'black', stat = 'identity') +
    theme_minimal() 

# 6. Adding labels to geom_bar -------------------------------------------------
  
# As you probably remember, labels can be added with the `geom_text` geometry. 
# The `y` aesthetic and `vjust` parameter can be use to set the height of a label.
# If the `vjust` value is positive, the label will appear below the edge of a bar.
# In other cases, the labels will appear above the edge.
rs3 <- rs %>% 
  group_by(Age) %>% 
  summarise(number = n())

# ggplot(data = rs3, aes(x = Age, y = number)) +
#   geom_bar(stat = 'identity', fill = 'darkgreen', color = 'darkgreen') +
#   theme_minimal() +
#   geom_text(aes(label = number), vjust = -.4)

# Labels outside bars:
ggplot(data = rs3, aes(x = Age, y = number)) +
  geom_bar(stat = 'identity', fill = 'darkgreen', color = 'darkgreen') +
  theme_minimal() +
  geom_text(aes(label=number), vjust = -.4, color = 'red', size = 3.5)

# Labels inside bars:
ggplot(data = rs3, aes(x = Age, y = number)) + 
  geom_bar(stat = 'identity', fill = 'darkgreen', color = 'darkgreen') +
  theme_minimal() +
  geom_text(aes(label = number), vjust = 1.2, color = 'white', size = 3.5)

ggplot(data = rs, aes(x = Age)) +
  geom_bar(stat = 'count', fill = 'darkgreen', color = 'darkgreen',
           aes(y = ..count..)) +
  theme_minimal() +
  geom_text(aes(label=..count..), stat = 'count', color = 'red', size = 3.5, vjust=-.3)

# A more complicated case is the complex plot, let's remind the example:
rs4 <- rs %>% 
  group_by(Age, Gender) %>% 
  summarise(number = length(Age))

# The first issue. Whether more aggregated barplot can we plot based on that data?
ggplot(data = rs4, aes(x = Age, y = number, fill = Gender)) +
  geom_bar(stat = 'identity', color = 'darkgreen') +
  theme_minimal() +
  geom_text(aes(label=number), position = 'stack')

# Second issue - let's put labels on the dodge barplot:
ggplot(data = rs4, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "dodge", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal()

# To name columns on a plot which has multiple columns near each other, 
# we need to add the `position_dodge()` parameter with  the column `width`
# value to the `geom_text` function. Thanks to that, the labels will be 
# positioned above the appropriate colums:
ggplot(data = rs4, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "dodge", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  geom_text(aes(label = number), vjust = -.5, position = position_dodge(.5),
            size = 2.5)

# Yet another challenge is the stacked bar plot:
ggplot(data = rs4, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  scale_fill_manual(values = c('grey78', 'khaki')) 

# solution:
ggplot(data = rs4, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  scale_fill_manual(values = c('grey78', 'khaki')) +
  geom_text(aes(label = number), position = 'stack')

# To add column labels we need to do two things:
#
#  * set the `position` parameter to the `position_stack` value,
#  * count the cumulated sums to be displayed on the plot.

final <- rs4 %>% 
  arrange(Age, desc(Gender)) %>% 
  group_by(Age) %>% 
  mutate(label_sum = cumsum(number)) 

ggplot(data = final, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  geom_text(aes(label = label_sum), position = position_stack(),
            vjust = -.5, color = "black", size = 3.5) +
  scale_x_discrete(limits = c('17','18','19','20','21','22','23','24'))

# We could also leave the partial sums and put them in the middle of bars:
final2 <- final %>% 
  mutate(label_sum2 = label_sum*.5)

ggplot(data = final2, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  geom_label(aes(y = label_sum2, label = number), fill = 'white', position = position_stack(),
            vjust = 0.5, color = "black", size = 3) +
  scale_x_discrete(limits = c('17','18','19','20','21','22','23','24')) +
  geom_text(aes(y = label_sum, label = label_sum),
            data = final2 %>% filter(Gender == 'female'), vjust=-.4)

# 7. Horizontal plot -----------------------------------------------------------

# As a bonus we present a simple trick to flip your barplot:
ggplot(data = rs, aes(x = Age, fill = Gender)) + 
  geom_bar(position = "stack", width = 0.5) +
  coord_flip()

ggplot(data = rs, aes(y = Age, fill = Gender)) + 
  geom_bar(position = "stack", width = 0.5)

# 8. Pie plot ------------------------------------------------------------------

# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization

df <- data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50)
)
head(df)

ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)

ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5))

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(), 
    plot.title = element_text(size=14, face="bold")
  )

ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) + 
  blank_theme

# Different palletes:
ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  blank_theme

# More info:
# https://r-charts.com/part-whole/pie-chart-ggplot2/

# New package:
library(ggpie)

ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "circle",
      label_size = 4, label_pos = "out")

# More information:
# https://cran.r-project.org/web/packages/ggpie/vignettes/ggpie.html


# 9. Bar plot and scatter plot -------------------------------------------------
source('4_barplots_adv.R')

head(df3)

ggplot(data = df3, aes(x = period)) +
  theme_bw() +
  geom_col(aes(y = revenue, fill = 'Revenue')) +
  geom_text(aes(label = format(round(revenue), big.mark = " "), y = revenue),
            size = 3, vjust = -.3) +
  geom_col(aes(y = costs, fill='Costs')) +
  geom_text(aes(label = format(round(costs), big.mark = " "), y=costs),
            size = 3, vjust = .9) +
  geom_point(aes(y = profit, colour = 'Profit')) +
  geom_text(aes(label = format(round(profit), big.mark = " "), y = profit),
            size = 3, vjust = -.7) +
  scale_color_manual(values = 'black') +
  scale_fill_manual(values=c("#EC7014", "#FEC44F")) +
  labs(title="Chart 1. Revenues, Costs and profits in 2012",
       y="Cashflows", x=NULL, fill=NULL, color=NULL) +
  scale_y_continuous(breaks = seq(-1e4, 1e4, by = 2500),
                     labels = format(seq(-1e4, 1e4, by = 2500), big.mark=" ")) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        )

# 10. Bullet chart -------------------------------------------------------------

# 10.1 Classic variant

# A bullet chart is a rich variant of the simple bar chart. 


ggplot(data=df, aes(x=city_size)) + 
  theme_minimal()+ 
  geom_col(aes(y=avg_exp, fill = "average\nexpenditure"), 
           color="black", width=.8) + 
  geom_col(aes(y=med_exp, fill="median\nexpenditure"), width=.4, colour="black") + 
  theme(title = element_text(size=8), 
        legend.position =   "top",
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        plot.title= element_text(size=20),
        axis.title= element_text(size=12),
        axis.ticks=element_blank()
        ) + 
  labs(x = NULL, y = "Expenditures [PLN]", fill=NULL,
       title = "Average/median household expenditure v. size of city") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 2000)) + 
  geom_text(aes(label = round(avg_exp, 0), y = avg_exp), size=4,vjust=-.5,color="black") + 
  geom_text(aes(label = round(med_exp, 0), y = med_exp), size=4,vjust=-.5,color="white") + 
  scale_fill_manual(values=c( 'darkgreen', "green"))

# Source: https://themockup.blog/posts/2020-11-29-bullet-chart-variants-in-r/

# install.packages("devtools")
devtools::install_github("ACDIVOCATech/bulletchartr")

library(bulletchartr)
data("bc_ex")
bullet_chart(dataframe = bc_ex)

# 9.2 bullet-like graph

# Source:

incidents.pct <- data.frame(
  measure = c(
    "Total Events (%)",
    "Security Events (%)",
    "Filtered (%)",
    "Tickets (%)"
  ),
  high = c(100, 100, 100, 100),
  mean = c(45, 40, 50, 30),
  low = c(25, 20, 10, 5),
  target = c(55, 40, 45, 35),
  value = c(50, 45, 60, 25)
)

ggplot(incidents.pct) +
  geom_col(aes(measure, high, fill = "high"), width=0.5, alpha=0.2) +
  geom_col(aes(measure, mean, fill = "mean"), width=0.5, alpha=0.2) +
  geom_col(aes(measure, low, fill = "low"), width=0.5, alpha=0.2) +        
  geom_point(aes(measure, target), colour="red", size=2.5) +
  scale_fill_manual(name = "Legend",
                    values = c("high" = "goldenrod2", 
                               "mean" = "goldenrod3", 
                               "low" = "goldenrod4"),
                    breaks = c("high", "mean", "low")) 

# Question: How to create legend for the points?
ggplot(incidents.pct) +
  geom_col(aes(measure, high, fill = "high"), width=0.5, alpha=0.2) +
  geom_col(aes(measure, mean, fill = "mean"), width=0.5, alpha=0.2) +
  geom_col(aes(measure, low, fill = "low"), width=0.5, alpha=0.2) +        
  geom_point(aes(measure, target, colour="Current\nlevel of KPI"), size=2.5,
             alpha = 1) +
  scale_fill_manual(name = "Legend",
                    values = c("high" = "goldenrod2", 
                               "mean" = "goldenrod3", 
                               "low" = "goldenrod4"),
                    breaks = c("high", "mean", "low")) 

ggplot(incidents.pct) +
  geom_point(aes(measure, target, fill="target"), shape = 21) +
  geom_col(aes(measure, high, fill = "high"), width=0.5, alpha=0.4) +
  geom_col(aes(measure, mean, fill = "mean"), width=0.5, alpha=0.4) +
  geom_col(aes(measure, low, fill = "low"), width=0.5, alpha=0.4) +        
  geom_col(aes(measure, target, fill="KPI"), 
             alpha = .7, width = .2) +
  scale_fill_manual(name = "Legend",
                    values = c("high" = "goldenrod2", 
                               "mean" = "goldenrod3", 
                               "low" = "goldenrod4",
                               "KPI" = 'red',
                               "target" = 'black'),
                    breaks = c("high", "mean", "low", 'KPI', "target"),
                    labels = c('low', 'mean', 'high', 'current',  "target"))+
  guides(fill = guide_legend(override.aes = list(shape = 15)))

# 11. diverging bar plot

# Source: https://albert-rapp.de/posts/ggplot2-tips/22_diverging_bar_plot/22_diverging_bar_plot



# Exercises --------------------------------------------------------------------

# 1. Using data from file `happy2015.csv` calculate average happiness scores for 
# each region and plot them on a bar plot. Color the bars according to the region 
# and hide the legend. Put the labels with the information about the mean values 
# slightly above the bars. Add axes titles. Remember to aggregate data first over
# the regions.



# 2. Using data from file `responses.csv` prepare a plot on which you will show 
# the number of people with fear of public speaking. Use colors to present distri-
# bution of this variable across genders (preferably with stacked bars). Put the 
# plot legend on the left hand side of the plot. Change the general plot color 
# to shades of blue. Add plot and axis titles, add caption with appropriate x-values 
# description.

# 3. Using the data from file `responses.csv` show the respondents' attitude toward 
# savings (variable Finances) (show as colors) vs the education level. Place the 
# bars next to each other, add labels mirroring the bar height (hint: create a 
# new variable for number of respondents in each group). Change the colors of 
# canvas and fill, as well as the theme of the plot. Label the axes, plot and legend.

# Additionally: change the order of column display (sort them with respect to the
# order of the education levels), change the positioning of columns to 'stacked', 
# and position the labels in the middle of columns.


# Homework ---------------------------------------------------------------------

# Find interesting dataset and prepare short report (in R Markdown) which will 
# consists:
# * short description of the dataset,
# * 4 barplots which will present interesting relationships between variables,
# * brief comments which describes obtained results.
# 
# Then, edit theme of the graphs and all scales of the graph and prepare 
# publication-ready plots. 