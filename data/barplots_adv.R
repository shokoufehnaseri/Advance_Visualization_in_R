#### Case Study
library(tidyverse)
# a)

df <- data.frame(city_size=c(1L,2L,3L,4L,5L,6L),
                avg_exp=c(1827,1444,1400,1285,1183,1039),
                med_exp=c(1500,1244,1168,1110,1002,863)
)

df$city_size <- factor(df$city_size, labels=c(">=500K","200K-499K", "100K-199K","20K-99K","<20K","country"))


# b)
df2 <- data.frame(city_size=c(1L,2L,3L,4L,5L,6L),
                  year=c(rep(2010,6),rep(2014,6)),
                  avg_exp=c(c(1827,1444,1400,1285,1183,1039)-166,c(1827,1444,1400,1285,1183,1039)),
                  med_wyd=c(c(1500,1244,1168,1110,1002,863)-187,c(1500,1244,1168,1110,1002,863)-44)
) %>% 
  mutate(
    city_size = factor(city_size, levels=1:6, 
                  labels=c(">=500K","200K-499K", "100K-199K","20K-99K","<20K","country")),
    year = factor(year,levels=c(2010,2014))
  )

# c)

df3 <- data.frame(period=c(1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,12L),
                revenue=c(6829,7508,9283,8725,3227,4902,3416,3039,825,7458,2215,4443),
                costs=c(-4123,-5659,-6986,-5682,-2452,-2485,-2095,-2103,-616,-6526,-1542,-3151)
) %>% 
  mutate(
    period = factor(period,levels=1:12, 
                     labels=c('Jan', 'Feb', 'marzec', 'Apr', 'May', 'Jun','Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
    profit = revenue + costs,
    margin = round(profit/revenue,4)
  )

dane <- df3 %>% gather(Type, Cashflow, revenue:costs)

dane.r <- df3 %>% filter(Type =='revenue')
dane.c <- df3 %>% filter(Type =='costs')
dane.p <- df3 %>% filter(Type =='profit')
dane.m <- df3 %>% filter(Type =='margin')
