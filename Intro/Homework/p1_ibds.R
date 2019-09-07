install.packages("tidyverse")
library(tidyverse)

# 3.2.4

# 1

ggplot(data = mpg)

# You see nothing because just an empty canvas had been created by ggplot, but nothing has been "drawn"
# yet on this canvas

# 2

mpg 

# 234 rows and 11 columns

# 3

?mpg()

# The drv variable describes in which type of driving mechanism the car has

# 4

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))


# 5

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

# Both variables are categorical, which means a scatterplot is non informative

# 3.3.1

# 1

# The color argument should be outside the parantheses (seperated by a comma) of the aes verb

# 3

# Map year to color
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = year))

# Map year to shape (error)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = year))

# continues variable cannot be mapped to shape

# Map year to size (not very informative..)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = year))

# The shape and size aesthetics are mostly of use in categorical variables, whereas color can also
# be informative in continuous data

# 4

# if multiple asthetics are called they will if possible be applied simultaneously 

# 6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

# The datapoints which meet this condition are colored blue and the ones that fail this condition are colored red


# 3.5.1

# 2

# It means that no data lies withing these ranges

# 4

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# An advantage is the visibility of the distribution of the data per category, however using coloring aesthetics 
# is better to paint the bigger picture so to say

# 3.6.1

# 6

# Graph 1
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Graph 2

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(group = drv), se = FALSE)

# Graph 3

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Graph 4

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(group = drv, color = drv)) +
  geom_smooth(se = FALSE)

# Graph 5

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv))
# 3.7.1

# 2

diamonds

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_col(mapping = aes(x = cut, y = carat))

# geom_col plots a discrete x variable against a continuous y variable whereas geom_bar creates a "new""
# count variable


# 3.8.1 
# question 3.8 what does the count variable on the y-axis represent in the position = "fill" example

# 2

# The problem with the graph is overplotting, many datapoints overlap. This problem can be solved by using geom_jitter() like so:

ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = cty, y = hwy))


# 3.9.1

# 2

# The labs() verb allows you to edit the x and y labels and add titles, subtiltes etc into your plots


# Chapter 5

library(nycflights13)

nycflights13::flights

View(flights)

# 1.1

filter(flights, arr_delay >= 120)

# 1.2

filter(flights, (dest == "IAH" | dest == "HOU"))

# 1.3

filter(flights, (carrier %in% c("UA","AA","DL")))

# 1.4

filter(flights, (month %in% c(6,7,8)))

# 1.5

filter(flights, (arr_delay > 120 & dep_delay == 0))

# 1.6

#filter(flights, (arr_delay >= 60 & )
       
# 1.7

filter(flights, (dep_time %in% c(seq(0,6,1))))

# 5.3.1 

# 1

?arrange

# 2

View(arrange(flights, desc(dep_delay)))

# 5.4.1

# 1

select(flights, dep_time, dep_delay, arr_time, arr_delay)

select(flights, -(year:day),-sched_dep_time, -sched_arr_time, -(carrier:time_hour))

select(flights, starts_with("arr_"), starts_with("dep_"))

select(flights, ends_with("_time"), ends_with("_delay"), -air_time, -sched_dep_time, -sched_arr_time)

# 2

select(flights, day, day)

# the column is selected only one time

# 5.5.2

View(select(flights, dep_time, sched_dep_time))


# 1 

transmute(flights, 
  dep_hours = dep_time %/% 100,
  dep_minutes = dep_time %% 100,
  dep_time_minutes = dep_hours * 60 + dep_minutes,
  sched_dep_hours = sched_dep_time %/% 100,
  sched_dep_minutes = sched_dep_time %% 100,
  sched_dep_time_minutes = (sched_dep_hours * 60 + sched_dep_minutes))
  

# I expect to see an incorrect number due to the fact that the times are hours and now they are  treated as integers
# from 0 to 100 insted of 0 - 60. To fix this rhe times need to be transmuted to minutes

# 5.7.1

# 4

not_canceled <- flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay)) 

count_not_canceled <- not_canceled %>%
                        group_by(dest) %>%
                        summarise(n = n())


popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
                     
# 5.7.1

# For each destination, compute the total minutes of delay. 

delay_per_dest <- not_canceled %>%
  group_by(dest) %>%
  summarise(
    delay = sum(arr_delay) + sum(dep_delay))

# For each flight, compute the proportion of the total delay for its destination.

not_canceled %>%
  group_by(dest) %>%
  mutate(total_delay = (arr_delay + dep_delay) / (sum(arr_delay) + sum(dep_delay))) 

# Chapter 7

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 10) +
  coord_cartesian(ylim = c(0,100), xlim = c(1000,2000))

diamonds %>%
  count(cut_width(price, 1000))

# There are certain price categories which don't include diamonds

# 7.5.2.1

# 2

# calc mean delay per destinatin

mean_delay_per_dest <- not_canceled %>%
  group_by(dest) %>%
  summarise(
    delay_dest = mean(arr_delay) + mean(dep_delay))

mean_delay_per_month <- not_canceled %>%
  group_by(month) %>%
  summarise(
    delay_month = mean(arr_delay) + mean(dep_delay))
  
# 10.5

# 1
# the datatype of the columns is printed, and not all columns are printed ofcourse
# you can also execute class() in order to check 

# 2 

# data frames are more work in terms of indexing. A tibble allows you select and manipulate your data in 
# a fast and intuïtive way.



#11.2

#1

# read_delim(file, delim)

# 3 

# The most important arguments for read_fwf() are fwf_positions() or fwf_widths()

# 4

read_csv("a,b,c\n1,2,3\n4,5,6")

# A part of the data is not shown in the tibble because the amount of columns do not match the amount of rows

read_csv("a,b,c\n1,2\n1,2,3,4")

# the missing data is replaced by NA

read_csv("a,b\n\"1")

# too many forwardslashes

read_csv("a,b\n1,2\na,b")

# \n is read as next row

read_csv("a;b\n1;3")

# wrong delimiter

library(tidyverse)

# 11.4.5

# 5

# read_csv2 reads semicolon seperated files instead of comma delimited files

# 7
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, "%B %d, %Y")
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4[1], "%B %d (%Y)")
parse_date(d4[2], "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")

library(hms)
parse_time(t1, "%H%M")
parse_time(t2)
# 

