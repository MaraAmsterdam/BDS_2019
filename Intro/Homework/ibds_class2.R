library(tidyverse)

#12.2.1 

# 3
table2 %>% 
  spread(type, count) 

ggplot(data = selection, aes(year, cases)) +
  geom_line(
    aes(group = country), color = "grey50")  + 
  geom_point(aes(color = country))

# 12.3 

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

# Philip Woods is represented two times in the tibble with two keys such that the key:value pair in
# this case is not unique

# 12.4.3

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "right")

#fill and extra deal with the cases when the data does not fit within the tibble

# 13.3.1
library("nycflights13")

flights %>% 
  mutate(
    id = row_number()
  ) %>% 
  select(id, everything())
# 2.1
install.packages("RPostgreSQL")
library(RPostgreSQL)
portal_db <- src_postgres(host = "pgstudio.research.sesync.org",
                          dbname = "portal", user = "student", 
                          password = "%password%")

# 14
library(stringr)

#14.2.5

# 2
?str_trim()
# removes white spaces from start and end the opposite is str_pad()

#
library("htmlwidgets")
x <- c("apple", "banana", "pear")
str_view_all(x, "an")

#3 The reg ex would look for the following string pattern: a.b.c.

# 14.3.2.1

# 1 match string "$^$"
# str_view(x, \\$\\^\\$)

# 2
words <- stringr::words
str_view_all(words, "^a")

str_view_all(words, "x$")

str_view_all(words, "^...$")

str_view_all(words, "^.......", match = TRUE)

# 14.3.3.1

#3
str_view_all(words, "^q[u,.]", match = TRUE)
View(words)
# yes q is always followed by a "u"

# 14.4.2.1

# 2.1
# starts with any character ends with any character
# matches "{followed by 1 or more characters and "}"
# 2.3 \d{4}-\d{2}-\d{2} matches double of exactly four digits, double of exactly 2 digits and double of exactly 2 digits
# 2.4 "\\\\{4}" matches "\\\\" 
 
# Chapter 15
library(forcats)

# 15.3.1
# 1
cats <- forcats::gss_cat
View(cats)
# The x labels are all on top of each other, flipping the barchart makes it more easy to read

ggplot(cats, aes(rincome)) +
  geom_bar() +
  coord_flip()

# 2
cats %>% 
  group_by(relig) %>% 
  count() %>% 
  arrange(desc(n))

# protestant is the most common religion

# 15.5.1
# 1
party <- cats %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"),
  )) 
by_party <- party %>% 
  count(year, partyid) %>% 
  group_by(year) %>% 
  mutate(prop = n/sum(n))

ggplot(by_party, aes(year, prop, colour = fct_reorder2(partyid, year, prop))) +
  geom_line() +
  labs(colour = "partyid") 

# 16.2.4: 3

library(lubridate)
library(nycflights13)
now()

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

# 16.3.4: 5
# convert hours and minutes
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

# create correct daytime tibble
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

# which day of the week has the most delays

flights_dt %>% 
  mutate(weekday = wday(dep_time)) %>% 
  group_by(weekday) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(avg_delay, .by_group = TRUE))

# 16.4.5: 4 

age <- function(d,m,y){
  dmy <- dmy(paste(d,m,y, sep = "-"))
  as.duration(today() - dmy)
  }
age(12,08,1992)

# 19.2.1: 5 
t0 <- c(1,6,4,8,NA, NA)
t1 <- c(1,6,7,8,NA,9)

check_na <- function(x){
  length(which(is.na(x)==TRUE))
}
both_na <- function(y,z) {
   
  stopifnot(length(y) == length(z)) 
    
  sum(check_na(y), check_na(z))
}
both_na(t0,t1)

# 19.4.4: 1
# ifelse will check for a condition and return a value dependent on the outcome. The return value is specified as an argument
# and will depent on the outcome of the logical test. An if statement is also a logical test but if the condition is not satisfied
# the code will continue to the next lines. 

# ifelse will always have an output
if (0 == TRUE) {
  print("succes")
}
ifelse(0 == TRUE, "succes", "failure")

# an if condition can be a chain of condition checks whereas ifelse can only check for one condition

if (0 == TRUE) {
  print("succes")
} else if (1 == FALSE) {
  print("failure")
} else {
  print("undecided")
}

# 2 

greeting <- function(t){ 
  if (hour(t) < 12 ) {
    print("good morning")
  } else if (hour(t) < 18) {
    print("good afternoon")
  } else {
    print("good evening")
  }
}
# 19.5.5: 1 ?
commas <- function(...) stringr::str_c(..., collapse = "-")

