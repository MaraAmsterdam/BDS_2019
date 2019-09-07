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
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "right")
?separate()

#fill and extra deal with the cases when the data does not fit within the tibble

# 13.3.1
library("nycflights13")

flights %>% 
  mutate(
    id = row_number()
  ) %>% 
  select(id, everything())
# 2.1 The following combination identifies the unique combination

batting <- Lahman::Batting

batting %>% 
  count(playerID, teamID, lgID, yearID, stint) %>% 
  filter(n > 1)

# 2.2 
library(babynames)

babynames %>% 
  count(prop, year, name, sex) %>% 
  filter(n > 1)
# 2.3
library(nasaweather)
nasaweather::atmos %>% 
  count(lat, long, year, month) %>% 
  filter(n > 1)

#2.4
library(fueleconomy)

fueleconomy::vehicles %>% 
  count(id) %>% 
  filter(n > 1)

# can't find unique id
ggplot2::diamonds %>% 
  count(z,y, x, carat, cut, price) %>% 
  filter(n > 1)

# 13.4.6

#2

View(flights)

View(flights %>% 
       left_join(airports, c("dest" = "faa", "origin" = "faa")) %>% 
       select(-alt, -tz, -dst, -tzone))

# 13.5.1

#2

hundred_flights <-
  flights %>% 
  count(tailnum) %>% 
  filter(n > 100)

flights %>% 
  semi_join(hundred_flights)

