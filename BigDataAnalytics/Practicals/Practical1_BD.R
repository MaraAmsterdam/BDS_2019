library("tidyverse")

# Load data

plots <- read_csv("https://raw.githubusercontent.com/weecology/portal-teachingdb/master/plots.csv")
species <- read_csv("https://raw.githubusercontent.com/weecology/portal-teachingdb/master/species.csv")
surveys <- read_csv("https://raw.githubusercontent.com/weecology/portal-teachingdb/master/surveys.csv")

# not tidy, because the counts are in different columns, whereas tidy requires every value 


counts_df <- data.frame(
  day = c("Monday", "Tuesday", "Wednesday"),
  wolf = c(2, 1, 3),
  hare = c(20, 25, 30),
  fox = c(4, 4, 4)
)
counts_df

# Turn into tibble

counts_gather <- gather(counts_df, key = "species", value = "count", wolf:fox)
counts_gather

# inverse of gather is counts_spread

counts_spread <- spread(counts_gather, key = species, value = count)
counts_spread

counts_gather <- counts_gather[-8,]
counts_gather

counts_spread <- spread(counts_gather, key = species, value = count, fill = 0)

library(dplyr)

# %in% means the value is in a set checks if the value in month colums contains 1, 2 or 3
surveys1990_winter <- filter(surveys, year == 1990, month %in% 1:3)

# selecting

surveys1990_winter <- select(surveys1990_winter, -year)
head(surveys1990_winter)

# exercise

selection <- surveys %>% filter(species_id == "RO") %>%
  select(record_id, sex, weight)

surveys1990_winter %>% 
  group_by(species_id) %>%
  summarise(
    average_weight = mean()
  )

# for DM species calculate average weight and hindfoot length per month
surveys %>% filter(species_id == "DM") %>%
  group_by(month) %>%
  summarise(
    average_weight = mean(weight, na.rm = TRUE),
    average_hindfoot = mean(hindfoot_length, na.rm = TRUE)
  )

# joining
surveys1990_winter <- group_by(surveys1990_winter, species_id)
counts_1990w <- summarize(surveys1990_winter, count = n())

counts_1990w_join <- inner_join(counts_1990w, species)

# 1st filter and 2nd group by taxon

counts_1990w_join %>% 
  group_by(genus) %>%
  filter(count == max(count))

counts_1990w_join %>%
  group_by(taxa) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  )

new_counts <- surveys %>%
  filter(year == 1990, month %in% 1:3) %>% 
  select(-year) %>%
  group_by(species_id) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  inner_join(species)

portal_db <- src_postgres(host = "pgstudio.research.sesync.org",
                          dbname = "portal", user = "student", 
                          password = "%password%")
surveys_sql <- tbl(portal_db, "surveys")
surveys_sql





