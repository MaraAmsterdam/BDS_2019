# Load data
library(readtext)
library(tidytext)
library(tidyverse)
# Read in transcripts
transcripts <- readtext("/Users/maraoosterbaan/GitHub/BDS_2019/Kaggle1/youtube-personality/transcripts/*",
                        docvarsfrom = "filenames",
                        docvarnames = "vlog_id")[,-1]
# Change order of df
transcripts <- transcripts[c("vlog_id", "text")]
transcripts_tibble <- as_tibble(transcripts)

transcripts_words <- transcripts %>% 
  unnest_tokens(word, text) %>% 
  group_by(vlog_id) %>% 
  count(word)

# Remove stop words
tidy_transcripts <- transcripts_words %>% 
  anti_join(stop_words)

# Tranform into "wide" format
transcripts_final <- tidy_transcripts %>% 
  spread(key = word, value =  n)

# should we remove xxxx?

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

test <- tidy_transcripts %>% 
  inner_join(nrc_joy) %>% 
  mutate(
    total_joy = n()
  )

joy_score <- test %>% 
  spread(key = word, value = n)

joy <- joy_score[c("vlog_id","total_joy")]



  