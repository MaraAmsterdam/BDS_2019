### word count with stop words included

word_count <- transcripts_df %>% 
  unnest_tokens(token, Text, token = 'words') %>% 
  group_by(vlogId) %>%
  tally(name = 'word count')


##WORD LENGHT


word_length <- transcripts_df %>% 
  unnest_tokens(token, Text, token = 'words') %>% 
  filter(str_length(token) > 6) %>%
  group_by(vlogId) %>%
  tally(name = 'lengthy words')



### swear words ###


swearwods <- read.csv("swear.csv", header = F)

swears <- transcripts_df %>% 
  unnest_tokens(token, Text, token = 'words') %>% 
  inner_join(swearwods, by = c(token = 'V1')) %>%
  group_by(vlogId) %>%
  tally(name = 'swear')



### SELF-REFERENCE ###


selfreference <- transcripts_df %>% 
  unnest_tokens(token, Text, token = 'words') %>% 
  filter(token == "i"  | token ==  "me" | token == "myself" | token == "i'm" | token == "mine" |
           token == "my") %>%
  group_by(vlogId) %>%
  tally(name = 'selfwords') 


###joining


finaldata <- left_join(word_length, vlogger_features_bing, by = 'vlogId') %>% 
  left_join(selfreference, by = 'vlogId') %>%
  left_join(swears, by = "vlogId") 

finaldata$swear[is.na(finaldata$swear)] = 0
finaldata$selfwords[is.na(finaldata$selfwords)] = 0 ### look at dplyr how to transform NAs to 0



lm_1 <- lm(formula = cbind(Extr, Agr, Cons, Emot, Open) ~ .-vlogId -gender, data = finaldata)
summary(lm_1)
