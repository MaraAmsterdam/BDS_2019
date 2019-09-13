library("httr")

url1 <- httr::GET(url = "https://raw.githubusercontent.com/vukasing95/BDA-2019/master/YouTube-Personality-Personality_impression_scores_train.csv")


database <- read.csv(textConnection(content(url1, 'text')), sep = " ")
