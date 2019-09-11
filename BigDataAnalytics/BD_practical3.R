# 
library(tidyverse)
syldf <- read.csv(file = "/Users/maraoosterbaan/GitHub/BDS_2019/en_syllable_3grams.csv", check.n = FALSE, stringsAsFactors = FALSE)

# caret::nearZeroVar(syldf)
install.packages("caret")
library(caret)
# remove near zero variance

remove <- caret::nearZeroVar(syldf)
syldf_new <- syldf[,-remove]

# remove highly correlated features
clean <- syldf_new[,-c(1:2)]
r <- cor(clean)
cor_ind <- caret::findCorrelation(cor_matrix) + 2
syldf3 <- clean[,-cor_ind]

names(syldf3) = gsub("^$", " ", names(syldf3))

# convert x to matrix

x = as.matrix(cbind(rep(1,1,3200), syldf3))

# 2
y <- syldf[,2]

#3
hat_beta <- solve(t(x) %*% x) %*% t(x) %*% y

