source("http://bit.ly/stevens96Ch2")

# t() transpose, %*% multiply matrices det() calculate determinant, inverse solve()

library(mlbench)
# recognize patters
letters <- LetterRecognition
sub <- letters[,c(2:17)]
v_new = c(2,4,4,5,4,8,6,4,4,6,6,5,6,8,6,9)
count = matrix(0, nrow = nrow(letters), ncol = 1)

for(i in 1:nrow(letters)){
  for(j in 1:ncol(sub)){
    if(letters[i,j] == v_new[j]){
      count[i,1] = count[i,1] + 1
     }
  }
}

# the most similar letter is
letters[which.max(count),1]

sort(count, decreasing = TRUE)[1:6]

which(count[,1] == 7)
