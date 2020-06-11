# Matrix with peoples opinions
# Each column represent one person
# Rows represent their opinions
# 1 = dislikes; 0 = likes
m <- matrix(sample(c(1,0), 400, prob=c(0.3, 0.7), replace=T), nrow = 20)

# Sum matrix with its transposition to get a sum of scores in each relationship
m <- m + t(m)

# Naming columns will ensure that we never lose track of individuals
colnames(m) <- c(1:20)

# We'll iterate the search process indefinetely, 
# And record each time a new perfect combination is found
pickg <- function(m, r = c(NA,NA,NA)){
  
  if(nrow(m)<2){
    r <- r[-1,]
    pairs.list <- list(r[,-3], "score" = sum(r[,3]))
    return(pairs.list)
  } else {
    
    # Sample two individuals to make a pair
    pair <- c(sample(c(1:nrow(m)),2, replace = FALSE))
    
    # Keep the score of this pair in a variable
    pair.score <- as.numeric(m[pair[1], pair[2]])
    
    # Build the pair list + corresponding scores
    r <- rbind(r, c(as.numeric(colnames(m)[pair]), pair.score))
    
    # Remove the chosen pairs from the matrix, and repeat the function
    n <- m[-as.numeric(pair), -as.numeric(pair)]
    pickg(n, r)
    
  }
}

pick.best <- function(m, runs = 10, criteria = 0, i = 1, picks = list(), mintime = 3){
  timer1 <- proc.time()[1]
  repeat{
    best.pick <- pickg(m)
    if(proc.time()[1]-timer1 > mintime){
      readline(prompt="Processing time too long. It may not be possible to find a suitable match. You should consider adjusting either the variable 'runs' (e.g. runs = 1), or the variable 'criteria' (e.g. criteria = 1). Otherwise, if you want to keep searching, press [enter] to continue.")
      mintime <- mintime*10
    }
    if(best.pick$score==criteria){
      picks[i] <- best.pick[1]
      if(i == runs){
        return(picks)
        break
      }
      i <- i + 1
    }
  }
}

# Pick one perfect combination of pairs in matrix m
pick.best(m, 1)
