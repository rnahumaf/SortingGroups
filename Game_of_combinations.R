# This image represents perfect pairs in each row
# The goal of this game is to find a combination of 10 rows
# In a way that all columns contain exactly one black square
# Which implies in a perfect combination of pairs
image(t(unique(t(replicate(50, sample(c(rep(1, 2), rep(0,18)), 20))))),  
      col = c("white", "black"))
