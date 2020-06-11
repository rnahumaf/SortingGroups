# Now let's try find all pairs with 0 incompatibilities
# Find all combinations in a combination matrix of size m x m
all.combn <- data.frame(t(combn(c(1:ncol(m)), 2)))

# Add a new column with the sum of incompatibilities
for(i in 1:nrow(all.combn)){
  all.combn$incomp[i] <- sum(c(m[all.combn[i,1], all.combn[i,2]], m[all.combn[i,2], all.combn[i,1]]))
}

# Keep only the ones with no incompatibilities
filter.combn <- all.combn[which(all.combn$incomp==0),c(1:2)]

# Name the rows, so we don't lose track of the combinations that were used
rownames(filter.combn) <- c(1:nrow(filter.combn))

# All possible perfect pairs
filter.combn
