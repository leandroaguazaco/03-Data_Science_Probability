# Random Variables ====

# Numeric outcomes resulting from a random process

beads <- rep(c("red", "blue"), times = c(2, 3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)
X
