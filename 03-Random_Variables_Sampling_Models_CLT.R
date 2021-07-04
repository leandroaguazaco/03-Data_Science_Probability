# Random Variables ====

# Numeric outcomes resulting from a random process

beads <- rep(c("red", "blue"), times = c(2, 3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)
X

# Sampling Models ====

# A sampling model models the random behavior of a process as the sampling of draws from an urn.

color <- rep(x = c("Red", "Black", "Green"), 
             times = c(18, 18, 2))
color

X <- sample(x = ifelse(color == "Red", -1, 1), 
            size = 1000, 
            replace = TRUE)
X[1:10]

X <- sample(x = c(-1, 1), # Sampling model
            size = 1000, 
            replace = TRUE,
            prob = c(9/19, 10/19))
S <- sum(X)
S

# Probability distribution of a random variable: probability of the observed
# value falling in any given interval

n <- 1000
B <- 10^4
S <- replicate(n = B, expr = {
  X <- sample(c(-1, 1), 
              size = n, 
              replace = TRUE, 
              prob = c(9/19, 10/19))
  S <- sum(X)
})

s <- seq(min(S), max(S), length = 100)
normal_density <- data.frame(s, f = dnorm(s, mean(S), sd(S)))

data.frame(S) %>% 
  ggplot(aes(x = S, y = ..density..)) +
    geom_histogram(color = "black", 
                   binwidth = 8) +
    labs(y = "Probability") +
    geom_line(data = normal_density, 
              mapping = aes(x = s,
                            y = f), 
              color = "blue") + 
    theme_bw()

mean(S < 0)

sample_quantiles <- quantile(S, p)
theoretical_quantiles <- qnorm(p, mean = mean(S), sd = sd(S))

ggplot(data = NULL,
       mapping = aes(x = theoretical_quantiles, 
                     y = sample_quantiles)) + 
  geom_point() +
  geom_abline() + 
  theme_bw()

# The average of many draws of a random variable is called its expected value.
# The standard deviation of many draws of a random variable is called its standard error.
# Distributions versus Probability Distributions ====

# Any list of numbers has a distribution. The probability distributions function of a random variable
# is defined mathematically and does not depend on a list of numbers.
# Central Limit Theorem ====

# CTL: when the number of draws is large (sample size), the sum of independent draws 
# is approximately normal.

# Assessment ====

# 1f
set.seed(21)

S <- replicate(n = 10^4, {
  X <- sample(x = c(1, -0.25), size = 1, prob = c(0.2, 0.8))
})

pnorm(q = 8, 
      mean = mean(S), 
      sd = sd(S)*sqrt(44), 
      lower.tail = FALSE)

# 2b
library(dplyr)

p <- seq(0.25, 0.95, 0.05)
p_1 <- 1 - p


data.frame(p, p_1) %>% 
  mutate(mu = 44 * p,
         se = sqrt(44) * sqrt(p*p_1), 
         prob = pnorm(35, mean = mu, sd = se, lower.tail = FALSE))

# 3a Expected value
p <- 5/38
6*p + (-1*(1 - p))

# 3b Standard error
abs(6-(-1)) * sqrt(p*(1-p))

# 3c Expected value of the average
6*p + (-1*(1 - p))

# 3d Standard error of the average
sd(sample(c(6, -1), 
          size = 500, 
          replace = TRUE, 
          prob = c(p, 1 - p)))/sqrt(500)

# 3e Expected value of the sum
(6*p + (-1*(1 - p)))*500

# 3f Standard error of the sum
sqrt(500) * (abs(6-(-1)) * sqrt(p*(1-p)))

# 3g
pnorm(q = 0, mean = -39.47368, sd = 52.91045, lower.tail = TRUE)
