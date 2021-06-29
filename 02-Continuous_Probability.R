library(tidyverse)
library(dslabs)

# Continuous Probability ====

# Operates on intervals than single values
# Empirical Cumulative Distribution Function eCDF

data("heights")
head(heights)

male_heights <- heights %>%  
  filter(sex == "Male") %>% 
  pull(height)

f <- function(a) mean(male_heights <= a) # eCDF

# If I pick one of the male students at random, what is the chance
# that he is taller than 70 inches?

1 - f(70.5)

# The cumulative distribution function defines a probability distribution
# for picking a height at random from our vector of male heights.

# Theoretical Distribution ====

# CDF for the normal distribution

pnorm(q = 70.5, 
      mean = mean(male_heights), 
      sd = sd(male_heights), 
      lower.tail = F)

plot(prop.table(table(male_heights)), 
     xlab = "a = Height in inches", 
     ylab = "P(X = a)")

limits <- list(a = c(68.5, 67.5), 
               b = c(69.5, 68.5), 
               c = c(70.5, 69.5))

Fun_Diff <- function(i) {
  mean(male_heights <= limits[[i]][1]) - mean(male_heights <= limits[[i]][2])
}

map_dbl(.x = 1:3, .f = Fun_Diff)

Fun_Diff_2 <- function(i) {
  pnorm(limits[[i]][1], mean = mean(male_heights), sd = sd(male_heights)) - 
  pnorm(limits[[i]][2], mean = mean(male_heights), sd = sd(male_heights))
}

map_dbl(.x = 1:3, .f = Fun_Diff_2)

# 
# Probability Density ====

# For continuous distributions, the probability of a single values is not defined.

pnorm(q = 76, # CDF
      mean = mean(male_heights), 
      sd = sd(male_heights), 
      lower.tail = T)


dnorm(x = 76, # PDF
      mean = mean(male_heights), 
      sd = sd(male_heights))

# Plotting de PDF, probability density function

x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) |> 
  ggplot(mapping = aes(x = x, 
                       y = f)) +
    geom_line(col = "blue") + 
    labs(y = "Probability Density") +
    theme_bw()

# Monte Carlo Simulation ====

# Monte Carlo simulation using normally distributed variables.
# Using rnorm() function.

avg <- mean(male_heights)
sd <- sd(male_heights)
n <- length(male_heights)

simulated_heights <- rnorm(n, avg, sd)

data.frame(x = simulated_heights) |> 
  ggplot(aes(x = x)) +
    geom_histogram(color = "black", 
                   binwidth = 2) +
    labs(y = "Frequency") + 
    theme_bw()

# How rare is that the tallest person is a seven footer?

tallest <- replicate(10^5, {
  simulated_data <- rnorm(n = 800, 
                          mean = avg, 
                          sd = sd)
  max(simulated_data)
})

data.frame(x = tallest) %>% 
  ggplot(aes(x = x)) + 
    geom_histogram(color = "black", 
                   binwidth = 1) + 
    labs(y = "Frequency") + 
    theme_bw()

mean(tallest >= 7*12)
# Continuous Probability Assessment ====

set.seed(16)
act_scores <- rnorm(n = 10^4, mean = 20.9, sd = 5.7)

# 1a
mean(act_scores)
# 1b
sd(act_scores)
# 1c
ceiling((10^4)*pnorm(q = 36, 
                     mean = 20.9, sd = 5.7, 
                     lower.tail = FALSE))

act_scores_sort <- sort(act_scores)
head(act_scores_sort, 100)
# 1d
pnorm(q = 30, mean = 20.84012, sd = 5.675237, lower.tail = FALSE)
# 1e
pnorm(q = 10, mean = 20.84012, sd = 5.675237, lower.tail = TRUE)
# 2
x <- seq(1, 36)
f_x <- dnorm(x, x = 20.9, sd = 5.7)

data.frame(x, f_x) %>% 
  ggplot(aes(x = x, 
             y = f_x)) + 
    geom_line() + 
    theme_bw()
# 3a
act_scores_stand <- scale(act_scores)
pnorm(q = 2, mean = 0, sd = 1, lower.tail = F)
# 3b
2*5.675237 + 20.84012
# 3c
qnorm(p = 0.975, mean = mean(act_scores), sd = sd(act_scores))
# 4a
x <- seq(1, 36)
CDF <- function(a) mean(act_scores <= a)
results <- sapply(x, CDF)
names(results) <- x
which(results >= 0.95)[1]
# 4b
qnorm(p = 0.95, mean = 20.9, sd = 5.7)
# 4c
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(x = act_scores, p)
# 4d
theoretical_quantiles <- qnorm(p, mean = 20.9, sd = 5.7)

ggplot(data = NULL, 
       mapping = aes(x = theoretical_quantiles, 
                     y = sample_quantiles)) + 
  geom_point() +
  geom_abline() + 
  theme_bw()
