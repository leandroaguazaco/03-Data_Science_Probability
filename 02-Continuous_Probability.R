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
