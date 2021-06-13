library(tidyverse)
library(gtools)

# Monte Carlo Simulations ====

# Urn that contains two red bead and three blue ones
beads <- rep(c("red", "blue"), c(2, 3))
beads
# Probability to pick one red bead is 2/5 or 40%

# Experiment: to pick a bead, one random outcome
sample(beads, size = 1) 

# Repeat the experiment a large enough number of times, equivalent to doing it over and over
# Stochastic process

# replicate() function permits to repeat the same task any number of times we want

events <- replicate(10000, sample(beads, size = 1))
prop.table(table(events)) # good approximation

events2 <- sample(x = beads, 
                  size = 10^7, 
                  replace = T)

prop.table(table(events2))
?set.seed

# Using the mean() function to calculate proportions
mean(beads == "blue") # mean() operates over logical elements

# Combinations and permutations ====

expand.grid() # Give us all combinations of two lists, similarly to tree diagram

suits <- c("Diamonds", "Clubs", "Hearts", "Spade")
values <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eigth", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(value = values, suit = suits)
deck <- paste(deck$value, deck$suit)

# Probability of a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)

# Permutations: order matters, a-b != b-a
permutations(5, 2)

all_phone_numbers <- permutations(10, 7, 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index, ]

# Possible hands in a deck game
hands <- permutations(n = 52, r = 2, v = deck)
first_card <- hands[ , 1] 
second_card <- hands[ , 2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# Combinations: order not matters, a-b == b-a
combinations(3, 2)

# Black Jack
aces <- paste("Ace", suits)
facecard <- c("Jack", "Queen", "King", "Ten")
facecard <- expand.grid(value = facecard, suit = suits)
facecard <- paste(facecard$value, facecard$suit)
hands <- combinations(n = 52, r = 2, v = deck)
mean(hands[ , 1] %in% aces & hands[ , 2] %in% facecard)
mean(hands[ , 1] %in% aces & hands[ , 2] %in% facecard | 
     hands[ , 1] %in% facecard & hands[ , 2] %in% aces)

# Monte Carlo Simulation 

results <- replicate(1000, {
  hand <- sample(deck, 2)
  (hands[ , 1] %in% aces & hands[ , 2] %in% facecard) | (hands[ , 1] %in% facecard & hands[ , 2] %in% aces)
})

mean(results)


# The Birthday Problem ====

results <- replicate(10^4, {
  birthdays <- sample(x = 1:365, size = 50, replace = T)
  any(duplicated(birthdays))
})

mean(results)

compute_prob <- function(n = 50, b = 10^4) {
  same_day <- replicate(b, {
    birthdays <- sample(x = 1:365, size = n, replace = TRUE)
    any(duplicated(birthdays))
  })
  mean(same_day)
}

compute_prob()

# sapply function

x <- 1:10 # Example
sapply(x, FUN = sqrt)

n <- seq(1, 60)
prob <- sapply(n, compute_prob)

plot(n, prob)

exact_prob <- function(n) {
  prob_unique <- seq(365, 365 - n + 1)/365
  1 - prod(prob_unique) # product
}

eprob <- sapply(n, exact_prob)

plot(n, prob)
points(n, eprob, col = "red")

# How many Monte Carlo simulations are enough? ====

# Check the stability of the estimate

b <- 10^seq(1, 5, len = 100)
compute_prob <- function(b, n = 22) {
  same_day <- replicate(b, {
    birthdays <- sample(1:365, size = n, replace = TRUE)
    any(duplicated(birthdays))
  })
  mean(same_day)
}

prob <- sapply(b, compute_prob)

plot(log10(b), prob, type = "l") 

# print numeric values in fixed or exponential notation
options(scipen = 999) # Avoids scientific notation
options(scipen = 0) # Allows scientific notation

ggplot(data = NULL, 
       mapping = aes(x  = b, 
                     y = prob)) + 
  geom_line() + 
  scale_x_log10() + 
  labs(x = "Number of Monte Carlo Simulations", 
       y = "Probability") + 
  theme_bw()

x <- c(1:5)
any(x > 3)

# Monty Hall Problem ====

# Stick strategy 
stick <- replicate(10^4, {
  doors <- as.character(1:3) 
  prize <- sample(c("car", "goat", "goat")) 
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1) 
  stick <- my_pick
  stick == prize_door
})

mean(stick)

# Switch strategy
stick <- replicate(10^4, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})

mean(stick)

# Assessment ====

# Question 1: Olympic running
nrow(permutations(n = 8, r = 3)) #1a
nrow(permutations(n = 3, r = 3)) #1b
(3/8) * (2/7) * (1/6) # 1c
# 1d
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
results <- replicate(10^4, {
  winners <- sample(x = runners, size = 3, replace = FALSE)
  all(winners == "Jamaica")
})
mean(results)

# Question 2: Restaurant Management
6 * nrow(combinations(n = 6, r = 2)) * 2 # 2a
6 * nrow(combinations(n = 6, r = 2)) * 3 # 2b
6 * nrow(combinations(n = 6, r = 3)) * 3 # 2c
# 2d
meal_comb <- function(n) {
  n * nrow(combinations(n = 6, r = 2)) * 3
}
meals <- sapply(1:12, meal_comb) 
which(meals >= 365)
# 2e
meal_comb <- function(b) { 
  6 * nrow(combinations(n = b, r = 2)) * 3
}
meals <- sapply(2:12, meal_comb)
which(meals >= 365) + 1

# Question 3-4
data("esoph")
head(esoph)
str(esoph) # 3a
all_cases <- sum(esoph$ncases) # 3b
all_controls <- sum(esoph$ncontrols) # 3c
# 4a
levels(esoph$alcgp)
esoph %>% 
  filter(alcgp == "120+") %>% 
  summarize(sum(ncontrols))
45/(67 + 45)
# 4b
esoph %>% 
  filter(alcgp == "0-39g/day") %>% 
  summarize(sum(ncontrols))
29/(29 + 415)
# 4c
esoph %>% 
  filter(!tobgp == "0-9g/day") %>% 
  summarize(sum(ncases))
122/all_cases
# 4d
esoph %>% 
  filter(!tobgp == "0-9g/day") %>% 
  summarize(sum(ncontrols))
450/all_controls  

# Question 5-6
# 5a
esoph %>% 
  filter(alcgp == "120+") %>% 
  summarise(sum(ncases))
45/all_cases
# 5b
levels(esoph$tobgp)
esoph %>% 
  filter(tobgp == "30+") %>% 
  summarize(sum(ncases))
31/all_cases
# 5c
esoph %>% 
  filter(alcgp == "120+", 
         tobgp == "30+") %>% 
  summarize(sum(ncases))
(10/45) * (45/all_cases) # Multiplication rule
# 5d
45/all_cases + 31/all_cases - ((10/45) * (45/all_cases)) # Addition rule
# 6a
esoph %>% 
  filter(alcgp == "120+") %>% 
  summarize(sum(ncontrols))
67/all_controls
# 6b
0.225/0.0687
# 6c
esoph %>% 
  filter(tobgp == "30+") %>% 
  summarize(sum(ncontrols))
82/all_controls
# 6d
esoph %>% 
  filter(alcgp == "120+", 
         tobgp == "30+") %>% 
  summarize(sum(ncontrols))
(12/82) * (82/all_controls) # Multiplication rule
# 6e
0.06871795 + 0.08410256 - 0.01230769 # Addition rule
# 6f
0.33 / 0.140 

# Git test