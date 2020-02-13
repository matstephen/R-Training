
#2. THE VERY BASICS

die <- 1:6

# sample function
sample(x = die, size = 1)

sample(die, 1)

# look up a functions arguments
args(sample)

# sample with replacement (FALSE is default value)
sample(x = die, size = 2, replace = TRUE)

# add up the values of the two dice

dice <- sample(x = die, size = 2, replace = TRUE)
dice
sum(dice)

# create a function called roll to roll the dice

roll <- function() {
  die <- 1:6
  dice <- sample(x = die, size = 2, replace = TRUE)
  sum(dice)
}

roll()


# supplying an object (called bones) to a function
roll2 <- function(bones) {
    dice <- sample(bones, size=2, replace = TRUE)
    sum(dice)
}

roll2(bones = 1:4)  
roll2(bones = 1:20)
  
# provide bones a default value
roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size=2, replace = TRUE)
  sum(dice)
}

roll2()
roll2(bones = 10:50)


# cubic plot using qplot (quick plot)
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
y <- x^3
qplot(x, y)


# qplot with provide a histogram whenever you give it a single vector
x <- c(1, 2, 2, 2, 3, 3)
qplot(x, binwidth = 1)


# replicate
replicate(3, 1+1)

replicate(10, roll())

# simulate 10,000 dice rolls and plot the results
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)


