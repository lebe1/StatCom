# Ratio of Fibonacci numbers

# a)
# Write two different R functions which return the sequence 
# ri = Fi+1 /Fi for i = 1, . . . , n where Fi is the ith
# Fibonacci number, once using for and once using while.

# For understanding, the sequence is as follows
# Fibonacci sequence: 0,1,1,2,3,5,8,13,21,..., with 0 as F0 
# r1 = F2/F1 = 1/1 = 1
# r2 = F3/F2 = 2/1 = 2
# r3 = F4/F3 = 3/2 = 1.5

fib_ratio_for <- function(n) {
  # Check for bad inputs
  if (n <= 0) {
    stop("Input 'n' cannot be 0 or negative.")
  } else if (n == 1) {
    # Catch case of n = 1 due to a leading error if not implemented
    return(1)
  } else {
    # numeric vector created with length n + 1 
    # to store the Fibonacci sequence up to the (n + 1)th element.
    fibonacci <- numeric(n + 1) 
    # Assign values (1,1) as first two elements
    fibonacci[1:2] <- c(1, 1)
    # for loop that iterates over the sequence 3:(n + 1)
    # starting from 3 because first two elements are already declared
    for (i in 3:(n + 1)) {
      # Assigning Fibonacci rule to get the next number out of the last two
      fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
    }
    # Finally calculate the ratio of each fibonacci number
    ratios <- fibonacci[2:(n + 1)] / fibonacci[1:n]
    return(ratios)
  }
}

fib_ratio_while <- function(n) {
  if (n <= 0) {
    stop("Input 'n' cannot be 0 or negative.")
  } else if (n == 1) {
    return(1)
  } else {
    fibonacci <- numeric(n + 1)
    fibonacci[1:2] <- c(1, 1)
    i <- 3
    while (i <= n + 1) {
      fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
      i <- i + 1
    }
    ratios <- fibonacci[2:(n + 1)] / fibonacci[1:n]
    return(ratios)
  }
}

fib_ratio_for(2)

fib_ratio_while(5)

# b)

# Benchmark the two functions for n = 200 and n = 2000 
# (you can use package microbenchmark or package
# bench for this purpose). Which function is faster?
# As given from the stats, the for loop is faster.

# install.packages('microbenchmark')
library(microbenchmark)

microbenchmark(
  fib_ratio_for(200),
  fib_ratio_while(200),
  fib_ratio_for(2000),
  fib_ratio_while(2000)
)

# Explanation for statistics given from microbenchmark
# min: Minimum time it took to execute
# lq: Lower quartile (25th percentile) time it took to execute
# mean: Mean time it took to execute
# median: Median time it took to execute
# uq: Upper quartile (75th percentile) time it took to execute
# max: Maximum time it took to execute
# neval: Number of times each expression was evaluated

# c)

# Plot the sequence for n = 100. For which value n it starts to converge? 
# What is the number that it converges to?
# It converges towards the 1.618034. The so-called golden ratio.

# Calculate the sequence for n = 100
sequence <- fib_ratio_for(100)

# Plot the sequence
plot(sequence, type = "l", xlab = "Index (i)", ylab = "Ratio (r_i)",
     main = "Sequence of Fibonacci Ratios for n = 100")

# Get the last 6 values to know what number it converges to
tail(sequence)
