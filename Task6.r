kwtest <- function (x, g, ...)
{
  if (is.list(x)) {
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    if (!missing(g))
      warning("'x' is a list, so ignoring argument 'g'")
    if (!all(sapply(x, is.numeric)))
      warning("some elements of 'x' are not numeric and will be coerced to numeric")
    k <- length(x)
    l <- lengths(x)
    if (any(l == 0L))
      stop("all groups must contain data")
    g <- factor(rep.int(seq_len(k), l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2L)
    stop("all observations are in the same group")
  }
  n <- length(x)
  if (n < 2L)
    stop("not enough observations")
  r <- rank(x)
  TIES <- table(x)
  STATISTIC <- sum(tapply(r, g, sum)^2/tapply(r, g, length))
  STATISTIC <- ((12 * STATISTIC/(n * (n + 1)) - 3 * (n + 1))/(1 -sum(TIES^3 - TIES)/(n^3 - n)))
  PARAMETER <- k - 1L
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  names(STATISTIC) <- "Kruskal-Wallis chi-squared"
  names(PARAMETER) <- "df"
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
               p.value = PVAL, method = "Kruskal-Wallis rank sum test")
  return(RVAL)
}

kwtest_fast <- function(x, g) {
  # Check if x and g have the same length
  if (length(x) != length(g))
    stop("'x' and 'g' must have the same length")
  
  # Convert g to factor
  g <- factor(g)
  
  # Check if there are at least 2 unique groups
  if (nlevels(g) < 2)
    stop("all observations are in the same group")
  
  if (k < 2L)
    stop("all observations are in the same group")
  
  N <- length(x)
  number_groups <- nlevels(factor(g))
  obs_group <- table(g)
  r <- rank(x)
  r_group_avg <- tapply(r, g, sum)/obs_group
  r_avg <- (N + 1)/2
  
  H_num <-(N - 1) * sum(obs_group * (r_group_avg - r_avg)**2)
  H_dem <- sum((r - r_avg)**2)
  
  H <- H_num/H_dem
  
  return(H)
}

kwtest_matrix <- function(X, grp) {
  res <- apply(X, 1, function(x) kwtest(x, grp))
}


kwtest_matrix2 <- function(X, g) {
  if (!is.matrix(X)) {
    stop("X must be a matrix.")
  }
  if (!is.numeric(g)) {
    stop("g must be a numeric vector")
  }
  if (ncol(X) != length(g)) {
    stop("Number of cols X of matrix have to be as long as length of vector g")
  }
  m <- nrow(X)
  test_statistics <- numeric(m)
  
  for (i in 1:m) {
    experiment_data <- X[i,]
    print("Experiment data")
    print(experiment_data)
    print("Group observations")
    print(g)
    
    
    test_statistics[i] = kwtest_fast(experiment_data, g)
    print("test statistic")
    print(test_statistics[i])
  }
  
  return(test_statistics)
  
}

set.seed(1234)
m <- 1000 # number of repetitions
n <- 50
X <- matrix(rt(m * n, df = 10), nrow = m)
grp <- rep(1:3, c(20, 20, 10))

loop_approach = kwtest_matrix(X, grp)
print(loop_approach)

X <- matrix(1:4, nrow = 8, ncol = 8, byrow=TRUE)
g <- c(1, 1, 2, 2, 3, 3, 4, 4)
print(paste("Result using matrix X:", kwtest_matrix(X, g)))

x_vector <- c(1, 3, 4, 2, 2, 6, 1, 7)
g_vector <- c(1, 1, 2, 2, 3, 3, 4, 4) # group labels

print(paste("Result using x as list:", kwtest(x_list)))

print(paste("Result using x as vector:", kwtest(x_vector, g_vector)))
print(paste("Result using x as vector:", kwtest_fast(x_vector, g_vector)))

