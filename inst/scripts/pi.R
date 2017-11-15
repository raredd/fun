## pi

pi


## greated common divisor
gcd <- function(x, y) {
  ifelse(r <- x %% y, Recall(y, r), y)
}
gcd(5040, 1024)

## common factors
factors <- function(...) {
  factors_ <- function(x) {
    y <- seq_len(abs(x))
    y[x %% y == 0L]
  }
  
  Reduce(intersect, lapply(as.integer(c(...)), factors_))
}
rawr::factors(25, 30)


number_set <- 500L
replications <- 1e5L


## method 1 - faster
## choose two numbers from the number set
## check for any factors (other than 1)
## repeat

set.seed(1)
y <- replicate(replications, {
  x <- sample(number_set, 2L)
  any(
    factors(x[1L])[-1L] %in%
      factors(x[2L])[-1L]
  )
})

pii <- sqrt(6 / (1 - mean(y)))
c(est = pii, diff = pii - pi)


## method 2 - faster
## choose two numbers from the number set
## check the greatest common divisor is larger than 1
## repeat

set.seed(1)
y <- replicate(replications, {
  x <- sample(number_set, 2L)
  gcd(x[1L], x[2L]) > 1L
})

pii <- sqrt(6 / (1 - mean(y)))
c(est = pii, diff = pii - pi)
