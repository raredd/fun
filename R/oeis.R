### oeis sequences
# A006877, A133058, A265326
# 
# todo
# A060412
###


#' A006877
#' 
#' A006877: In the "3x+1" problem, these values for the starting value set new
#' records for number of steps to reach 1.
#' 
#' @return
#' 1, 2, 3, 6, 7, 9, 18, 25, 27, 54, 73, 97, 129, 171, 231, 313, 327, 649,
#' 703, 871, 1161, 2223, 2463, 2919, 3711, 6171, 10971, 13255, 17647, 23529,
#' 26623, 34239, 35655, 52527, 77031, 106239, 142587, 156159, 216367, 230631,
#' 410011, 511935, 626331, 837799, ...
#' 
#' @family oeis
#' 
#' @seealso
#' \url{https://oeis.org/A006877}
#' 
#' \code{\link{collatz}}
#' 
#' @export

A006877 <- function(n = 1000L, plot = TRUE) {
  collatz1 <- function(n) {
    n  <- as.integer(n)
    ii <- 1L
    nn <- n
    
    while (n > 1L) {
      n  <- (n / 2L) * (n %% 2L == 0L) + (3L * n + 1L) * (n %% 2L == 1L)
      ii <- ii + 1L
      nn <- c(nn, n)
    }
    
    list(sequence = nn, iterations = ii - 1L,
         max_value = max(nn), max_position = which.max(nn))
  }
  
  A006877 <- vapply(seq.int(n), function(x)
    collatz1(x)$iterations, integer(1L))
  
  A006877 <- c(1L, sapply(seq.int(n), function(x)
    if (A006877[x] > max(A006877[1:(x - 1L)])) x))
  A006877 <- unlist(A006877)
  
  logn <- ifelse(n > 5000L, 'x', '')
  if (plot)
    plot(A006877, log = logn, ylab = 'A006877')
  
  A006877
}

#' A133058
#' 
#' A133058: a(0)=a(1)=1; for n>1, a(n) = a(n-1) + n + 1 if a(n-1) and n are
#' coprime, otherwise a(n) = a(n-1)/gcd(a(n-1),n).
#' 
#' @return
#' 1, 1, 4, 8, 2, 8, 4, 12, 3, 1, 12, 24, 2, 16, 8, 24, 3, 21, 7, 27, 48, 16,
#' 8, 32, 4, 30, 15, 5, 34, 64, 32, 64, 2, 36, 18, 54, 3, 41, 80, 120, 3, 45,
#' 15, 59, 104, 150, 75, 123, 41, 91, 142, 194, 97, 151, 206, 262, 131, 189,
#' 248, 308, 77, 139, 202, 266, 133, 199, 266, 334, 167, ...
#' 
#' @family oeis
#' 
#' @seealso
#' \url{https://oeis.org/A133058}
#' 
#' @export

A133058 <- function(n = 2000L, plot = TRUE) {
  n <- seq(0, n)
  A133058 <- rep_len(1L, length(n))
  
  gcd <- function(x, y) {
    ifelse(r <- x %% y, Recall(y, r), y)
  }
  
  for (ii in seq_along(n[-(1:2)]) + 2L) {
    A133058[ii] <- if ((g <- gcd(n[ii], A133058[ii - 1L])) == 1L)
      A133058[ii - 1L] + n[ii] + 1L
    else A133058[ii - 1L] / g
  }
  
  if (plot)
    plot(A133058, pch = '.', log = 'y')
  
  A133058
}

#' A265326
#' 
#' A265326: n-th prime minus its binary reversal.
#' 
#' @return
#' 1, 0, 0, 0, -2, 2, 0, -6, -6, 6, 0, -4, 4, -10, -14, 10, 4, 14, -30, -42,
#' 0, -42, -18, 12, 30, 18, -12, 0, 18, 42, 0, -62, -8, -70, -20, -82, -28,
#' -34, -62, -8, -26, 8, -62, 62, 34, -28, 8, -28, 28, 62, 82, -8, 98, 28, 0,
#' -186, -84, -210, -60, ...
#' 
#' @family oeis
#' 
#' @seealso
#' \url{https://oeis.org/A265326}
#' 
#' @export

A265326 <- function(n = 10000L, plot = TRUE) {
  is.prime <- function(x) {
    vapply(x, function(y) sum(y / 1:y == y %/% 1:y), integer(1L)) == 2L
  }
  
  n <- seq(2, n)
  Primes <- n[is.prime(n)]
  
  b <- sapply(Primes, function(x) as.integer(intToBits(x)))
  i <- max.col(t(b), ties.method = 'last')
  
  c <- sapply(seq_along(Primes), function(ii) {
    b[1:i[ii], ii] <- rev(b[1:i[ii], ii])
    b[, ii]
  })
  
  r <- 2 ^ (seq.int(nrow(b)) - 1L)
  A265326 <- colSums(b * r) - colSums(c * r)
  
  if (plot)
    plot(Primes, A265326, pch = '.')
  
  A265326
}
