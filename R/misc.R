### miscellaneous stuff
# sparkbar, R, collatz, trace_path, bubble_sort, fibonnaci, fibonnaci2,
# golden, ch, is.happy, is.prime, is.unary, is.binary, binToDec, decToBin,
# decToUna, enigma, heart, the_force
###


#' SparkBar generator
#' 
#' Generates a sparkbar from a sequence of numbers.
#' 
#' @aliases
#' sparkbar, spark
#' 
#' @param ... numerics
#' @seealso \url{https://gist.github.com/ramnathv/7793167}
#' 
#' @examples
#' spark(30, 31, 32, 33)
#' spark(runif(20))
#' 
#' @export

spark <- function(...) {
  nums <- c(...)
  min_value <- min(nums)
  max_value <- max(nums)
  scale_value <- max_value - min_value
  res <- NULL
  
  for (number in nums) {
    scaled_value <- if ((number - min_value) != 0 && (scale_value != 0))
      (number - min_value) / scale_value else 0
    
    ## hack: 9604 and 9608 aren't vertically aligned as other block elements
    num <- floor(min(6, scaled_value * 7))
    num[num == 3] <- ifelse((scaled_value * 7) < 3.5, 2, 4)
    num[num == 7] <- 6
    
    res <- c(res, num)
  }
  
  noquote(intToUtf8(9601L + res))
}

#' r
#' 
#' \code{R}
#' 
#' @export

R <- function() {
  eval(quote({
    h=character;C=rep;a=b=h(0);p=options()$wi%/%2-5;n="
    ";j=C(toupper(substring(mode(a),4,4)),sum(C(5:9,2)+1)-3)
    k=C(5:9,2);k[4:5]=7;k=cumsum(k+1);`[`(j,k)=n;m=paste(h(1),h(1
    ));s=c(0,k[-10])+1;j[c(16:17,24:26,32:33,46:47,53:55,61:64
    ,70:74)]=m;for(i in 1:10)a=c(a,C(m,p),j[`:`(s[i],k[i])])
    cat(`[<-`(c(n,a),C(389),m),sep=b)
  }))
}

#' Collatz conjecture
#' 
#' Perform and visualize the Collatz conjecture.
#' 
#' @param n starting number (integer)
#' @param stoptime logical; if \code{FALSE} (default), the value at each
#' iteration is plotted with a summary of the values; if \code{TRUE}, instead,
#' the first \code{n} stopping times are plotted along with the OEIS sequence
#' of numbers \code{<= n}, that is, from one to \code{n}, the number of 
#' iterations each starting value takes to reach 1
#' 
#' @seealso
#' \code{\link{A006877}}
#' 
#' @references 
#' \url{http://imgs.xkcd.com/comics/collatz_conjecture.png}
#' 
#' \url{http://en.wikipedia.org/wiki/Collatz_conjecture}
#' 
#' \url{https://oeis.org/A006877}
#' 
#' @examples
#' collatz(19)
#' collatz(1161)
#' collatz(1618, stoptime = TRUE)
#' collatz(5005, stoptime = TRUE)
#' collatz(63728127)
#' 
#' @export

collatz <- function(n, stoptime = FALSE) {
  stopifnot(
    round(n) == n,
    n > 0
  )
  
  if (stoptime) {
    res <- vapply(seq.int(n), function(x)
      collatz1(x)$iterations, integer(1L))
    cat('\n Max time:', max(res),
        '\n  Element:', which.max(res))
    
    maxv <- c(1, sapply(seq.int(n), function(x)
      if (res[x] > max(res[1:(x - 1L)])) x))
    message('\n\nOEIS sequence A006877: \nNumbers with a total stopping time',
            ' longer than any smaller starting value\n', 
            paste(unlist(maxv), collapse = ', '))
    
    logn <- ifelse(n > 5000L, 'x', '')
    plot(seq.int(n), res, las = 1, type = 'p', bty = 'l', log = logn,
         xlab = 'Starting value', ylab = 'Stopping time',
         main = sprintf('First %s stopping times', n), 
         cex.main = .8, cex.lab = .8)
  } else {
    res <- collatz1(n)
    cat('\n   Iterations:', res$iterations, 
        '\nMaximum value:', format(res$max_value, scientific = FALSE),
        '\n Max position:', res$max_position)
    plot(res$sequence, type = 'l', las = 1, bty = 'l',
         xlab = 'Iterations', ylab = '',
         main = 'Value per iteration',
         cex.main = .8, cex.lab = .8)
  }
  
  invisible(collatz1(n))
}

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

#' Trace path
#' 
#' @param x a sequence of points
#' @param theta rotation at each value of \code{x}
#' @param ... additional arguments passed to \code{\link{lines}} or graphical
#' parameters passed to \code{\link{par}}
#' @param add logical; if \code{TRUE}, the figure is added to the current
#' plotting window
#' 
#' @examples
#' x <- seq(0, 1, length.out = 7L)
#' trace_path(1:100, rep(pi / 2, 100) * c(-1, 1), type = 'p')
#' sapply(seq_along(x), function(ii)
#'   trace_path(1:100, rep(pi * x[ii], 100) * c(-1, 1), col = ii, add = TRUE))
#' 
#' 
#' trace_path(seq(0, 1,  length.out = 200),
#'            rep(pi / 2 * (-1 + 1 / 200), 200))
#' 
#' trace_path(seq(1, 10, length.out = 1000),
#'            rep(2 * pi / 10, 1000))
#' 
#' trace_path(seq(0, 1,  length.out = 500),
#'            seq(0, pi, length.out = 500))
#' 
#' trace_path(seq(0, 1,  length.out = 600) * c(1, -1),
#'            seq(0, 8 * pi, length.out = 600) * seq(-1, 1, length.out = 200))
#' 
#' trace_path(seq(-1, 1,  length.out = 200),
#'            rep(pi / 1 * (-1 + 1 / 200), 200))
#' 
#' trace_path(seq(-1, 1,  length.out = 500),
#'            rep(pi / 1 * (-1 + 1 / 200), 500))
#' 
#' trace_path(seq(-1, 1,  length.out = 1000),
#'            rep(pi / 1 * (-1 + 1 / 500), 1000))
#' 
#' @export

trace_path <- function(x, theta, ..., add = FALSE) {
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  
  face <- pi / 2 + cumsum(theta)
  move <- x * exp(1i * face)
  pos  <- cumsum(move)
  x <- c(0, Re(pos))
  y <- c(0, Im(pos))
  
  if (!add) {
    plot.new()
    plot.window(range(x), range(y))
  }
  
  lines(x, y, ...)
}

#' Bubble sort
#' 
#' The bubble sort swaps neighboring pairs of numbers if not in proper order.
#' The algorithm is called recursively until all pairs are in order.
#' 
#' @param x a vector
#' @param plot logical; if \code{TRUE}, the sorting algorithm will be shown
#' in real-time with a fascinating plot
#' 
#' @seealso
#' \code{\link{sort}}
#' 
#' @references
#' Modified from 
#' \url{http://www.numbertheory.nl/2013/05/10/bubble-sort-implemented-in-pure-r/}
#' 
#' @examples
#' bubble_sort(rnorm(10))
#' 
#' set.seed(1)
#' x <- round(runif(100, 0, 100))
#' (sorted <- bubble_sort(x))
#' identical(sorted, sort(x))
#' # [1] TRUE
#' 
#' \dontrun{
#' library('microbenchmark')
#' microbenchmark(bubble_sort(x), sort(x), unit = 'relative')
#' 
#' # Unit: relative
#' #            expr      min       lq    mean   median       uq      max neval cld
#' #  bubble_sort(x) 9686.046 7540.075 4453.95 3490.656 3510.477 3310.549   100   b
#' #         sort(x)    1.000    1.000    1.00    1.000    1.000    1.000   100  a 
#' 
#' 
#' ## an obnoxious visualization !
#' bubble_sort(x, plot = TRUE)
#' }
#' @export

bubble_sort <- function(x, plot = FALSE) {
  ## helper fns
  sample1 <- function(x) sample(x, 1L, TRUE)
  swap_pass <- function(x) {
    for (ii in seq(1L, length(x) - 1L))
      x[ii:(ii + 1L)] <- swap_if_gt(x[ii:(ii + 1L)])
    x
  }
  swap_if_gt <- function(pair)
    ifelse(c(gt(pair), gt(pair)), rev(pair), pair)
  gt <- function(pair) pair[1L] > pair[2L]
  
  sort_vec <- swap_pass(x)
  
  if (plot) {
    op <- par(mar = c(0,0,0,0))
    on.exit(par(op))
    xx <- seq_along(sort_vec)
    
    plot.new()
    plot.window(range(xx), range(sort_vec))
    
    points(xx, sort_vec, pch = 19, col = sample1(colors()), xpd = NA)
    text(sample1(xx), sample1(sort_vec), labels = '¡¡ bubble sort !!',
         srt = sample1(1:360), col = sample1(colors()), font = 2, xpd = NA)
    
    Sys.sleep(0.1) 
  }
  
  if (isTRUE(all.equal(x, sort_vec)))
    sort_vec else Recall(sort_vec, plot)
}

#' Fibonacci's sequence
#' 
#' Calculates the nth number of Fibonacci's sequence. \code{fibonacci2} is
#' the closed form (and vectorized).
#' 
#' @param n a positive integer
#' 
#' @examples
#' fibonacci(20)
#' sapply(1:20, fibonacci)
#' fibonacci2(1:20)
#' 
#' @export

fibonacci <- local({
  memo <- c(1, 1, rep(NA, 100))
  f <- function(n) {
    if (n == 0) 
      return(0)
    if (n < 0) 
      return(NA)
    if (n > length(memo))
      stop('\'n\' is too big for implementation')
    if (!is.na(memo[n])) 
      return(memo[n])
    ans <- f(n - 2) + f(n - 1)
    memo[n] <<- ans
    ans
  }
})

#' @rdname fibonacci
#' @export
fibonacci2 <- function(n) {
  x <- sqrt(5)
  (1 / x) * ((1 + x) / 2) ** n - (1 / x) * ((1 - x) / 2) ** n
}

#' Golden ratio
#' 
#' Plots the golden ratio.
#' 
#' @param theta sequence of angles
#' 
#' @examples
#' golden()
#' 
#' @export

golden <- function(theta) {
  op <- par(ann = FALSE, mar = c(0,0,0,0), bg = 'lightgoldenrod')
  on.exit(par(op))
  
  f <- function(x) 
    ifelse(x == 90, 1, exp(2 * pi / tan(x * 2 * pi / 360)))
  f1 <- function(x)
    ifelse(x == 1, 90, 360 * atan(2 * pi / log(x)) / 2 / pi)
  
  if (missing(theta))
    theta <- seq(-100, -9.25, length.out = 1e4)
  r <- f(60) ^ (theta / 2 / pi)
  x <- r * cos(theta)
  y <- r * sin(theta)
  
  plot(x, y, type = 'l', axes = FALSE)
  
  invisible(list(x = x, y = y, r = r))
}

#' Calvin and Hobbes
#' 
#' Draw Calvin and Hobbes.
#' 
#' @param who who, \code{'calvin'} or \code{'hobbes'}
#' 
#' @seealso \url{http://www.stolaf.edu/people/olaf/cs125/}
#' 
#' @examples
#' ch()
#' 
#' @export

# see ./inst/source/ch_data.R for data
ch <- function(who) {
  theme_nothing <- function(base_size = 12, base_family = 'Helvetica') {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(rect = element_blank(),
            line = element_blank(),
            text = element_blank(),
            axis.ticks.margin = unit(0, 'lines'))
  }
  f <- function(vecx, vecy) {
    vecx <- ifelse(vecx > 100, NA, vecx)
    vecx <- ifelse(vecx < 0, NA, vecx)
    vecy <- ifelse(vecy > 100, NA, vecy)
    vecy <- ifelse(vecy < 0, NA, vecy)
    qplot(x, y, data = data.frame(x = vecx, y = vecy),
          # color = I('black'), group = 1, 
          geom = 'path') + theme_nothing() +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100))
  }
  if (missing(who))
    who <- ifelse(rnorm(1) > 0, 'calvin', 'hobbes')
  source(system.file('source', 'ch_data.R', package = 'fun'))
  switch(who, 
         calvin = f(p0x, p0y),
         hobbes = f(p1x, p1y))
}

#' Happy numbers
#' 
#' For any positive integer, \code{N}, if the sum of the squares of its
#' digits equals one (or by recursion), then \code{N} is happy \code{:\}}!
#' Otherwise, \code{N} is sad \code{:c}.
#' 
#' @param x a positive integer (any non-digit characters will be ignored)
#' 
#' @examples
#' is.happy(20)
#' is.happy(19)
#' 
#' @export

is.happy <- function(x) {
  sad <- sample(c('>:[',':-(',':(',':-c',':c',':-<',':C',':{',':-[',':[',':{'), 1L)
  hap <- sample(c(':-)',':)',':o)',':]',':3',':)',':}','=]','8)','=)',':}',':^)',':)'), 1L)
  
  x <- gsub('\\D', '', x)
  ok <- (function(x) {
    xx <- as.numeric(strsplit(as.character(x), '')[[1]]) ** 2
    tryCatch(if ((xx <- sum(xx)) == 1) sprintf('happy! %s', hap) else Recall(xx), 
             error = function(e) sprintf('sad %s', sad))
  })(x)
  
  sprintf('%s is %s', x, ok)
}

#' Prime numbers
#' 
#' Check if a number is prime using a regular expression (\code{is.prime}) or
#' by checking that only two integers from \code{1:x} divide \code{x} evenly
#' (\code{is.prime2}).
#' 
#' @param x an integer
#' 
#' @seealso
#' \code{\link{decToUna}}
#' 
#' @references
#' \url{http://montreal.pm.org/tech/neil_kandalgaonkar.shtml}
#' 
#' @examples
#' x <- 1:100
#' x[is.prime(x)]
#' x[is.prime2(x)]
#' 
#' @export

is.prime <- function(x) {
  x <- abs(as.integer(x))
  !grepl('^1?$|^(11+?)\\1+$', decToUna(x))
}

#' @rdname is.prime
#' @export
is.prime2 <- function(x)
  vapply(x, function(y) sum(y / 1:y == y %/% 1:y), integer(1L)) == 2L

#' Nary numbers
#' 
#' Convert to, from, or check if a number is unary or binary.
#' 
#' @param x a vector to be converted or tested
#' 
#' @seealso
#' \code{\link[=intToBits]{rawConversion}}
#' 
#' @examples
#' x <- 1:100
#' x_bin <- decToBin(x)
#' stopifnot(is.binary(x_bin))
#' 
#' x_dec <- binToDec(x_bin)
#' 
#' x_una <- decToUna(x_dec)
#' stopifnot(is.unary(x_una))
#' 
#' stopifnot(identical(x, x_dec))
#' 
#' @name nary
NULL

#' @rdname nary
#' @export
is.unary <- function(x) {
  gsub('.', '1', x) == x
}

#' @rdname nary
#' @export
is.binary <- function(x) {
  sci <- getOption('scipen')
  on.exit(options(scipen = sci))
  options(scipen = 999)
  x <- as.character(x)
  chartr('1', '0', x) == strrep('0', nchar(x))
}

#' @rdname nary
#' @export
binToDec <- Vectorize(function(x) {
  stopifnot(is.binary(x))
  x <- strsplit(as.character(x), '')
  x <- which(rev(unlist(x) == 1L))
  as.integer(sum(2 ^ (x - 1L)))
})

#' @rdname nary
#' @export
decToBin <- Vectorize(function(x) {
  x <- abs(as.integer(x))
  x <- strsplit(paste(rev(intToBits(x))), '')
  x <- paste(sapply(x, `[[`, 2), collapse = '')
  as.integer(gsub('^0*', '', x))
})

#' @rdname nary
#' @export
decToUna <- function(x) {
  s <- sign(x)
  x <- abs(x)
  stopifnot(is.integer(x))
  ## return character instead of integer
  # Warning: NAs introduced by coercion to integer range
  # as.integer(strrep('1', x)) * s
  strrep('1', x)
}

#' Enigma
#' 
#' Encode and decode a message.
#' 
#' @param x a string to encode or decode; alternatively, an object of class
#' \code{"enigma"}
#' @param i indices of the message letters in encoded string
#' @param prob optional probabilities of random letters
#' @param decode logical; if \code{TRUE}, \code{x} is assumed to be encoded
#' 
#' @examples
#' x <- "this is my secret message. Don't tell anyone"
#' set.seed(1)
#' e1 <- enigma(x, c(3, 2, 2, 3))
#' e1
#' enigma(e1)
#' ## alternatively
#' enigma(c(e1), c(3, 2, 2, 3), decode = TRUE)
#' 
#' 
#' ## use observed letter frequencies
#' freq <- read.table(
#'   system.file('scripts', 'letters.txt', package = 'fun'), header = TRUE
#' )
#' freq <- freq[order(freq$Letter), ]
#' 
#' set.seed(1)
#' e2 <- enigma(x, c(3, 2, 2, 3), freq$Count)
#' e2
#' enigma(e2)
#' 
#' 
#' identical(enigma(e1), enigma(e2))
#' table(factor(e1, LETTERS))
#' table(factor(e2, LETTERS))
#' 
#' 
#' ## the code is every third letter
#' e3 <- enigma(x, 3)
#' matrix(e3, 3)[3, ]
#' 
#' @export

enigma <- function(x, i = 1L, prob = NULL, decode = FALSE) {
  if (decode) {
    x <- gsub('[^A-z]', '', paste(x, collapse = ''))
    x <- toupper(strsplit(x, '')[[1L]])
    x <- structure(x, i = i, class = 'enigma')
  }
  
  if (inherits(x, 'enigma')) {
    idx <- cumsum(rep_len(attr(x, 'i'), length(x)))
    return(paste(x[idx[idx <= length(x)]], collapse = ''))
  }
  
  x <- gsub('[^A-z]', '', x)
  x <- toupper(strsplit(x, '')[[1L]])
  
  idx <- cumsum(rep_len(i, length(x)))
  res <- character(max(idx))
  
  res[idx] <- x
  res[-idx] <- sample(LETTERS, sum(!nzchar(res)), TRUE, prob)
  
  structure(res, i = i, class = 'enigma')
}

#' @export
print.enigma <- function(x, ...) {
  print(paste(x, collapse = ''))
  invisible(x)
}

#' <3
#' 
#' \code{<3}
#' 
#' @export

heart <- function(n = 1e4, col = rgb(1, 0, 1, 0.25), ...) {
  ## <3
  set.seed(1)
  r <- 2 / (1 + sqrt(5))
  re <- sqrt(1 - r * r)
  x <- rnorm(n)
  y <- x * r + rnorm(n) * re
  y[x < 0] <- -y[x < 0]
  
  op <- par(mar = rep(0, 4))
  on.exit(par(op))
  plot(x, y, col = col, axes = FALSE, asp = 1, ...)
  
  invisible(list(x = x, y = y))
}

#' the force
#' 
#' the force
#' 
#' @seealso
#' \url{https://twitter.com/rafalab/status/992397723532963841}
#' 
#' @export

the_force <- function() {
  op <- par(bg = 'black', fg = 'white')
  on.exit(par(op))
  
  x <- y <- 0.5
  z <- '|-o-|'
  s <- cbind(runif(50), runif(50))
  m <- c(-1, 1) / 20
  
  while (TRUE){
    plot(0, 0, 'n', 0:1, 0:1, axes = FALSE, ann = FALSE)
    points(s, pch = '.')
    text(x, y, z, cex = 4)
    x <- pmin(pmax(x + sample(m, 1), 0), 1)
    y <- pmin(pmax(y + sample(m, 1), 0), 1)
    Sys.sleep(0.25)
  }
}
