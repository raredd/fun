### miscellaneous stuff
# sparkbar, R, collatz, trace_path, bubble_sort, fibonnaci
###

#' SparkBar generator
#' 
#' Generates a sparkbar from a sequence of numbers.
#' 
#' @usage
#' spark(...)
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
  value_scale <- max_value - min_value
  zzz <- NULL
  
  for (number in nums) {
    if ((number - min_value) != 0 && (value_scale != 0)) {
      scaled_value <- (number - min_value) / value_scale
    } else {
      scaled_value <- 0
    }
    ## hack:
    ## 9604 and 9608 aren't vertically aligned 
    ## the same as other block elements
    num <- floor(min(6, scaled_value * 7))
    if (num == 3) {
      num <- ifelse ((scaled_value * 7) < 3.5, 2, 4)
    } else if (num == 7) {
      num <- 6
    }
    zzz <- c(zzz, num)
  }
  noquote(intToUtf8(9601 + zzz))
}

#' r
#' 
#' \code{R}
#' 
#' @usage R()
#' 
#' @export

R <- function() {
  eval(quote({h=character;r=rep;a=b=h(0);p=options()$width%/%2-5;n="
              ";j=r(toupper(substring(mode(a),4,4)),sum(r(5:9,2)+1)-3)
              k=r(5:9,2);k[4:5]=7;k=cumsum(k+1);j[k]=n;m=paste(h(1),h(1
              ));s=c(0,k[-10])+1;j[c(16:17,24:26,32:33,46:47,53:55,61:64
                                     ,70:74)]=m;for(i in 1:10)a=c(a,r(m,p),j[s[i]:k[i]])
              cat(c(n,a),sep=b)}))
}

#' Collatz conjecture
#' 
#' Perform and visualize the Collatz conjecture.
#' 
#' @usage collatz(n, stoptime = FALSE)
#' 
#' @param n starting number (integer)
#' @param stoptime logical; if \code{FALSE} (default), the value at each
#' iteration is plotted with a summary of the values; if \code{TRUE}, instead,
#' the first \code{n} stopping times are plotted along with the OEIS sequence
#' of numbers \code{<= n}, that is, from one to \code{n}, the number of 
#' iterations each starting value takes to reach 1
#' 
#' @author Robert Redd
#' @references 
#' \url{http://imgs.xkcd.com/comics/collatz_conjecture.png}
#' \url{http://en.wikipedia.org/wiki/Collatz_conjecture}
#' \url{https://oeis.org/A006877}
#' 
#' @examples
#' collatz(19)
#' collatz(1161)
#' collatz(1618, stoptime = TRUE)
#' collatz(5005, stoptime = TRUE)
#' 
#' @export

collatz <- function(n, stoptime = FALSE) {
  
  f <- function(n) {
    iter <- 1
    zzz <- n
    while (n > 1) {
      n <- (n / 2) * (n %% 2 == 0) + (3 * n + 1) * (n %% 2 == 1)
      iter <- iter + 1
      zzz <- c(zzz, n)
    }
    list(zzz = zzz,
         iter = iter,
         max = max(zzz),
         nth = which.max(zzz))
  }
  
  if (stoptime) {
    res <- sapply(1:n, function(x) f(x)$iter)
    cat('\n Max time:', max(res),
        '\n  Element:', which.max(res))
    
    maxv <- c(1, sapply(1:n, function(x) if (res[x] > max(res[1:(x - 1)])) x))
    message('\n\nOEIS sequence A006877: \nNumbers with a total stopping time',
            ' longer than any smaller starting value\n', 
            paste(unlist(maxv), collapse = ', '))
    
    logn <- ifelse(n > 5000, 'x', '')
    plot(1:n, res, las = 1, type = 'p', bty = 'l', log = logn,
         xlab = 'starting value', ylab = 'time', 
         main = paste0('first ', n, ' stopping times'), 
         cex.main = .8, cex.lab = .8)
  } else {
    res <- f(n)
    cat('\n   Iterations:', res$iter, 
        '\nMaximum value:', res$max,
        '\nNth iteration:', res$nth)
    plot(res$zzz, type = 'l', las = 1, bty = 'l',
         xlab = 'iterations', ylab = '',
         main = 'value per iteration', cex.main = .8, cex.lab = .8)
  }
}

#' Trace path
#' 
#' @usage trace_path(lens, turn)
#' 
#' @param lens length, number of points
#' @param turn turns at each value along \code{lens}
#' 
#' @author Robert Redd
#' 
#' @examples
#' trace_path(lens = seq(0, 1,  length.out = 200),
#'            turn = rep(pi/2 * (-1 + 1/200), 200))
#' trace_path(lens = seq(1, 10, length.out = 1000),
#'            turn = rep(2 * pi / 10, 1000))
#' trace_path(lens = seq(0, 1,  length.out = 500),
#'            turn = seq(0, pi, length.out = 500))
#' trace_path(lens = seq(0, 1,  length.out = 600) * c(1, -1),
#'            turn = seq(0, 8*pi, length.out = 600) * seq(-1, 1, length.out = 200))
#' 
#' @export

trace_path <- function(lens, turn) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  facing <- pi / 2 + cumsum(turn)
  move <- lens * exp(1i * facing)
  position <- cumsum(move)
  x <- c(0, Re(position))
  y <- c(0, Im(position))
  plot.new()
  par(mar = c(0, 0, 0, 0))
  plot.window(range(x), range(y))
  lines(x, y)
}

#' Bubble sort
#' 
#' The bubble sort algorithm iteratively sorts pairs of numbers by swapping 
#' each pair if unsorted and repeating until all numbers are properly sorted.
#' 
#' @usage bubble_sort(vec)
#' 
#' @param vec a vector of unsorted integers
#' 
#' @references
#' Modified from 
#' \url{http://www.numbertheory.nl/2013/05/10/bubble-sort-implemented-in-pure-r/}
#' 
#' @examples
#' \dontrun{
#' bubble_sort(round(runif(100, 0, 100)))
#' }
#' @export

bubble_sort <- function(vec) {
  swap_pass <- function(vec) {
    for (i in seq(1, length(vec) - 1)) {
      vec[i:(i + 1)] <- swap_if_larger(vec[i:(i + 1)])
    }
    return(vec)
  }
  swap_if_larger <- function(pair) 
    ifelse (c(larger(pair), larger(pair)), rev(pair), pair)
  larger <- function(pair)
    ifelse (pair[1] > pair[2], TRUE, FALSE)
  f <- function(x) sample(1:length(x), 1)
  
  new_vec <- swap_pass(vec)
  plot.new()
  par(xpd = NA, mar = c(0,0,0,0))
  plot.window(xlim = range(new_vec), ylim = range(new_vec))
  points(new_vec, 1:length(new_vec), pch = 19, cex = .5, col = f(colors()))
  text(x = f(new_vec), y = f(new_vec), labels = '!! bubble sort !!', 
       srt = f(1:360), col = f(colors()), font = 2)
  Sys.sleep(.1)
  if (isTRUE(all.equal(vec, new_vec))) { 
    return(new_vec) 
  } else {
    return(bubble_sort(new_vec))
  }
}

#' Fibonacci's sequence
#' 
#' @usage fibonacci(x)
#' 
#' @param x integer
#' 
#' @examples
#' fibonacci(20)
#' sapply(1:20, function(x) fibonacci(x))
#' 
#' @export

fibonacci <- local({
  memo <- c(1, 1, rep(NA, 100))
  f <- function(x) {
    if(x == 0) 
      return(0)
    if(x < 0) 
      return(NA)
    if(x > length(memo))
      stop('\'x\' is too big for implementation')
    if(!is.na(memo[x])) 
      return(memo[x])
    ans <- f(x - 2) + f(x - 1)
    memo[x] <<- ans
    ans
  }
})

#' Golden ratio
#' 
#' Plots the golden ratio.
#' 
#' @usage golden(theta)
#' @param theta sequence of angles
#' 
#' @examples
#' golden()
#' 
#' @export

golden <- function(theta) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  f <- function(x) 
    ifelse(x == 90, 1, exp(2 * pi / tan(x * 2 * pi / 360)))
  f1 <- function(x)
    ifelse(x == 1, 90, 360 * atan(2 * pi / log(x)) / 2 / pi)
  
  if (missing(theta))
    th <- seq(-100, -9.25, by = .01)
  r <- f(60) ** (th / 2 / pi)
  x <- r * cos(th)
  y <- r * sin(th)
  
  par(ann = FALSE, mar = c(0,0,0,0), bg = 'lightgoldenrod')
  plot(x, y, type = 'l', axes = FALSE)
}