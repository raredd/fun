fun
====

misc things and games written in r, collected from everywhere

to install:
```
# install.packages('devtools')
devtools::install_github('raredd/fun')
```

### games included:

to play:            | description
--------------------|------------
`alzheimer_test()`  | test for Alzheimer's disease for peace of mind
`lights_out()`      | turn off all the lights to get some sleep
`mine_sweeper()`    | clear the field of mines to save your legs
`sliding_puzzle()`  | slide the tiles to order the numbers because you're bored
`gomoku()`          | get five tiles in a row for bragging rights (two players)
`witchcraft()`      | remember the sequence of numbers and win a prize (there is no prize)

### other stuff

  * sparkbar
  * R
  * [Collatz conjecture](#collatz-conjecture)
  * [Trace path](#trace-path)

----------------

### collatz conjecture

<div align=center><a href="http://xkcd.com/710/"><img src="http://imgs.xkcd.com/comics/collatz_conjecture.png" style="display: block; margin: auto;" /></a></div>

#### function

```r
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
    cat('\n\nOEIS sequence A006877:', 
        '\nNumbers with a total stopping time longer than any smaller starting value\n', 
        unlist(maxv))
    
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
```

#### examples

```r
collatz(19)
```

```
## 
##    Iterations: 21 
## Maximum value: 88 
## Nth iteration: 4
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/c1.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
collatz(1161)
```

```
## 
##    Iterations: 182 
## Maximum value: 190996 
## Nth iteration: 35
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/c2.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
collatz(1618, stoptime = TRUE)
```

```
## 
##  Max time: 182 
##   Element: 1161
## 
## OEIS sequence A006877: 
## Numbers with a total stopping time longer than any smaller starting value
##  1 2 3 6 7 9 18 25 27 54 73 97 129 171 231 313 327 649 703 871 1161
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/c3.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
collatz(5005, stoptime = TRUE)
```

```
## 
##  Max time: 238 
##   Element: 3711
## 
## OEIS sequence A006877: 
## Numbers with a total stopping time longer than any smaller starting value
##  1 2 3 6 7 9 18 25 27 54 73 97 129 171 231 313 327 649 703 871 1161 2223 2463 2919 3711
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/c4.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

----------------


<a id='trace'></a>

### Trace path

#### function

```r
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
```

#### example plots

```r
trace_path(lens = seq(0, 1,  length.out = 200),
           turn = rep(pi/2 * (-1 + 1/200), 200))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t1.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace_path(lens = seq(1, 10, length.out = 1000),
           turn = rep(2 * pi / 10, 1000))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t2.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace_path(lens = seq(0, 1,  length.out = 500),
           turn = seq(0, pi, length.out = 500))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t3.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace_path(lens = seq(0, 1,  length.out = 600) * c(1, -1),
           turn = seq(0, 8*pi, length.out = 600) * seq(-1, 1, length.out = 200))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t4.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />
