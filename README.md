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
`chimp_test()`      | https://www.youtube.com/watch?v=zsXP8qeFF6A

### other stuff

  * sparkbar
  * R
  * [Collatz conjecture](#collatz-conjecture)
  * [Trace path](#trace-path)
  * [Bubble sort](#bubble-sort)
  * [Calvin and Hobbes](#calvin-and-hobbes)
  * [Happy numbers](#happy-numbers)
  * [Game of Life](#game-of-life)

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

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/c1.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
collatz(1161)
```

```
## 
##    Iterations: 182 
## Maximum value: 190996 
## Nth iteration: 35
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/c2.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

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

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/c3.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

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

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/c4.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

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

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/t1.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace_path(lens = seq(1, 10, length.out = 1000),
           turn = rep(2 * pi / 10, 1000))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/t2.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace_path(lens = seq(0, 1,  length.out = 500),
           turn = seq(0, pi, length.out = 500))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/t3.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace_path(lens = seq(0, 1,  length.out = 600) * c(1, -1),
           turn = seq(0, 8*pi, length.out = 600) * seq(-1, 1, length.out = 200))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/t4.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />


#### (the following have absolutely no use, I agree)

### Bubble sort

```r
set.seed(1)
bubble_sort(runif(100), plot = TRUE)
```

<div align=center><img src="https://raw.githubusercontent.com/raredd/fun/master/inst/animate/bubble_sort/bubble_sort.gif" title="bubble sort example" alt="bubble sort example" style="display: block; margin: auto;" /></div>

### Calvin and Hobbes

```r
ch('calvin')
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/c.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
ch('hobbes')
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/figs/h.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

### Happy numbers

Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number either equals 1 (where it will stay), or it loops endlessly in a cycle that does not include 1.

Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers (or sad numbers).

https://en.wikipedia.org/wiki/Happy_number

```r
is.happy(20)
```

```
[1] "20 is sad :["
```

```r
is.happy(19)
```

```
[1] "19 is happy! :}"
```

### Game of Life

An implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). Using the `rules` argument, one can specify a rule set for the cellular automation or supply a custom function.

```r
## load some initial states
source(system.file('source', 'gol_special.R', package = 'fun'))

## simulate life
plot(play_gol(gol_special$glider_gun, 200))
```

<div align=center>
<img src="https://raw.githubusercontent.com/raredd/fun/master/inst/animate/gol/glider_gun.gif" title="glider gun example" alt="glider gun example" style="display: block; margin: auto;" />
</div>

This package also provides several special cases of initial states for the built-in rule functions:

```r
names(gol_special)
```

```
[1] "big_exploder"   "bowtie"    "death"          "glider"      "glider_gun"     "gosper_gun"     "invader"
[8] "ladder"         "rowof10"   "small_exploder" "tumbler"
```

|Rule set           |package option     |Description                                                                                               |
|:------------------|:------------------|:---------------------------------------------------------------------------------------------------------|
|Conway             |`"conway"`         |default rules                                                                                             |
|Highlife           |`"highlife"`       |similar to Conway with an additional rule that dead cells with six neighbors are born                     |
|Life without death |`"life_without_death"` |Conway's rules without death                                                                              |
|Day and night      |`"day_and_night"`  |dead cells with 3, 6, 7, or 8 neighbors are born, and alive cells with 3, 4, 6, 7, or 8 neighbors survive |
