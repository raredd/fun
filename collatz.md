
#### collatz conjecture


```r
library(knitr)
opts_chunk$set(cache = FALSE, tidy = FALSE, echo = TRUE,
               dev = 'CairoPNG', 
               dev.args = list(antialias = 'none', bg = 'transparent'), 
               dpi = 150, fig.align = 'center',
               fig.width = 9, fig.height = 6)
```

<a href="http://xkcd.com/710/"><img src="http://imgs.xkcd.com/comics/collatz_conjecture.png" style="display: block; margin: auto;"/></a>

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

<img src="./collatz_files/figure-html/examples1.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
collatz(1161)
```

```
## 
##    Iterations: 182 
## Maximum value: 190996 
## Nth iteration: 35
```

<img src="./collatz_files/figure-html/examples2.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

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

<img src="./collatz_files/figure-html/examples3.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

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

<img src="./collatz_files/figure-html/examples4.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />
