
trace path


```r
library(knitr)
opts_chunk$set(cache = FALSE, tidy = FALSE, echo = TRUE,
               dev = 'CairoPNG', 
               dev.args = list(antialias = 'none', bg = 'transparent'), 
               dpi = 150, fig.align = 'center')
```

#### function


```r
trace.path <- function(lens, turn) {
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
trace.path(lens = seq(0, 1,  length.out = 200),
           turn = rep(pi/2 * (-1 + 1/200), 200))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t1.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace.path(lens = seq(1, 10, length.out = 1000),
           turn = rep(2 * pi / 10, 1000))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t2.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace.path(lens = seq(0, 1,  length.out = 500),
           turn = seq(0, pi, length.out = 500))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t3.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />

```r
trace.path(lens = seq(0, 1,  length.out = 600) * c(1, -1),
           turn = seq(0, 8*pi, length.out = 600) * seq(-1, 1, length.out = 200))
```

<img src="https://raw.githubusercontent.com/raredd/fun/master/figs/t4.png" title="plot of chunk examples" alt="plot of chunk examples" style="display: block; margin: auto;" />
