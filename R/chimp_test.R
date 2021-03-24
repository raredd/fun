#' Simple memory game
#' 
#' Pick boxes in sequential order to win a prize. Harder difficulties give
#' less time to memorize the grid and randomize colors after each turn.
#' 
#' @param level level of difficulty, one of \code{"easy"}, \code{"medium"} or
#' \code{"hard"}
#' @param n the length of the test
#' @param seed set a \code{seed} to play the same game
#' 
#' @references
#' \url{https://www.youtube.com/watch?v=zsXP8qeFF6A}
#' 
#' @examples
#' \dontrun{
#' chimp_test()
#' chimp_test('hard', n = 10)
#' }
#' @export

chimp_test <- function(level = c('easy', 'medium', 'hard'), n = 5L, seed = NULL) {
  switch(
    match.arg(level),
    easy = {
      col <- cex <- FALSE
      sleep <- 6
    },
    medium = {
      cex <- TRUE
      col <- FALSE
      sleep <- 4
    },
    hard = {
      col <- cex <- TRUE
      sleep <- 2
    }
  )
  
  set.seed(seed)
  s <- seq.int(n)
  x <- runif(n)
  y <- runif(n)
  
  plot(
    x, y, type = 'n', xlab = '', ylab = '', xlim = 0:1, ylim = 0:1,
    xaxt = 'none', yaxt = 'none', bty = 'none', asp = 1,
    main = 'Are you smarter than a chimp?'
  )
  text(x, y, s, xpd = NA)
  
  f <- function(col = FALSE, cex = FALSE, which = NULL) {
    col <- if (col)
      sample(s) else tail(s, n - i + 1L)
    cex <- if (cex)
      sample(1:5 / 2, n, TRUE) else rep_len(5, n)
    pch <- if (is.null(which))
      rep_len(15L, n) else {
        i <- 1
        cex <- rep_len(5, n)
        col <- replace(rep_len('black', n), which, 'red')
        col[s < which] <- 'green'
        replace(rep_len(1L, n), which, 4L)
      }
    plot(
      x[i:n], y[i:n], xlab = 'Pick one', ylab = '', main = 'Go on...',
      xaxt = 'none', yaxt = 'none', bty = 'none', asp = 1,
      pch = pch[s], cex = cex[s], col = col[s], xlim = 0:1, ylim = 0:1
    )
    if (!is.null(which))
      text(x, y, s, xpd = NA)
  }
  
  Sys.sleep(sleep)
  plot.new()
  palette(sample(palette()))
  on.exit(palette('default'))
  
  for (i in s) {
    f(cex, col)
    ans <- identify(x, y, n = 1L, plot = FALSE)
    
    if (ans == n && i == n) {
      f(which = ans + 1L)
      cat('\014')
      cat('Guess you are as smart as a chimp!\n')
    } else if (ans == i) {
      cat('\014')
      cat('Lucky guess. \n\n\nKeep going...\n')
    } else if (ans != i) {
      f(which = i)
      cat('\014')
      cat('Chimps are much smarter than you!')
      break
    }
  }
}
