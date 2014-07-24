#' Simple memory game
#' 
#' Pick boxes in the correct sequential order to win a prize.
#' 
#' @usage witchcraft(level = 1, seed)
#' 
#' @param level difficulty; choose levels 0 (easy) to 3 (difficult)
#' @param seed seed; see \code{\link{set.seed}}
#' 
#' @return Interactive game
#' @details Set the seed to play the same game.
#' @return A prize.
#' @author Robert Redd 
#' 
#' @examples
#' \dontrun{
#' witchcraft()
#' witchcraft(level = 3)
#' }
#' @export

witchcraft <- function(level = 1, seed) {
  
  stopifnot(is.numeric(level))
  cat('\014')
  level <- as.numeric(cut(level, c(Inf, 3, 2, 1, -Inf))) - 1
  level <- abs(3 - level)
  
  if (!missing(seed))
    set.seed(seed)
  x <- runif(9)
  y <- runif(9)
  
  plot.new()
  par(mfrow = c(1,1))
  plot(x, y, pch = as.character(1:9), 
       xlab = '', ylab = '',  
       xaxt = 'none', yaxt = 'none', bty = 'none',
       main = 'Get ready to fight, you pansy!',
       xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
  Sys.sleep(level)
  plot.new()
  
  for(i in 1:9) {
    plot(x[i:9], y[i:9], xlab = 'Pick one', ylab = '', main = 'Go on...',
         xaxt = 'none', yaxt = 'none', bty = 'none',
         pch = 15, cex = sample(1:9) / 2, col = sample(1:9),
         xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
    
    ans <- identify(x, y, n = 1, plot = FALSE)
    
    if(ans == 9 && i == 9) {
      cat('\014')
      cat('WITCHCRAFT... DIE, DEMON, DIE!\n')
      # smiley face, go again
      plot(c(.47,.4,.4,.55), c(.5,.35,.65,.5), xlab = 'go again!', 
           ylab = '', main = 'cool!', xaxt = 'none', yaxt = 'none', 
           bty = 'none', pch = c(19,19,19,41), cex = c(25,2,2,4), 
           col = c('yellow', rep('black',3)),
           xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
    } else if(ans == i) {
      cat('\014')
      cat('Grrr... lucky guess. \n\n\nGo again :)\n')
    } else if(ans != i) {
      cat('\014')
      cat(matrix('LOL you lose!', 100, ncol = 4))
      # X$ face
      plot(c(.4,.4,.55), c(.35,.65,.5), xlab = 'you have died of dysentery', 
           ylab = '', main = '', xaxt = 'none', yaxt = 'none', bty = 'none',
           pch = c('x','x','{'), cex = 4, col = 'black',
           xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
      break
    }
  }
}
