#' Lights out
#'
#' @description
#' By default, the white squares in the plot denote lights on, and black 
#' squares are lights off. When you click on a light, this light as well as 
#' its four neighbors will switch their status. 
#' 
#' Your mission is to turn off all the lights.
#' 
#' @usage
#' lights_out(width = 5, height = 5, steps = 3, cheat = FALSE, 
#'            col.off = 'black', col.on = 'white', 
#'            col.frame = 'lightblue', seed)
#'
#' @param width number of lights in x-axis
#' @param height number of lights in y-axis
#' @param steps number of \code{seed} lights to initialize the puzzle; in 
#' general, the larger \code{steps} is, the more complex the puzzle will be
#' @param cheat logical; if \code{TRUE} a data frame indicating the steps to 
#' solve this puzzle will be printed
#' @param col.off color when lights off
#' @param col.on color when lights on
#' @param col.frame color of lights border
#' @param seed seed for random number generator
#' 
#' @author Yixuan Qiu \email{yixuan.qiu@@cos.name}
#' @note Linux/Mac users have to use \code{X11(type = 'Xlib')} or the
#' Cairo graphics device \code{Cairo()} in the package
#' \pkg{cairoDevice}.
#' @references \url{http://en.wikipedia.org/wiki/Lights_Out_(game)}
#' 
#' @examples
#' lights_out()
#' 
#' @export

lights_out <- function(width = 5, height = 5,
    steps = 3, cheat = FALSE, col.off = 'black', col.on = 'white',
    col.frame = 'lightblue', seed) {
  
    if (!interactive()) 
      return(invisible())
    
    x11()
    zmat <- mat.ini <- matrix(1, height, width)
    trans <- function(z, x, y) {
        nr <- nrow(z)
        nc <- ncol(z)
        mrow <- intersect(1:nr, (x - 1):(x + 1))
        mcol <- intersect(1:nc, (y - 1):(y + 1))
        z[x, y] <- z[x, y] * (-1)
        z[x, mcol] <- z[x, mcol] * (-1)
        z[mrow, y] <- z[mrow, y] * (-1)
        return(z)
    }
    if (!missing(seed)) 
      set.seed(seed)
    grid.x <- sample(1:height, steps, replace = TRUE)
    grid.y <- sample(1:width, steps, replace = TRUE)
    if (cheat) {
        print(data.frame(row = grid.x, col = grid.y))
    }
    for (i in 1:steps) {
        zmat <- trans(zmat, grid.x[i], grid.y[i])
    }
    replot <- function(z) {
        nr <- nrow(z)
        nc <- ncol(z)
        xv <- rep(1:nc, rep(nr, nc))
        yv <- nr + +1 - rep(1:nr, nc)
        color <- ifelse(as.vector(z) == 1, col.off, col.on)
        symbols(xv, yv, rectangles = matrix(1, length(xv), 2),
            inches = FALSE, fg = col.frame, bg = color, add = TRUE)
    }
    par(mar = c(0, 0, 0, 0))
    plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5,
        width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
    replot(zmat)

    mousedown <- function(buttons, x, y) {
        nr <- nrow(zmat)
        nc <- ncol(zmat)
        plx <- round(grconvertX(x, "ndc", "user"))
        ply <- round(grconvertY(y, "ndc", "user"))
        if (plx < 1 | plx > nc | ply < 1 | ply > nr) {
            return(zmat)
        }
        zmat.trans <- trans(zmat, nr - ply + 1, plx)
        replot(zmat.trans)
        return(zmat.trans)
    }

    while (1) {
        if (!any(zmat == -1)) {
            cat("You win!")
            break
        }
        zmat <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
    }
}
