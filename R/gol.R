### game of life
# play_gol, gol_step, waffle, plot.gol
###
# see ./inst/source/gol_special.R for data


#' Conway's Game of Life
#' 
#' A simulation of cellular automaton devised by mathematician John Horton
#' Conway. The life cycle is determined by the initial state matrix,
#' \code{mat}, and will evolve for \code{gen} generations. \code{plot.gol}
#' loops through all generation states to display the evolution over time.
#' 
#' Conway's Game of Life, or Life, consists of a grid of cells which have two
#' states at initiation, alive or dead, represented by 1s and 0s, respectively.
#' The evolution of these cells is determined by the eight neighboring cells
#' according to the following rules which are applied simultaneously to each
#' cell:
#' 
#' \enumerate{
#' \item{Any live cell with fewer than two live neighbours dies, as if caused
#' by under-population.}
#' \item{Any live cell with two or three live neighbours lives on to the next
#' generation.}
#' \item{Any live cell with more than three live neighbours dies, as if by
#' over-population.}
#' \item{Any dead cell with exactly three live neighbours becomes a live cell,
#' as if by reproduction.}
#' }
#' 
#' This function has two methods to calculate the neighbors for individual
#' cells. The default method (\code{rotate = TRUE}) uses a matrix rotation and
#' is much faster than the looping method (\code{rotate = FALSE}).
#' 
#' If \code{scale} is \code{TRUE}, the neighbor count matrix is scaled to
#' \code{[0,1]} giving a smoother distribution of the neighbor count. This is
#' useful when \code{col} is given as a vector of three or more colors which
#' is converted to a continuous scale via \code{\link{colorRampPalette}}.
#' 
#' The resulting plot is created with \code{\link[rawr]{waffle}}, a non
#' exported function which can be accessed using \code{fun:::waffle}. Full
#' documentation and usage can be found in the \pkg{rawr} package version.
#' 
#' Additionally, a custom rule function can be passed using the \code{rules}
#' argument. This function should have three arguments: an integer matrix
#' representing the current cell state, an integer matrix of the neighbor
#' count of these cells, and and integer representing the iteration count.
#' The function should apply a set of rules and return an integer matrix
#' having the same dimensions as the initial state.
#' 
#' Note that this function will be checked for input/output consistency only,
#' so all some or all of these parameters may be ignored so long as the check
#' passes; see examples.
#' 
#' @param mat a \code{{0,1}} integer matrix
#' @param gen number of generations to simulate
#' @param rotate logical; neighbor calculator; see details
#' @param scale logical; neighbor scaling; see details
#' @param rules character string specifying the rule set to use; choices are
#' \code{'conway'}, \code{'life_without_death'}, and \code{'day_and_night'};
#' alternatively, a rule function can be given, see details
#' @param x \code{gol} object, the result of \code{play_gol}
#' @param col a vector of two or more colors if \code{scale} is \code{TRUE};
#' otherwise, a vector of length two with the colors for live and dead cells,
#' respectively
#' @param time length of pause between generations
#' @param ... additional parameters passed to \code{\link[rawr]{waffle}} or
#' further to \code{\link{par}}; see details
#' 
#' @references
#' \url{https://en.wikipedia.org/wiki/Conway\%27s_Game_of_Life}
#' 
#' @examples
#' \dontrun{
#' 
#' set.seed(1)
#' n <- 150
#' m <- matrix(rbinom(n * n, 1, 0.3), n)
#' plot(play_gol(m))
#' 
#' plot(play_gol(m, rotate = TRUE, scale = TRUE),
#'      col = c('white','red','yellow','blue','white'))
#' 
#' ## this system file contains some special cases
#' source(system.file('source', 'gol_special.R', package = 'fun'))
#' 
#' plot(play_gol(death))
#' plot(play_gol(rowof10))
#' plot(play_gol(big_exploder))
#' plot(play_gol(small_exploder))
#' plot(play_gol(glider, 50))
#' plot(play_gol(invader, 20))
#' plot(play_gol(tumbler))
#' plot(play_gol(glider_gun, 200))
#' 
#' ## alternative rules
#' plot(play_gol(ladder, rules = 'life_without_death'))
#' plot(play_gol(glider, 50, rules = 'day_and_night'))
#' plot(play_gol(bowtie, 30, rules = 'high_life'))
#' 
#' 
#' ## custom rules
#' my_rule <- function(X, ...) {
#'   x <- c(X)
#'   y <- fun:::rotate_(X, FALSE)
#'   x[sample(seq_along(x), max(y))] <- 0  ## kill random cells
#'   matrix(x, nrow(X))
#' }
#' plot(play_gol(matrix(1, 10, 10), 50, rules = my_rule))
#' 
#' 
#' ## rule using time (current iteration)
#' my_rule <- function(X, Y, Z) {
#'   x <- c(X)
#'   y <- fun:::rotate_(X, FALSE)
#'   z <- Z %% 5 == 0L
#'   x[sample(seq_along(x), max(y))] <- 0  ## kill random cells
#'   x[sample(seq_along(x), 5)]      <- 1  ## spontaneous combustion
#'   matrix(x, nrow(X))
#' }
#' plot(play_gol(matrix(1, 10, 10), 50, rules = my_rule))
#' 
#' 
#' ## rule ignoring inputs
#' my_rule <- function(...) {
#'   x <- matrix(sample(c(..1)), nrow(..1))
#'   x[sample(seq.int(nrow(x)), 1), ] <- 0
#'   x
#' }
#' plot(play_gol(matrix(1, 10, 10), 20, rules = my_rule))
#' 
#' }
#' 
#' @aliases gol
#' @name game_of_life
NULL

#' @rdname game_of_life
#' @export
play_gol <- function(mat, gen = max(dim(mat)), rotate = TRUE, scale  = FALSE,
                     rules = 'conway') {
  stopifnot(is.matrix(mat))
  stopifnot(all(mat %in% 0:1))
  
  RULES <- if (is.function(rules)) {
    if (any(dim(mat) != dim(rules(mat, rep(0, length(mat)), NULL))))
      stop('Improper rule function')
    rules
  } else {
    match.arg(rules, c('conway','life_without_death','day_and_night',
                       'high_life'),
              several.ok = FALSE)
  }
  
  ii <- 1
  life <- array(dim = c(dim(mat), gen + 1))
  life[,, 1] <- mat
  
  cat('\nEvolving\n')
  pb <- txtProgressBar(max = gen, style = 3, title = 'Evolving')
  while (ii <= gen) {
    setTxtProgressBar(pb, ii, title = 'Evolving')
    life[,, ii + 1] <- mat <- gol_step(mat, rotate, scale, RULES, ii)
    ii <- ii + 1
  }
  close(pb)
  
  life <- structure(list(life = life, scaled = scale, rules = RULES),
                    class = 'gol')
  invisible(life)
}

gol_step <- function(mat, rotate = TRUE, scale = FALSE, rules, iteration) {
  RULES <- if (is.function(rules))
    rules else switch(rules,
                      conway = rules_conway_,
                      life_without_death = rules_lwod_,
                      day_and_night = rules_dn_,
                      high_life = rules_hl_,
                      stop('Invalid rule function'))
  if (rotate) {
    if (scale)
      return(rotate_(mat, TRUE))
    RULES(mat, rotate_(mat, FALSE), iteration)
  } else {
    if (scale)
      return(matrix(all_neighbors(mat), nrow(mat)))
    RULES(mat, all_neighbors(mat), iteration)
  } 
}

waffle <- function(mat, xpad = 0, ypad = 0, asp = 1, ..., reset_par = TRUE) {
  op <- par(no.readonly = TRUE)
  if (reset_par) on.exit(par(op))
  plot.new()
  par(list(...))
  
  o <- cbind(c(row(mat)), c(col(mat))) - 1
  psum <- function(...) rowSums(do.call('cbind', list(...)))
  
  plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
              xaxs = 'i', yaxs = 'i', asp = asp)
  rect(xl <- o[, 2], yb <- o[, 1], xr <- o[, 2] + (1 - xpad),
       yt <- o[, 1] + (1 - ypad), col = c(mat), border = NA)
  invisible(list(matrix = mat, origin = `colnames<-`(o[, 2:1], c('x','y')),
                 centers = cbind(x = psum(xl, xr) / 2, y = psum(yb, yt) / 2)))
}

#' @rdname game_of_life
#' @export
plot.gol <- function(x, col, time = 0.1, ...) {
  stopifnot(inherits(x, 'gol'))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(1,1,1,1))
  
  col <- if (missing(col))
    c('white','black') else
      if (length(col) > 2L) colorRampPalette(col)(1000) else col
  X <- x$life
  lx <- dim(X)[3]
  
  cat('\nPlotting\n')
  pb <- txtProgressBar(max = lx, style = 3, title = 'Plotting')
  for (ii in seq.int(lx)) {
    setTxtProgressBar(pb, ii, title = 'Plotting')
    Sys.sleep(time)
    y <- if (x$scaled)
      matrix(col[round(X[,, ii] * 1000 + 1L)], nrow(X[,, ii])) else
        matrix(col[X[,, ii] + 1L], nrow(X[,, ii]))
    waffle(y, ...)
  }
  close(pb)
  
  invisible(NULL)
}

## rule sets
rules_conway_ <- function(X, Y, Z) {
  ## x,y integer matrices of alive/dead (x) and neighbor count (y)
  stopifnot(length(x <- c(X)) == length(y <- c(Y)))
  xl <- as.logical(x)       ## poor, wretched souls
  
  x[xl  & y < 2]      <- 0  ## under-population
  x[xl  & y %in% 2:3] <- 1  ## x lives
  x[!xl & y == 3]     <- 1  ## reproduction
  x[xl  & y >= 4]     <- 0  ## over-population
  
  matrix(x, nrow(X))
}

rules_hl_ <- function(X, Y, Z) {
  stopifnot(length(x <- c(X)) == length(y <- c(Y)))
  xl <- as.logical(x)
  
  x[xl  & y < 2]      <- 0  ## under-population
  x[xl  & y %in% 2:3] <- 1  ## x lives
  x[!xl & y == 3]     <- 1  ## reproduction
  x[xl  & y >= 4]     <- 0  ## over-population
  x[!xl & y == 6]     <- 1  ## additional rule to conway for hl
  
  matrix(x, nrow(X))
}

rules_lwod_ <- function(X, Y, Z) {
  stopifnot(length(x <- c(X)) == length(y <- c(Y)))
  xl <- as.logical(x)
  x[xl  & y %in% 2:3] <- 1
  x[!xl & y == 3]     <- 1
  matrix(x, nrow(X))
}

rules_dn_ <- function(X, Y, Z) {
  stopifnot(length(x <- c(X)) == length(y <- c(Y)))
  xl <- as.logical(x)
  x[!xl & y %in% c(3,6:8)]   <- 1
  x[xl  & y %in% c(3:4,6:8)] <- 1
  matrix(x, nrow(X))
}

rotate_ <- function(mat, scale = scale) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  padr <- rep(0, nr)
  padc <- rep(0, nc)
  
  l <- list(
    u = rbind(padc, mat[-nr, ]),
    d = rbind(mat[-1, ], padc),
    l = cbind(padr, mat[, -nc]),
    r = cbind(mat[, -1], padr),
    
    ul = rbind(padc, cbind(padr[-1], mat[-nr, -nc])),
    ur = rbind(padc, cbind(mat[-nr, -1], padr[-1])),
    dl = rbind(cbind(padr[-1], mat[-1, -nc]), padc),
    dr = rbind(cbind(mat[-1, -1], padr[-1]), padc)
  )
  rot <- `dimnames<-`(Reduce('+', l), NULL)
  if (scale)
    rot / max(rot) else rot
}

## rotate_ does the job of these three fns which are slow af
dir <- list(u = c(-1, 0), d = c(1, 0), l = c(0, -1), r = c(0, 1))
dir <- c(dir, list(ul = c(-1,-1), ur = c(-1,1), dl = c(1,-1), dr = c(1,1)))
try0 <- function(x)
  tryCatch(if (length(x)) as.integer(x) else 0L, error = function(e) 0L)

one_neighbor <- function(mat, rc) {
  ## get count of neighbors for one (rc = c(row_index, column_index))
  # one_neighbor(mat, c(2,4))
  nn <- vapply(seq_along(dir), function(x) {
    d <- dir[[x]]
    try0(mat[rc[1] + d[1], rc[2] + d[2]])
  }, integer(1))
  sum(nn)
}

all_neighbors <- function(mat) {
  ## get count of neighbors for all
  idx <- cbind(c(row(mat)), c(col(mat)))
  m <- vapply(1:nrow(idx), function(x)
    one_neighbor(mat, idx[x, ]), integer(1))
  matrix(m, nrow(mat))
}
