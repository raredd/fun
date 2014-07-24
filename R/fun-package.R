#' fun with R
#'
#' This is a collection of games and other stuff written in R.
#' 
#' @name fun-package
#' @docType package
NULL

.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > .5)
    return(invisible())
  else
    R()
}