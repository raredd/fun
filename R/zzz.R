.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > .5)
    return(invisible()) else R()
}

if (getRversion() >= '2.15.1') {
  utils::globalVariables(c('x','y'))
}
