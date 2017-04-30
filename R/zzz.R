.onAttach <- function(...) {
  if (!interactive() || stats::runif(1L) > 0.5)
    invisible(NULL) else fun::R()
}

if (getRversion() >= '2.15.1') {
  utils::globalVariables(c('x', 'y'))
}
