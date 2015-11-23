### utils
# progress
###

## rawr::progress
progress <- function(value, max.value, textbar = FALSE) {
  oo <- options()
  on.exit(options(oo))
  options(scipen = 10)
  if (!is.numeric(value)) 
    stop("\'value\' must be numeric")
  if (missing(max.value)) {
    max.value <- 100
    percent <- TRUE
  } else percent <- FALSE
  
  f <- function(...) paste0(..., collapse = '')
  erase.only <- value > max.value
  max.value <- as.character(round(max.value))
  l <- nchar(max.value)
  # value <- formatC(round(value), width = l, format = 'd')
  # max.value <- formatC(max.value, width = l, format = 'd')
  
  if (textbar) {
    # m <- getOption('width')
    # r <- floor(as.numeric(value) / as.numeric(max.value) * m)
    # backspaces <- f(rep('\b', m * 2))
    #
    # if (erase.only) message <- ''
    # else {
    #   message <- f('|', f(rep('=', max(0, r - 1))),
    #                f(rep(' ', max(0, m - r))), '|')
    #   cat(backspaces, message, sep = '')
    # }
    m <- getOption('width') - 5
    pct <- as.numeric(value) / as.numeric(max.value)
    r <- floor(pct * m)
    backspaces <- f(rep('\b', m * 2))
    if (erase.only) message <- ''
    else {
      message <- f('|', f(rep('=', max(0, r - 1))), 
                   f(rep(' ', max(0, m - r))), '|')
      cat(backspaces, message, sprintf('  %s%%', round(pct * 100)), sep = '')
    }
  } else {
    if (percent) {
      backspaces <- f(rep('\b', l + 14))
      if (erase.only) message <- ''
      else message <- paste0('Progress: ', value, '%  ')
      cat(backspaces, message, sep = '')
    } else {
      backspaces <- f(rep('\b', 2 * l + 17))
      if (erase.only) message <- ''
      else message <- f('Progress: ', value, ' of ', max.value, '  ')
      cat(backspaces, message, sep = '')
    }
  }
  if (.Platform$OS.type == 'windows') 
    flush.console()
  cat('\n')
}
