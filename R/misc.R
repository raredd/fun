### miscellaneous stuff
# sparkbar, R
###

#' SparkBar generator
#' 
#' Generates a sparkbar from a sequence of numbers.
#' 
#' @usage
#' spark(...)
#' 
#' @aliases
#' sparkbar, spark
#' 
#' @param ... numerics
#' @seealso \url{https://gist.github.com/ramnathv/7793167}
#' 
#' @examples
#' spark(30, 31, 32, 33)
#' spark(runif(20))
#' 
#' @export

spark <- function(...) {
  
  nums <- c(...)
  min_value <- min(nums)
  max_value <- max(nums)
  value_scale <- max_value - min_value
  zzz <- NULL
  
  for (number in nums) {
    if ((number - min_value) != 0 && (value_scale != 0)) {
      scaled_value <- (number - min_value) / value_scale
    } else {
      scaled_value <- 0
    }
    ## hack:
    ## 9604 and 9608 aren't vertically aligned 
    ## the same as other block elements
    num <- floor(min(6, scaled_value * 7))
    if (num == 3) {
      num <- ifelse ((scaled_value * 7) < 3.5, 2, 4)
    } else if (num == 7) {
      num <- 6
    }
    zzz <- c(zzz, num)
  }
  noquote(intToUtf8(9601 + zzz))
}

#' r
#' 
#' \code{R}
#' 
#' @usage R()
#' 
#' @export

R <- function() {
  eval(quote({h=character;r=rep;a=b=h(0);p=options()$width%/%2-5;n="
  ";j=r(toupper(substring(mode(a),4,4)),sum(r(5:9,2)+1)-3)
  k=r(5:9,2);k[4:5]=7;k=cumsum(k+1);j[k]=n;m=paste(h(1),h(1
  ));s=c(0,k[-10])+1;j[c(16:17,24:26,32:33,46:47,53:55,61:64
  ,70:74)]=m;for(i in 1:10)a=c(a,r(m,p),j[s[i]:k[i]])
  cat(c(n,a),sep=b)}))
}
