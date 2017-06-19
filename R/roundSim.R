#' Rounding values for presentation
#'
#' \code{roundSim} takes a vector or data.frame object and returns the same
#' type of object, but rounded.
#'
#' @param x A \code{vector} or \code{data.frame} object.
#' @param digits An \code{integer} value indicating the number of digits to round to.
#' @param percent A \code{boolean} value indicating whether column elements should be multiplied by 100.
#'
#' @return A \code{vector} or \code{data.frame} object.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' roundSim(Brown1974) # returns error
#' roundSim(Brown1974[,4:9]) # dataframe input, dataframe output
#' str(roundSim(Brown1974[,4:9])) # vectors are now character-type
#' roundSim(Brown1974[,4:9], 2)
#'
#' set.seed(10)
#' dat <- rnorm(n = 5, mean = 0, sd = 150) # Wider range, vector input
#' roundSim(dat) # vector output
#' }
#'
roundSim <- function(x, digits = 3, percent = FALSE) {
  check <- !(sapply(x, is.numeric))
  if (sum(check) > 0) stop("Pass numeric vectors only.")
  if (percent) x <- x*100
  out <- round(x, digits = digits)
  out <- format(out, scientific = FALSE, drop0trailing = FALSE)
  return(out)
}
