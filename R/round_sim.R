#' Rounding values for presentation
#'
#' \code{round_sim} takes a vector or data.frame object and returns the same
#' type of object, but rounded
#'
#' @param x A \code{vector} or \code{data.frame} object.
#' @param digits An \code{integer} value indicating the number of digits to round to.
#'
#' @return A \code{vector} or \code{data.frame} object.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' round_sim(Brown1974) # returns error
#' round_sim(Brown1974[,5:9]) # dataframe input, dataframe output
#' str(round_sim(Brown1974[,5:9])) # vectors are now character-type
#' round_sim(Brown1974[,5:9], 2)
#'
#' dat <- rnorm(n = 50, mean = 0, sd = 150) # Wider range, vector input
#' round_sim(dat) # vector output
#' }
#'
round_sim <- function(x, digits = 3) {
  check <- !(sapply(x, is.numeric))
  if (sum(check) > 0) stop("Pass numeric vectors only.")
  out <- round(x, digits = digits)
  out <- format(out, scientific = FALSE, drop0trailing = FALSE)
  return(out)
}