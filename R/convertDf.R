#' Convert to SimDesign Object
#'
#' \code{convertDf} is a helper function that
#' can convert a \code{data.frame} into a \code{SimDesign} object.
#'
#' @param x A \code{data.frame} object, specifically with simulation design
#' variables specified as factors, and simulation results as numeric.
#'
#' @return A \code{data.frame} object also of class \code{SimDesign}.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' n <- c(15, 30, 50)
#' sd <- c(0.5, 1, 2)
#' skew <- c(1, 5, 10)
#' dat <- expand.grid(N = n, SD = sd, Skew = skew,
#'                    KEEP.OUT.ATTRS = FALSE)
#' set.seed(123)
#' dat$Result1 <- rnorm(27)
#' dat$Result2 <- rnorm(27)
#'
#' convertDf(dat) # Error: N, SD, and Skew not set as factors
#'
#' dat[,1:3] <- lapply(dat[,1:3], as.factor)
#' newdat <- convert_df(dat)
#' str(newdat)
#' }
#'
convertDf <- function(x){
  x <- as.data.frame(x)
  is.fact <- sapply(x, is.factor)
  attributes(x)$design_names$design <- names(is.fact[is.fact == TRUE])

  if(sum(is.fact) == 0){
    stop("No factors found in dataframe.")
  }

  is.sim <- sapply(x, is.numeric)
  attributes(x)$design_names$sim <- names(is.sim[is.sim == TRUE])

  if(sum(is.sim) == 0){
    stop("No numeric results found in dataframe.")
  }

  class(x) <- c("SimDesign", "data.frame")
  return(x)
}
