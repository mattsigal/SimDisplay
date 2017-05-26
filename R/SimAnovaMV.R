#' Decomposing multivariate simulation results into ANOVA-based effects sizes
#'
#' This function is an extension of \code{SimAnova}, which takes an output object from
#' a simulation, as well as a vector for the design factors and outcome measures. If
#' there is only one design factor, then the result is a dataframe with rows pertaining
#' to each outcome measure. If there is more than one design factor, this function
#' automatically conducts a full-factorial design, and outputs a list with elements
#' corresponding to each main and interaction effect.
#'
#' @param data A \code{data.frame} object, specifically of class \code{SimDesign}.
#' @param terms A \code{character} vector indicating the design factors to summarise.
#' @param responses A \code{character} vector indicating the response variables produced in the simulation.
#'
#' @return A \code{data.frame} or \code{list} object.
#'
#' @seealso \code{\link{SimAnova}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974) # 3 design factors, 6 response variables
#' TypeI <- subset(Brown1974, var_ratio == 1)
#' Power <- subset(Brown1974, var_ratio != 1)
#' tvals <- c("distribution")
#' rvals <- c("F", "Jacknife", "Layard", "Levene", "W10", "W50")
#' SimAnovaMV(data = TypeI, terms = tvals, responses = rvals)
#' SimAnovaMV(data = Power, terms = tvals, responses = rvals)
#'
#' data(tsimresults) # 3 design factors, 2 response variables
#' tvals <- c("group_size_ratio", "standard_deviation_ratio")
#' rvals <- c("lessthan.05.welch", "lessthan.05.independent")
#' SimAnovaMV(data = tsimresults, terms = tvals, responses = rvals)
#' }
#'
SimAnovaMV <- function(data, terms, responses){

  if (length(terms) == 1){
    out <- data.frame(matrix(NA, nrow = length(responses), ncol = 4))

    for (i in 1:length(responses)){
      form <- reformulate(termlabels = terms,
                          response = responses[i])
      out[i,1] <- responses[i]
      out[i,2] <- SimAnova(form, dat = data)$F[1]
      out[i,3] <- SimAnova(form, dat = data)$p[1]
      out[i,4] <- SimAnova(form, dat = data)$eta.sq[1]
    }
    colnames(out) <- c("Method", "F", "p", "eta.sq")
    return(out)
  }

  if (length(terms) > 1){
    out <- list()
    nterms <- length(terms) + (2^length(terms) - length(terms) - 1)
    # nterms is the number of main effects and interactions in the model
    for (i in 1:nterms){
      tmp <- data.frame(matrix(NA, nrow = length(responses), ncol = 4))
      for (j in 1:length(responses)){
        form <- as.formula(paste0(responses[j], " ~ (", paste0(terms, collapse = "+"),")^", length(terms)))
        tmp[j,1] <- responses[j]
        Sresults <- SimAnova(form, dat = data)
        tmp[j,2] <- Sresults$F[i]
        tmp[j,3] <- Sresults$p[i]
        tmp[j,4] <- Sresults$eta.sq[i]
      }
      colnames(tmp) <- c("Method", "F", "p", "eta.sq")
      out[[i]] <- tmp
    }
    names(out) <- row.names(Sresults)[1:length(row.names(Sresults))-1]
    return(out)
  }
}
