#' @title Print method of class portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Print method of class portfolio.
#' 
#' @param object object of class portfolio
#' @param ... controlled variables for \code{print()}
#' 
#' @examples
#' asset.names = c("MSFT", "NORD", "SBUX")
#' er = c(0.0427, 0.0015, 0.0285)
#' names(er) = asset.names
#' covmat = matrix(c(0.0100, 0.0018, 0.0011,
#'                   0.0018, 0.0109, 0.0026,
#'                   0.0011, 0.0026, 0.0199),
#'                 nrow=3, ncol=3)
#' r.free = 0.005
#' dimnames(covmat) = list(asset.names, asset.names)
#' 
#' # compute equally weighted portfolio
#' ew = rep(1,3)/3
#' equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
#' print(equalWeight.portfolio)

print.portfolio <-
function(object, ...)
{
  cat("Call:\n")
  print(object$call, ...)
  cat("\nPortfolio expected return:    ", format(object$er, ...), "\n")
  cat("Portfolio standard deviation: ", format(object$sd, ...), "\n")
  cat("Portfolio weights:\n")
  print(round(object$weights,4), ...)
  invisible(object)
}