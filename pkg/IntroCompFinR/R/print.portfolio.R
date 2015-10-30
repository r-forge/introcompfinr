#' @title Print method of class portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Print method for objects of class \samp{portfolio}.
#' 
#' @param x object of class portfolio
#' @param ... additional arguments passed to \samp{print()}
#' 
#' @examples
#' # construct the data
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
#' 
#' @export

print.portfolio <-
function(x, ...)
{
  cat("Call:\n")
  print(x$call, ...)
  cat("\nPortfolio expected return:    ", format(x$er, ...), "\n")
  cat("Portfolio standard deviation: ", format(x$sd, ...), "\n")
  cat("Portfolio weights:\n")
  print(round(x$weights,4), ...)
  invisible(x)
}