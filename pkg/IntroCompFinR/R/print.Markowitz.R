#' @title Print efficient frontier
#' 
#' @author Eric Zivot
#' 
#' @description
#' Print efficient frontier
#' 
#' @param object object of class Markowitz
#' @param ... additional arguments passed to \code{print()}
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
#' # tangency portfolio
#' tan.port <- tangency.portfolio(er, covmat, r.free)
#' # compute global minimum variance portfolio
#' gmin.port = globalMin.portfolio(er, covmat)
#' 
#' # compute portfolio frontier
#' ef <- efficient.frontier(er, covmat, alpha.min=-2, 
#'                          alpha.max=1.5, nport=20)
#' attributes(ef)
#' print(ef)
#' 
#' @export print.Markowitz

print.Markowitz <-
function(object, ...)
{
  cat("Call:\n")
  print(object$call)
  xx <- rbind(object$er,object$sd)
  dimnames(xx)[[1]] <- c("ER","SD")
  cat("\nFrontier portfolios' expected returns and standard deviations\n")
  print(round(xx,4), ...)
  invisible(object)
}