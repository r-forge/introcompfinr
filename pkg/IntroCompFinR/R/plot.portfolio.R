#' @title Plot method of class portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' The \samp{plot()} method shows a bar chart of the portfolio weights.
#' 
#' @param x object of class portfolio
#' @param ... additional arguments passed to \samp{barplot()}
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
#' plot(equalWeight.portfolio, col="blue")
#' 
#' @export plot.portfolio

plot.portfolio <-
function(x, ...)
{
  asset.names <- names(x$weights)
  barplot(x$weights, names=asset.names,
	  xlab="Assets", ylab="Weight", main="Portfolio Weights", ...)
  invisible()
}