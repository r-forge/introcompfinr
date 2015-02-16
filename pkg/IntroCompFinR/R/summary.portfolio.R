#' @title Summary method of class portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Summary method of class portfolio.
#' 
#' @param object object of class portfolio
#' @param risk.free numeric, risk free rate
#' @param ... controlled variables for \code{summary()}
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
#' summary(equalWeight.portfolio)

summary.portfolio <-
function(object, risk.free=NULL, ...)
# risk.free			risk-free rate. If not null then
#				compute and print Sharpe ratio
# 
{
  cat("Call:\n")
  print(object$call)
  cat("\nPortfolio expected return:    ", format(object$er, ...), "\n")
  cat(  "Portfolio standard deviation: ", format(object$sd, ...), "\n")
  if(!is.null(risk.free)) {
    SharpeRatio <- (object$er - risk.free)/object$sd
    cat("Portfolio Sharpe Ratio:       ", format(SharpeRatio), "\n")
  }
  cat("Portfolio weights:\n")
  print(round(object$weights,4), ...)
  invisible(object)
}