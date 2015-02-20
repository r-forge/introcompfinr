#' @title Summary method of class portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Summary method for objects of class \samp{portfolio}. The output is the same as the \samp{print}.
#' If \samp{risk.free} is specified then the portfolio Sharpe ratio is also returned.
#' 
#' @param object object of class portfolio
#' @param risk.free numeric, risk free rate
#' @param ... additional arguments passed to \samp{summary()}
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
#' summary(equalWeight.portfolio)
#' 
#' @export summary.portfolio

summary.portfolio <-
function(object, risk.free=NULL, ...)
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