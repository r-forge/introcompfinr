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
