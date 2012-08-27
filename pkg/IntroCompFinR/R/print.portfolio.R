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
