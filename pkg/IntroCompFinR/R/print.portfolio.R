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
