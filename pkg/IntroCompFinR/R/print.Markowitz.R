print.Markowitz <-
function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  xx <- rbind(x$er,x$sd)
  dimnames(xx)[[1]] <- c("ER","SD")
  cat("\nFrontier portfolios' expected returns and standard deviations\n")
  print(round(xx,4), ...)
  invisible(x)
}
