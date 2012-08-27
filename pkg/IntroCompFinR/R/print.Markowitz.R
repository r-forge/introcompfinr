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
