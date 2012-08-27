plot.portfolio <-
function(object, ...)
{
  asset.names <- names(object$weights)
  barplot(object$weights, names=asset.names,
	  xlab="Assets", ylab="Weight", main="Portfolio Weights", ...)
  invisible()
}
