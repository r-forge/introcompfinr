print.summary.Markowitz <-
function(x, ...)
{
	xx <- rbind(x$er,x$sd)
	port.names <- names(x$er)
	asset.names <- colnames(x$weights)
	dimnames(xx)[[1]] <- c("ER","SD")
	cat("Frontier portfolios' expected returns and standard deviations\n")
	print(round(xx,4), ...)
	cat("\nPortfolio weights:\n")
	print(round(x$weights,4), ...)
	invisible(x)
}
