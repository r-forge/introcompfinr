plot.Markowitz <-
function(object, plot.assets=FALSE, ...)
# plot.assets		logical. If true then plot asset sd and er
{
  if (!plot.assets) {
     y.lim=c(0,max(object$er))
     x.lim=c(0,max(object$sd))
     plot(object$sd,object$er,type="b",xlim=x.lim, ylim=y.lim,
          xlab="Portfolio SD", ylab="Portfolio ER", 
          main="Efficient Frontier", ...)
     }
  else {
	  call = object$call
	  mu.vals = eval(call$er)
	  sd.vals = sqrt( diag( eval(call$cov.mat) ) )
	  y.lim = range(c(0,mu.vals,object$er))
	  x.lim = range(c(0,sd.vals,object$sd))
	  plot(object$sd,object$er,type="b", xlim=x.lim, ylim=y.lim,
          xlab="Portfolio SD", ylab="Portfolio ER", 
          main="Efficient Frontier", ...)
        text(sd.vals, mu.vals, labels=names(mu.vals))
  }
  invisible()
}
