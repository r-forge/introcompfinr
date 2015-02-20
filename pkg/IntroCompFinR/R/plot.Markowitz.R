#' @title Plot method of class Markowitz
#' 
#' @author Eric Zivot
#' 
#' @description
#' Plot efficient frontier. The efficient frontier is a plot of portfolio expected return vs. portfolio
#' standard deviation for a collection of mean-variance efficient portfolios - portfolios that minimize vriance
#' subject to a target expected return.
#' 
#' @param object object of class Markowitz
#' @param plot.assets if \code{TRUE} then plot asset \code{sd} and \code{er} with asset name labels
#' @param ... additional arguments passed to \code{plot()}
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
#' # tangency portfolio
#' tan.port <- tangency.portfolio(er, covmat, r.free)
#' # compute global minimum variance portfolio
#' gmin.port = globalMin.portfolio(er, covmat)
#' 
#' # compute portfolio frontier
#' ef <- efficient.frontier(er, covmat, alpha.min=-2, 
#'                          alpha.max=1.5, nport=20)
#' attributes(ef)
#' 
#' plot(ef)
#' plot(ef, plot.assets=TRUE, col="blue", pch=16)
#' points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
#' points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
#' text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
#' text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)    
#' sr.tan = (tan.port$er - r.free)/tan.port$sd
#' abline(a=r.free, b=sr.tan, col="green", lwd=2)
#' 
#' @export plot.Markowitz

plot.Markowitz <-
function(object, plot.assets=FALSE, ...)
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