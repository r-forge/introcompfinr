summary.Markowitz <-
function(object, risk.free=NULL)
{
  call <- object$call
  asset.names <- colnames(object$weights)
  port.names <- rownames(object$weights)
  if(!is.null(risk.free)) {
    # compute efficient portfolios with a risk-free asset
    nport <- length(object$er)
    sd.max <- object$sd[1]
    sd.e <- seq(from=0,to=sd.max,length=nport)	
    names(sd.e) <- port.names

    #
    # get original er and cov.mat data from call 
    er <- eval(object$call$er)
    cov.mat <- eval(object$call$cov.mat)

    #
    # compute tangency portfolio
    tan.port <- tangency.portfolio(er,cov.mat,risk.free)
    x.t <- sd.e/tan.port$sd		# weights in tangency port
    rf <- 1 - x.t			# weights in t-bills
    er.e <- risk.free + x.t*(tan.port$er - risk.free)
    names(er.e) <- port.names
    we.mat <- x.t %o% tan.port$weights	# rows are efficient portfolios
    dimnames(we.mat) <- list(port.names, asset.names)
    we.mat <- cbind(rf,we.mat) 
  }
  else {
    er.e <- object$er
    sd.e <- object$sd
    we.mat <- object$weights
  }
  ans <- list("call" = call,
	      "er"=er.e,
	      "sd"=sd.e,
	      "weights"=we.mat)
  class(ans) <- "summary.Markowitz"	
  ans
}
