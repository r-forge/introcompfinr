#' @title Summary method of class Markowitz
#' 
#' @author Eric Zivot
#' 
#' @description
#' Summary method for objects of class \samp{Markowitz}. For all portfolios on the efficient
#' frontier, the expected return, standard deviation and asset weights are shown. If
#' \samp{risk.free} is given then efficient portfolios that are combinations of the risk free asset
#' and the tangency portfolio are computed. The class \samp{summary.Markozitz} will be created.
#' 
#' @param object object of class Markowitz
#' @param risk.free numeric, risk free rate
#' @param ... additional arguments passed to \samp{summary()}
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
#' summary(ef)
#' 
#' @export summary.Markowitz

summary.Markowitz <-
function(object, risk.free=NULL, ...)
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
    x.t <- sd.e/tan.port$sd		                        # weights in tangency port
    rf <- 1 - x.t			                                # weights in t-bills
    er.e <- risk.free + x.t*(tan.port$er - risk.free)
    names(er.e) <- port.names
    we.mat <- x.t %o% tan.port$weights	              # rows are efficient portfolios
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