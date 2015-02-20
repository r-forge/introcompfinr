#' @title Compute efficient frontier of risky assets
#' 
#' @author Eric Zivot
#' 
#' @description
#' The function constructs the set of mean-variance efficient portfolios that either allow all
#' assets to be sold short or not allow any asset to be sold short. The returned object is of class
#' \samp{Markowitz} for which there are \samp{print}, \samp{summary} and \samp{plot} methods.
#' 
#' @details 
#' If short sales are allowed (negative weights) then the set of efficient portfolios of risky
#' assets can be computed as a convex combination of any two efficient portfolios. It is convenient
#' to use the global minimum variance portfolio as one portfolio and an efficient portfolio with
#' target expected return equal to the maximum expected return of the assets under consideration as
#' the other portfolio. Call these portfolios \eqn{m} and \eqn{x}, respectively. Then for any number
#' \samp{alpha}, another efficient portfolio can be computed as \eqn{z=\alpha m+(1-\alpha)x}. If
#' short sales are not allowed, then the set of efficient portfolios is computed by repeated calls
#' to the function \samp{efficient.portfolio()}, with \samp{shorts=FALSE}, for a grid of target
#' expected returns starting at the expected return of the global minimum variance portfolio (not
#' allowing short sales) and ending at the expected return equal to the maximum expected return of 
#' the assets under consideration.
#' 
#' @param er \samp{N x 1} vector of expected returns
#' @param cov.mat \samp{N x N} return covariance matrix
#' @param nport scalar, number of efficient portfolios to compute
#' @param alpha.min minimum value of \samp{alpha}, default is \samp{-.5}
#' @param alpha.max maximum value of \samp{alpha}, default is \samp{1.5}
#' @param shorts logical, if \samp{TRUE} then short sales (negative portfolio weights)
#' are allowed. If \samp{FALSE} then no asset is allowed to be sold short
#' 
#' @return 
#'  \item{call}{captures function call}
#'  \item{er}{\samp{nport x 1} vector of expected returns of efficient porfolios}
#'  \item{sd}{\samp{nport x 1} vector of standard deviations of efficient portfolios}
#'  \item{weights}{\samp{nport x N} matrix of weights of efficient portfolios}
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
#' @export efficient.frontier

efficient.frontier <-
function(er, cov.mat, nport=20, alpha.min=-0.5, alpha.max=1.5, shorts=TRUE)
{
  call <- match.call()

  #
  # check for valid inputs
  #
  asset.names <- names(er)
  er <- as.vector(er)
  N <- length(er)
  cov.mat <- as.matrix(cov.mat)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")

  #
  # create portfolio names
  #
  port.names <- rep("port",nport)
  ns <- seq(1,nport)
  port.names <- paste(port.names,ns)

  #
  # compute global minimum variance portfolio
  #
  cov.mat.inv <- solve(cov.mat)
  one.vec <- rep(1, N)
  port.gmin <- globalMin.portfolio(er, cov.mat, shorts)
  w.gmin <- port.gmin$weights

  if(shorts==TRUE){
    # compute efficient frontier as convex combinations of two efficient portfolios
    # 1st efficient port: global min var portfolio
    # 2nd efficient port: min var port with ER = max of ER for all assets
    er.max <- max(er)
    port.max <- efficient.portfolio(er,cov.mat,er.max)
    w.max <- port.max$weights    
    a <- seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
    we.mat <- a %o% w.gmin + (1-a) %o% w.max	         # rows are efficient portfolios
    er.e <- we.mat %*% er							                 # expected returns of efficient portfolios
    er.e <- as.vector(er.e)
  } else if(shorts==FALSE){
    we.mat <- matrix(0, nrow=nport, ncol=N)
    we.mat[1,] <- w.gmin
    we.mat[nport, which.max(er)] <- 1
    er.e <- as.vector(seq(from=port.gmin$er, to=max(er), length=nport))
    for(i in 2:(nport-1)) 
      we.mat[i,] <- efficient.portfolio(er, cov.mat, er.e[i], shorts)$weights
  } else {
    stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
  }
  
  names(er.e) <- port.names
  cov.e <- we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
  sd.e <- sqrt(diag(cov.e))					        # std devs of efficient portfolios
  sd.e <- as.vector(sd.e)
  names(sd.e) <- port.names
  dimnames(we.mat) <- list(port.names,asset.names)

  # 
  # summarize results
  #
  ans <- list("call" = call,
	      "er" = er.e,
	      "sd" = sd.e,
	      "weights" = we.mat)
  class(ans) <- "Markowitz"
  ans
}