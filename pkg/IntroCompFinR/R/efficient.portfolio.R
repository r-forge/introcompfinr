#' @title Compute minimum variance portfolio subject to target return
#' 
#' @author Eric Zivot
#' 
#' @description
#' Compute minimum variance portfolio subject to target return either allowing all assets to be sold
#' short or not allowing any asset to be sold short. The returned object is of class
#' \samp{portfolio}.
#' 
#' @details 
#' A mean-variance efficient portfolio \eqn{x} allowing short sales (negative weights) that achieves
#' the target expected return \eqn{\mu_0} solves the optimization problem: min \eqn{t(x)\Sigma x}
#' s.t. \eqn{t(x)1=1} and \eqn{t(x)\mu=\mu_0}, for which there is an analytic solution using matrix
#' algebra. If short sales are not allowed then the portfolio is computed numerically using the 
#' function \samp{solve.QP()} from the \samp{quadprog} package.
#' 
#' @param er \samp{N x 1} vector of expected returns
#' @param cov.mat \samp{N x N} return covariance matrix
#' @param target.return scalar, target expected return
#' @param shorts logical, if \samp{TRUE} then short sales (negative portfolio weights)
#' are allowed. If \samp{FALSE} then no asset is allowed to be sold short.
#' 
#' @return 
#'  \item{call}{captures function call}
#'  \item{er}{portfolio expected return}
#'  \item{sd}{portfolio standard deviation}
#'  \item{weights}{\samp{N x 1} vector of portfolio weights}
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
#' # compute efficient portfolio subject to target return
#' target.return = er["MSFT"]
#' e.port.msft = efficient.portfolio(er, covmat, target.return)
#' e.port.msft
#' summary(e.port.msft, risk.free=r.free)
#' plot(e.port.msft, col="blue")
#' 
#' # compute efficient portfolio subject to target return with no short sales
#' target.return = er["MSFT"]
#' e.port.msft.ns = efficient.portfolio(er, covmat, target.return, shorts=FALSE)
#' e.port.msft.ns
#' summary(e.port.msft.ns, risk.free=r.free)
#' plot(e.port.msft.ns, col="blue")
#' 
#' @export efficient.portfolio

efficient.portfolio <-
function(er, cov.mat, target.return, shorts=TRUE)
{
  call <- match.call()

  #
  # check for valid inputs
  #
  asset.names <- names(er)
  er <- as.vector(er) # assign names if none exist
  N <- length(er)
  cov.mat <- as.matrix(cov.mat)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semidefinite

  #
  # compute efficient portfolio
  #
  if(shorts==TRUE){
    ones <- rep(1, N)
    top <- cbind(2*cov.mat, er, ones)
    bot <- cbind(rbind(er, ones), matrix(0,2,2))
    A <- rbind(top, bot)
    b.target <- as.matrix(c(rep(0, N), target.return, 1))
    x <- solve(A, b.target)
    w <- x[1:N]
  } else if(shorts==FALSE){
    Dmat <- 2*cov.mat
    dvec <- rep.int(0, N)
    Amat <- cbind(rep(1,N), er, diag(1,N))
    bvec <- c(1, target.return, rep(0,N))
    result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=2)
    w <- round(result$solution, 6)
  } else {
    stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
  }

  #
  # compute portfolio expected returns and variance
  #
  names(w) <- asset.names
  er.port <- crossprod(er,w)
  sd.port <- sqrt(w %*% cov.mat %*% w)
  ans <- list("call" = call,
	      "er" = as.vector(er.port),
	      "sd" = as.vector(sd.port),
	      "weights" = w) 
  class(ans) <- "portfolio"
  return(ans)
}