#' @title Compute global minimum variance portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Compute global minimum variance portfolio given expected return vector and covariance matrix. The
#' portfolio can allow all assets to be shorted or not allow any assets to be shorted. The returned
#' object is of class \samp{portfolio}.
#' 
#' @details 
#' The global minimum variance portfolio \eqn{m} allowing for short sales solves the optimization
#' problem: min \eqn{t(m)\Sigma m} s.t. \eqn{t(m)1=1} for which there is an analytic solution using
#' matrix algebra. If short sales are not allowed then the portfolio is computed numerically using
#' the function \samp{solve.QP()} from the \samp{quadprog} package.
#' 
#' @param er \samp{N x 1} vector of expected returns
#' @param cov.mat \samp{N x N} return covariance matrix
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
#' # compute global minimum variance portfolio
#' gmin.port = globalMin.portfolio(er, covmat)
#' attributes(gmin.port)
#' print(gmin.port)
#' summary(gmin.port, risk.free=r.free)
#' plot(gmin.port, col="blue")
#' 
#' # compute global minimum variance portfolio with no short sales
#' gmin.port.ns = globalMin.portfolio(er, covmat, shorts=FALSE)
#' attributes(gmin.port.ns)
#' print(gmin.port.ns)
#' summary(gmin.port.ns, risk.free=r.free)
#' plot(gmin.port.ns, col="blue")
#' 
#' @export globalMin.portfolio

globalMin.portfolio <-
function(er, cov.mat, shorts=TRUE)
{
  call <- match.call()

  #
  # check for valid inputs
  #
  asset.names <- names(er)
  er <- as.vector(er) # assign names if none exist
  cov.mat <- as.matrix(cov.mat)
  N <- length(er)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semi-definite

  #
  # compute global minimum portfolio
  #
  if(shorts==TRUE){
    cov.mat.inv <- solve(cov.mat)
    one.vec <- rep(1,N)
    w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
    w.gmin <- as.vector(w.gmin)
  } else if(shorts==FALSE){
    Dmat <- 2*cov.mat
    dvec <- rep.int(0, N)
    Amat <- cbind(rep(1,N), diag(1,N))
    bvec <- c(1, rep(0,N))
    result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
    w.gmin <- round(result$solution, 6)
  } else {
    stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
  }
 
  names(w.gmin) <- asset.names
  er.gmin <- crossprod(w.gmin,er)
  sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
  gmin.port <- list("call" = call,
		    "er" = as.vector(er.gmin),
		    "sd" = as.vector(sd.gmin),
		    "weights" = w.gmin)
  class(gmin.port) <- "portfolio"
  gmin.port
}